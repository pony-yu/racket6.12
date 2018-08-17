#lang scribble/doc
@(require scribble/manual scribble/eval racket/sandbox
          "utils.rkt"
          (for-label racket/base racket/contract))

@;{@title[#:tag "contracts-gotchas"]{Gotchas}}
@title[#:tag "contracts-gotchas"]{问题}

@;{@ctc-section{Contracts and @racket[eq?]}}
@ctc-section[#:tag "Contracts-and-eq"]{合约和@racket[eq?]}

@;{As a general rule, adding a contract to a program should
either leave the behavior of the program unchanged, or
should signal a contract violation. And this is almost true
for Racket contracts, with one exception: @racket[eq?].}
作为一般规则，向程序中添加一个合约既应该使程序的行为保持不变，也应该标志出一个合约违反。并且这对于Racket合约几乎是真实的，只有一个例外：@racket[eq?]。

@;{The @racket[eq?] procedure is designed to be fast and does
not provide much in the way of guarantees, except that if it
returns true, it means that the two values behave
identically in all respects. Internally, this is implemented
as pointer equality at a low-level so it exposes information
about how Racket is implemented (and how contracts are
implemented). }
@racket[eq?]过程被设计为快速且不提供太多的确保方式，除非它返回true，这意味着这两个值在所有方面都是相同的。在内部，这被实现为在一个底层的指针相等，因此它揭示了有关Racket如何被实现的信息（以及合约如何被实现的信息）。

@;{Contracts interact poorly with @racket[eq?] because function
contract checking is implemented internally as wrapper
functions. For example, consider this module:}
用@racket[eq?]进行合约交互是糟糕的，因为函数合约检查被内部实现为包装器函数。例如，考虑这个模块：

@racketmod[
racket

(define (make-adder x)
  (if (= 1 x)
      add1
      (lambda (y) (+ x y))))
(provide (contract-out 
          [make-adder (-> number? (-> number? number?))]))
]

@;{It exports the @racket[make-adder] function that is the usual curried
addition function, except that it returns Racket's @racket[add1] when
its input is @racket[1].}
除当它的输入是@racket[1]时它返回Racket的@racket[add1]外，它输出通常被柯里化为附加函数的@racket[make-adder]函数。

@;{You might expect that}
你可能希望这样：

@racketblock[
(eq? (make-adder 1)
     (make-adder 1))
]

@;{would return @racket[#t], but it does not. If the contract were
changed to @racket[any/c] (or even @racket[(-> number? any/c)]), then
the @racket[eq?] call would return @racket[#t].}
应该返回@racket[#t]，但它却没有。如果该合约被改为@racket[any/c]（或者甚至是@racket[(-> number? any/c)]），那@racket[eq?]调用将返回@racket[#t]。

@;{Moral: Do not use @racket[eq?] on values that have contracts.}
教训：不要对有合约的值使用@racket[eq?]。

@;{@ctc-section[#:tag "gotcha-nested"]{Contract boundaries and @racket[define/contract]}}
@ctc-section[#:tag "gotcha-nested"]{合约边界和@racket[define/contract]}

@;{The contract boundaries established by @racket[define/contract], which
creates a nested contract boundary, are sometimes unintuitive. This is
especially true when multiple functions or other values with contracts
interact. For example, consider these two interacting functions:}
被@racket[define/contract]建立的合约边界，它创建了一个嵌套的合约边界，有时是不直观的。当多个函数或其它带有合约的值相互作用时尤其如此。例如，考虑这两个相互作用的函数：

@(define e2 (make-base-eval))
@(interaction-eval #:eval e2 (require racket/contract))
@interaction[#:eval e2
(define/contract (f x)
  (-> integer? integer?)
  x)
(define/contract (g)
  (-> string?)
  (f "not an integer"))
(g)
]

@;{One might expect that the function @racket[g] will be blamed
for breaking the terms of its contract with @racket[f]. 
Blaming @racket[g] would be right if @racket[f] and @racket[g]
were directly establishing contracts with each other.
They aren't, however. Instead, the access between @racket[f]
and @racket[g] is mediated through the top-level of the enclosing
module.}
人们可能期望这个函数@racket[g]将因为违反其带@racket[f]的合约条件而被归咎。如果@racket[f]和@racket[g]是直接建立合约的对方，归咎于@racket[g]就是对的。然而，它们不是。相反，@racket[f]和@racket[g]之间的访问是通过封闭模块的顶层被协调的。

@;{More precisely, @racket[f] and the top-level of the module have
the @racket[(-> integer? integer?)] contract mediating their
interaction; @racket[g] and the top-level have @racket[(-> string?)]
mediating their interaction, but there is no contract directly
between @racket[f] and @racket[g]. This means that the reference to
@racket[f] in the body of @racket[g] is really the top-level
of the module's responsibility, not @racket[g]'s. In other words,
the function @racket[f] has been given to @racket[g] with
no contract between @racket[g] and the top-level and thus
the top-level is blamed.}
更确切地说，@racket[f]和模块的顶层有@racket[(-> integer? integer?)]合约协调它们的相互作用，@racket[g]和顶层有@racket[(-> string?)]协调它们的相互作用，但是@racket[f]和@racket[g]之间没有直接的合约，这意味着在@racket[g]的主体内对@racket[f]的引用实际上是模块职责的顶层，而不是@racket[g]的。换句话说，函数@racket[f]已经被用在@racket[g]与顶层之间没有合约的方式赋予@racket[g]，因此顶层被归咎。

@;{If we wanted to add a contract between @racket[g] and the
top-level, we can use @racket[define/contract]'s
@racket[#:freevar] declaration and see the expected blame:}
如果我们想在@racket[g]和顶层之间增加一个合约，我们可以使用@racket[define/contract]的@racket[#:freevar]申明并看到预期的归咎：

@interaction[#:eval e2
(define/contract (f x)
  (-> integer? integer?)
  x)
(define/contract (g)
  (-> string?)
  #:freevar f (-> integer? integer?)
  (f "not an integer"))
(g)
]
@(close-eval e2)

@;{Moral: if two values with contracts should interact,
       put them in separate modules with contracts at
       the module boundary or use @racket[#:freevar].}
教训：如果带合约的两个值应相互作用，在模块边界上将它们放置在具有合约的分开的模块中或使用@racket[#:freevar]。

@;{@ctc-section[#:tag "exists-gotcha"]{Exists Contracts and Predicates}}
@ctc-section[#:tag "exists-gotcha"]{存在的合约和判断}

@;{Much like the @racket[eq?] example above, @racket[#:∃] contracts
can change the behavior of a program.}
很像上面的这个@racket[eq?]例子，@racket[#:∃]合约能够改变一个程序的行为。

@;{Specifically,
the @racket[null?] predicate (and many other predicates) return @racket[#f]
for @racket[#:∃] contracts, and changing one of those contracts to @racket[any/c]
means that @racket[null?] might now return @racket[#t] instead, resulting in
arbitrarily different behavior depending on how this boolean might flow around
in the program.}
具体来说，@racket[null?]判断（和许多其它判断）为@racket[#:∃]合约返回@racket[#f]，同时那些合同中的一个改变为@racket[any/c]意味着@racket[null?]现在可能反而返回@racket[#t]，任何不同行为的结果依赖于这个布尔值可以怎样在程序中流动。

@defmodulelang[racket/exists]

@;{To work around the above problem, the 
@racketmodname[racket/exists] library behaves just like @racketmodname[racket],
but predicates signal errors when given @racket[#:∃] contracts.}
要解决上述问题，@racketmodname[racket/exists]库行为就像@racketmodname[racket]，但当给定@racket[#:∃]合约时判断会发出错误信号。

@;{Moral: Do not use predicates on @racket[#:∃] contracts, but if you're not sure, use
@racketmodname[racket/exists] to be safe.}
教训：不要使用基于@racket[#:∃]合约的判断，但是如果你并不确定，用@racketmodname[racket/exists]在是安全的。

@;{@ctc-section{Defining Recursive Contracts}}
@ctc-section[#:tag "Defining-Recursive-Contracts"]{定义递归合约}

@;{When defining a self-referential contract, it is natural to use
@racket[define]. For example, one might try to write a contract on
streams like this:}
当定义一个自参考合约时，很自然地去使用@racket[define]。例如，人们可能试图在像这样的流上编写一个合约：

@(define e (make-base-eval))
@(interaction-eval #:eval e (require racket/contract))
@interaction[
  #:eval e
(define stream/c
  (promise/c
   (or/c null?
         (cons/c number? stream/c))))
]
@close-eval[e]

@;{Unfortunately, this does not work because the value of
@racket[stream/c] is needed before it is defined. Put another way, all
of the combinators evaluate their arguments eagerly, even though the
values that they accept do not.}
不幸的是，这不会工作，因为@racket[stream/c]的值在被定义之前就被需要。换句话说，所有的组合器都渴望对它们的参数求值，即使它们不接受这些值。

@;{Instead, use}
相反，使用

@racketblock[
(define stream/c
  (promise/c
   (or/c
    null?
    (cons/c number? (recursive-contract stream/c)))))
]

@;{The use of @racket[recursive-contract] delays the evaluation of the
identifier @racket[stream/c] until after the contract is first
checked, long enough to ensure that @racket[stream/c] is defined.}
@racket[recursive-contract]的使用延迟对标识符@racket[stream/c]的求值，直到合约被首先检查之后，足够长以确保@racket[stream/c]被定义。

@;{See also @ctc-link["lazy-contracts"].}
也参见《@ctc-link["lazy-contracts"]》。

@;{@ctc-section{Mixing @racket[set!] and @racket[contract-out]}}
@ctc-section{混合@racket[set!]和@racket[contract-out]}

@;{The contract library assumes that variables exported via
@racket[contract-out] are not assigned to, but does not enforce
it. Accordingly, if you try to @racket[set!] those variables, you 
may be surprised. Consider the following example:}
假定变量通过@racket[contract-out]输出的合约库没有被分配，但没有执行它。因此，如果你试图@racket[set!]这些变量，你可能会感到惊讶。考虑下面的例子：

@interaction[
(module server racket
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide (contract-out [inc-x! (-> void?)]
                         [x integer?])))

(module client racket
  (require 'server)

  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require 'client)
]

@;{Both calls to @racket[print-latest] print @racket[0], even though the
value of @racket[x] has been incremented (and the change is visible
inside the module @racket[x]).}
尽管@racket[x]的值已经被增加（并且在模块@racket[x]内可见），两个对@racket[print-latest]的调用打印@racket[0]。

@;{To work around this, export accessor functions, rather than
exporting the variable directly, like this:}
为了解决这个问题，输出访问器函数，而不是直接输出变量，像这样：

@racketmod[
racket

(define (get-x) x)
(define (inc-x!) (set! x (+ x 1)))
(define x 0)
(provide (contract-out [inc-x! (-> void?)]
                       [get-x (-> integer?)]))
]

@;{Moral: This is a bug that we will address in a future release.}
教训：这是一个我们将在一个以后版本中讨论的缺陷。