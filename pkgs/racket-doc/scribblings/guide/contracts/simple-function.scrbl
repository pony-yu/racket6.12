#lang scribble/doc
@(require scribble/manual scribble/eval
          scribble/core racket/list 
          scribble/racket
          "../guide-utils.rkt" "utils.rkt"
          (for-label racket/contract))

@;{@title[#:tag "contract-func"]{Simple Contracts on Functions}}
@title[#:tag "contract-func"]{函数的简单合约}

@;{A mathematical function has a @deftech{domain} and a
@deftech{range}. The domain indicates the kind of values that the
function can accept as arguments, and the range indicates the kind of
values that it produces. The conventional notation for describing a
function with its domain and range is}
一个数学函数有一个@deftech{定义域(domain)}和一个@deftech{值域(range)}。定义域表示这个函数可以作为参数接受的值的类型，值域表示它生成的值的类型。用其定义域和值域描述一个函数的常规符号是

@racketblock[
f : A -> B
]

@;{where @racket[A] is the domain of the function and @racket[B] is the
range.}
这里@racket[A]是这个函数的定义域，@racket[B]是值域。

@;{Functions in a programming language have domains and ranges, too, and
a contract can ensure that a function receives only values in its
domain and produces only values in its range. A @racket[->] creates
such a contract for a function. The forms after a @racket[->] specify
contracts for the domains and finally a contract for the range.}
一个编程语言中的函数也有定义域和值域，而一个合约可以确保一个函数在其定义域中只接收值并且在其值域中只产生值。一个@racket[->]为一个函数创建这样的一个合约。一个@racket[->]之后的表为定义域指定定义域并且最后为值域指定一个合约。

@;{Here is a module that might represent a bank account:}
这里有一个可以代表一个银行帐户的模块：

@racketmod[
racket

(provide (contract-out
          [deposit (-> number? any)]
          [balance (-> number?)]))

(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (balance) amount)
]

@;{The module exports two functions: }
这个模块输出两个函数：

@itemize[

@item{
  @;{@racket[deposit], which accepts a number and returns some value
      that is not specified in the contract, and}
    @racket[deposit]，它接受一个数字并返回某个未在合约中指定的值，
    }

@item{
  @;{@racket[balance], which returns a number indicating the current
      balance of the account.}
    @racket[balance]，它返回指示账户当前余额的一个数值。
    }

]

@;{When a module exports a function, it establishes two channels of
communication between itself as a ``server'' and the ``client'' module
that imports the function. If the client module calls the function, it
sends a value into the server module. Conversely, if such a function
call ends and the function returns a value, the server module sends a
value back to the client module. This client--server distinction is
important, because when something goes wrong, one or the other of the
parties is to blame.}
当一个模块输出一个函数时，它在自己作为“服务器（server）”与“客户端（client）”的输入这个函数的模块之间建立两个通信通道。如果客户端模块调用该函数，它发送一个值进入服务器模块。相反，如果这样一个函数调用结束并且这个函数返回一个值，这个服务器模块发送一个值回到客户端模块。这种客户端-服务器区别是很重要的，因为当出现问题时，一方或另一方将被归咎。

@;{If a client module were to apply @racket[deposit] to @racket['millions],
it would violate the contract.  The contract-monitoring system would
catch this violation and blame the client for breaking the contract with
the above module. In contrast, if the @racket[balance] function were
to return @racket['broke], the contract-monitoring system
would blame the server module.}
如果一个客户端模块准备应用@racket[deposit]到@racket['millions]，这将违反其合约。合约监视系统会获得这个违规并因为与上述模块违背合约而归咎于这个客户端。相比之下，如果@racket[balance]函数准备返回@racket['broke]，合同监视系统将归咎于服务器模块。

@;{A @racket[->] by itself is not a contract; it is a @deftech{contract
combinator}, which combines other contracts to form a contract.}
一个@racket[->]本身不是一个合约；它是一种@deftech{合约组合（contract combinator）}，它结合其它合约以构成一个合约。

@; ------------------------------------------------------------------------

@;{@section{Styles of @racket[->]}}
@section{@racket[->]类型}

@;{If you are used to mathematical functions, you may prefer a contract
  arrow to appear between the domain and the range of a function, not
  at the beginning. If you have read @|HtDP|, you have seen this many
  times. Indeed, you may have seen contracts such as these in other
  people's code:}
如果你已经习惯了数学函数，你可以选择一个合约箭头出现在函数的定义域和值域之间而不是在开头。如果你已经阅读过《@|HtDP|》，那你已经见过这个很多次了。事实上，你也许已经在其他人的代码中看到比如这些合约：

@racketblock[
(provide (contract-out
          [deposit (number? . -> . any)]))
]

@;{If a Racket S-expression contains two dots with a symbol in the middle,
the reader re-arranges the S-expression and place the symbol at the front, 
as described in @secref["lists-and-syntax"].  Thus,}
如果一个Racket的S表达式包含在中间带一个符号的两个点，读取器重新安排这个S表达式并放置符号到前面，就如《@secref["lists-and-syntax"]》里描述的那样。因此，


@racketblock[
(number? . -> . any)
]

@;{is just another way of writing}
只是编写的另一种方式

@racketblock[
(-> number? any)
]

@; ------------------------------------------------------------------------

@;{@section[#:tag "simple-nested"]{Using @racket[define/contract] and @racket[->]}}
@section[#:tag "simple-nested"]{使用@racket[define/contract]和 @racket[->]}

@;{The @racket[define/contract] form introduced in @ctc-link["intro-nested"] can
also be used to define functions that come with a contract. For example,}
在《@ctc-link["intro-nested"]》中引入的@racket[define/contract]表也可以用来定义合约中的函数。例如,

@racketblock[
(define/contract (deposit amount)
  (-> number? any)
  (code:comment @;{"implementation goes here"}"实现在这里进行")
  ....)
]

@;{which defines the @racket[deposit] function with the contract from earlier.
Note that this has two potentially important impacts on the use of
@racket[deposit]:}
它用合约更早定义@racket[deposit]函数。请注意，这对@racket[deposit]的使用有两个潜在的重要影响：

@itemlist[#:style 'ordered
  @item{
           @;{Since the contract will always be checked on calls to @racket[deposit],
        even inside the module in which it is defined, this may increase
        the number of times the contract is checked. This could lead to
        a performance degradation. This is especially true if the function
        is called repeatedly in loops or using recursion.}
由于合约总是在调用@racket[deposit]时进行检查，即使在定义它的模块内，这也可能增加合约被检查的次数。这可能导致一个性能下降。如果函数在循环中反复调用或使用递归时尤其如此。
             }
  @item{
           @;{In some situations, a function may be written to accept a more
        lax set of inputs when called by other code in the same module.
        For such use cases, the contract boundary established by
        @racket[define/contract] is too strict.}
在某些情况下，当在同一模块中被其它代码调用时，一个函数可以编写来接受一组更宽松的输入。对于此类用例，通过@racket[define/contract]建立的合约边界过于严格。
             }
]

@; ----------------------------------------------------------------------
@;{@section{@racket[any] and @racket[any/c]}}
@section[#:tag "any-and-any|c"]{@racket[any]和@racket[any/c]}

@;{The @racket[any] contract used for @racket[deposit] matches any kind
of result, and it can only be used in the range position of a function
contract.  Instead of @racket[any] above, we could use the more
specific contract @racket[void?], which says that the function will
always return the @racket[(void)] value. The @racket[void?] contract,
however, would require the contract monitoring system to check the
return value every time the function is called, even though the
``client'' module can't do much with the value. In contrast,
@racket[any] tells the monitoring system @italic{not} to check the
return value, it tells a potential client that the ``server'' module
@italic{makes no promises at all} about the function's return value,
even whether it is a single value or multiple values.}
用于@racket[deposit]的@racket[any]合约匹配任何结果，并且它只能用于一个函数合约的值域位置。代替上面的@racket[any]，我们可以使用更具体的合约@racket[void?]，它表示函数总会返回@racket[(void)]值。然而，@racket[void?]合约会要求合约监视系统每次在函数被调用时去检查这个返回值，即使“客户端”模块不能很好用这个值工作。相反，@racket[any]告诉监视系统@italic{不}检查这个返回值，它告诉一个潜在客户端这个“服务器”模块对这个函数的返回值@italic{不作任何承诺}，甚至不管它是一个单独的值或多个值。

@;{The @racket[any/c] contract is similar to @racket[any], in that it
makes no demands on a value. Unlike @racket[any], @racket[any/c]
indicates a single value, and it is suitable for use as an argument
contract. Using @racket[any/c] as a range contract imposes a check
that the function produces a single value. That is,}
@racket[any/c]合约类似于@racket[any]，在那里它对一个值不做要求。不像@racket[any]，@racket[any/c]表示一个单个值，并且它适合用作一个参数合约。使用@racket[any/c]作为一个值域合约强迫一个对这个函数产生一个单个值的检查。就像这样，

@racketblock[(-> integer? any)]

@;{describes a function that accepts an integer and returns any number of
values, while}
描述一个接受一个整数并返回任意数值的函数，然而

@racketblock[(-> integer? any/c)]

@;{describes a function that accepts an integer and produces a single
result (but does not say anything more about the result). The function}
描述接受一个整数并生成一个单个结果（但对结果没有更多说明）的一个函数。以下函数

@racketblock[
(define (f x) (values (+ x 1) (- x 1)))
]

@;{matches @racket[(-> integer? any)], but not @racket[(-> integer? any/c)].}
匹配@racket[(-> integer? any)]，但不匹配@racket[(-> integer? any/c)]。

@;{Use @racket[any/c] as a result contract when it is particularly
important to promise a single result from a function. Use @racket[any]
when you want to promise as little as possible (and incur as little
checking as possible) for a function's result.}
当对承诺来自一个函数的一个单个结果特别重要时，使用@racket[any/c]作为一个结果合约。当你希望对一个函数的结果尽可能少地承诺（并尽可能少地检查）时，使用@racket[any/c]。

@; ------------------------------------------------------------------------

@;{@ctc-section[#:tag "own"]{Rolling Your Own Contracts}}
@ctc-section[#:tag "own"]{运转你自己的合约}

@;{The @racket[deposit] function adds the given number to the value of
@racket[amount]. While the function's contract prevents clients from
applying it to non-numbers, the contract still allows them to apply
the function to complex numbers, negative numbers, or inexact numbers,
none of which sensibly represent amounts of money.}
@racket[deposit]函数将给定的数值添加到@racket[amount]中。当该函数的合约阻止客户端将它应用到非数值时，这个合约仍然允许它们把这个函数应用到复数、负数或不精确的数字中，但没有一个能合理地表示钱的金额。

@;{The contract system allows programmers to define their own contracts
as functions:}
合约系统允许程序员定义他们自己的合约作为函数：

@racketmod[
racket
  
(define (amount? a)
  (and (number? a) (integer? a) (exact? a) (>= a 0)))

(provide (contract-out
          (code:comment @;{"an amount is a natural number of cents"}"一个amount是一个美分的自然数")
          (code:comment @;{"is the given number an amount?"}"是给定的数字amount?")
          [deposit (-> amount? any)]
          [amount? (-> any/c boolean?)]
          [balance (-> amount?)]))
  
(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (balance) amount)
]

@;{This module defines an @racket[amount?] function and uses it as a
contract within @racket[->] contracts. When a client calls the
@racket[deposit] function as exported with the contract @racket[(->
amount? any)], it must supply an exact, nonnegative integer, otherwise
the @racket[amount?] function applied to the argument will return
@racket[#f], which will cause the contract-monitoring system to blame
the client. Similarly, the server module must provide an exact,
nonnegative integer as the result of @racket[balance] to remain
blameless.}
这个模块定义了一个@racket[amount?]函数并在@racket[->]合约内使用它作为一个合约。当一个客户端用@racket[(->
amount? any)]调用@racket[deposit]函数作为输出时，它必须提供一个精确的、非负的整数，否则@racket[amount?]函数应用到参数将返回@racket[#f]，这将导致合约监视系统归咎于客户端。类似地，服务器模块必须提供一个精确的、非负的整数作为@racket[balance]的结果以保持无可归咎。

@;{Of course, it makes no sense to restrict a channel of communication to
values that the client doesn't understand. Therefore the module also
exports the @racket[amount?] predicate itself, with a contract saying
that it accepts an arbitrary value and returns a boolean.}
当然，将一个通信通道限制为客户端不明白的值是没有意义的。因此，这个模块也输出@racket[amount?]判断本身，用一个合约表示它接受一个任意值并返回一个布尔值。

@;{In this case, we could also have used @racket[natural-number/c] in
place of @racket[amount?], since it implies exactly the same check:}
在这种情况下，我们也可以使用@racket[natural-number/c]代替@racket[amount?]，因为它恰恰意味着相同的检查：

@racketblock[
(provide (contract-out
          [deposit (-> natural-number/c any)]
          [balance (-> natural-number/c)]))
]

@;{Every function that accepts one argument can be treated as a predicate
and thus used as a contract. For combining existing checks into a new
one, however, contract combinators such as @racket[and/c] and
@racket[or/c] are often useful. For example, here is yet another way
to write the contracts above:}
接受一个参数的每一个函数可以当作一个判断从而被用作一个合约。然而，为了结合现有的对一个新参数的检查，合约连接符像@racket[and/c]或@racket[or/c]往往是有用的。例如，这里还有另一种途径去编写上述合约：

@racketblock[
(define amount/c 
  (and/c number? integer? exact? (or/c positive? zero?)))

(provide (contract-out
          [deposit (-> amount/c any)]
          [balance (-> amount/c)]))
]

@;{Other values also serve double duty as contracts.  For example, if a
function accepts a number or @racket[#f], @racket[(or/c number?  #f)]
suffices. Similarly, the @racket[amount/c] contract could have been
written with a @racket[0] in place of @racket[zero?]. If you use a
regular expression as a contract, the contract accepts strings and
byte strings that match the regular expression.}
其它值也作为合约提供双重任务。例如，如果一个函数接受一个数值或@racket[#f]，@racket[(or/c number?  #f)]就够了。同样，@racket[amount/c]合约也许已经用一个@racket[0]代替@racket[zero?]来编写。如果你使用一个正则表达式作为一个合约，该合约接受与正则表达式匹配的字符串和字节字符串。

@;{Naturally, you can mix your own contract-implementing functions with
combinators like @racket[and/c]. Here is a module for creating strings
from banking records:}
当然，你可以用连接符像@racket[and/c]混合你自己的合约执行函数。这里有一个用于创建来自于银行记录的字符串的模块：

@racketmod[
racket

(define (has-decimal? str)
  (define L (string-length str))
  (and (>= L 3)
       (char=? #\. (string-ref str (- L 3)))))

(provide (contract-out
          (code:comment @;{"convert a random number to a string"}"转换一个随机数为一个字符串")
          [format-number (-> number? string?)]

          (code:comment @;{"convert an amount into a string with a decimal"}"用一个十进制数转换一个数字为一个字符串。")
          (code:comment @;{"point, as in an amount of US currency"}"点，如一个美国货币的数量")
          [format-nat (-> natural-number/c
                          (and/c string? has-decimal?))]))
]

@;{The contract of the exported function @racket[format-number] specifies
that the function consumes a number and produces a string. The
contract of the exported function @racket[format-nat] is more
interesting than the one of @racket[format-number].  It consumes only
natural numbers. Its range contract promises a string that has a
@litchar{.} in the third position from the right.}
输出函数@racket[format-number]的合约指定该函数接受一个数值并生成一个字符串。这个输出函数@racket[format-nat]的合约比@racket[format-number]的其中之一更有趣。它只接受自然数。它的值域合约承诺在右边的第三个位置带有一个@litchar{.}的字符串。

@;{If we want to strengthen the promise of the range contract for
@racket[format-nat] so that it admits only strings with digits and a single
dot, we could write it like this:}
如果我们希望加强@racket[format-nat]的值域合约的承诺，以便它只接受带数字和一个点的字符串，我们可以这样编写：

@racketmod[
racket

(define (digit-char? x) 
  (member x '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(define (has-decimal? str)
  (define L (string-length str))
  (and (>= L 3)
       (char=? #\. (string-ref str (- L 3)))))

(define (is-decimal-string? str)
  (define L (string-length str))
  (and (has-decimal? str)
       (andmap digit-char?
               (string->list (substring str 0 (- L 3))))
       (andmap digit-char?
               (string->list (substring str (- L 2) L)))))

....

(provide (contract-out
          ....
          (code:comment @;{"convert an  amount (natural number) of cents"}"转换美分的数量（自然数）")
          (code:comment @;{"into a dollar-based string"}"成为一个基于美元的字符串")
          [format-nat (-> natural-number/c 
                          (and/c string? 
                                 is-decimal-string?))]))
]

@;{Alternately, in this case, we could use a regular expression as a
contract:}
另外，在这种情况下，我们可以使用一个正则表达式作为一个合约：

@racketmod[
racket

(provide 
 (contract-out
  ....
  (code:comment @;{"convert an  amount (natural number) of cents"}"转换美分的一个数量（自然数）")
  (code:comment @;{"into a dollar-based string"}"成为一个基于美元的字符串")
  [format-nat (-> natural-number/c
                  (and/c string? #rx"[0-9]*\\.[0-9][0-9]"))]))
]

@; ------------------------------------------------------------------------

@;{@ctc-section{Contracts on Higher-order Functions}}
@ctc-section[#:tag "Contracts-on-Higher-order-Functions"]{高阶函数的合约}

@;{Function contracts are not just restricted to having simple
predicates on their domains or ranges. Any of the contract
combinators discussed here, including function contracts
themselves, can be used as contracts on the arguments and
results of a function.}
函数合约不仅仅局限于在其定义域或值域上的简单判断。还包括它们自己的函数合约，能够被用作参数及一个函数结果。

@;{For example, }
例如：

@racketblock[(-> integer? (-> integer? integer?))]

@;{is a contract that describes a curried function. It matches functions
that accept one argument and then return another function accepting a
second argument before finally returning an integer. If a server
exports a function @racket[make-adder] with this contract, and if
@racket[make-adder] returns a value other than a function, then the
server is to blame. If @racket[make-adder] does return a function, but
the resulting function is applied to a value other than an integer,
then the client is to blame.}
是描述一个柯里函数的一个合约。它匹配接受一个参数的函数并接着在返回另一个接受一个前面的第二个参数，最后返回一个整数。如果一个服务器用这个合约输出一个函数@racket[make-adder]，并且如果@racket[make-adder]返回一个函数外还返回一个值，那么这个服务器应被归咎。如果@racket[make-adder]确实返回一个函数，但这个返回函数被应用于一个整数外还有一个值，则客户端应被归咎。

@;{Similarly, the contract}
同样，合约

@racketblock[(-> (-> integer? integer?) integer?)]

@;{describes functions that accept other functions as its input. If a
server exports a function @racket[twice] with this contract and the
@racket[twice] is applied to a value other than a function of one
argument, then the client is to blame. If @racket[twice] is applied to
a function of one argument and @racket[twice] calls the given function
on a value other than an integer, then the server is to blame.}
描述接受其它函数作为其输入的函数。如果一个服务器用这个合约输出一个函数@racket[twice]并且@racket[twice]被应用给一个带一个参数的函数外还给一个值，那么客户端应被归咎。如果@racket[twice]被应用给一个带一个参数的函数并且@racket[twice]对一个整数调用这个给定的函数外还对一个值，那么服务器应被归咎。

@; ----------------------------------------------------------------------

@;{@ctc-section[#:tag "flat-named-contracts"]{Contract Messages with ``???''}}
@ctc-section[#:tag "flat-named-contracts"]{带”???“的合约信息}

@;{You wrote your module. You added contracts. You put them into the interface
so that client programmers have all the information from interfaces. It's a
piece of art: }
你编写了你的模块。你添加了合约。你将它们放入接口以便客户端程序员拥有来自接口的所有信息。这是一门艺术：

@interaction[#:eval 
             contract-eval
             (module bank-server racket
               (provide
                (contract-out
                 [deposit (-> (λ (x)
                                (and (number? x) (integer? x) (>= x 0)))
                              any)]))
               
               (define total 0)
               (define (deposit a) (set! total (+ a total))))]

@;{Several clients used your module. Others used their
modules in turn. And all of a sudden one of them sees this error
message:}
几个客户端使用了你的模块。其他人转而使用了他们的模块。突然他们中的一个看到了这个错误消息：

@interaction[#:eval 
             contract-eval
             (require 'bank-server)
             (deposit -10)]

@;{What is the @racketerror{???} doing there?  Wouldn't it be nice if
we had a name for this class of data much like we have string, number,
and so on?}
@racketerror{???}在那里代表什么？如果我们有这样一个数据类型的名字，就像我们有字符串、数字等等，那不是很好吗？

@;{For this situation, Racket provides @deftech{flat named
contracts}. The use of ``contract'' in this term shows that contracts
are first-class values. The ``flat'' means that the collection of data
is a subset of the built-in atomic classes of data; they are described
by a predicate that consumes all Racket values and produces a
boolean. The ``named'' part says what we want to do, which is to name
the contract so that error messages become intelligible:}
针对这种情况，Racket提供了@deftech{扁平命名合约（flat named contract）}。在这一术语中使用“合约”表明合约是第一类值。这个“扁平（flat）”意味着数据的集合是内建的数据原子类的一个子集；它们由一个接受所有Racket值并产生一个布尔值的判断来描述。这个“命名（named）”部分表示我们想要做的事情，它将去命名这个合约以便错误消息变得明白易懂：

@interaction[#:eval 
             contract-eval
             (module improved-bank-server racket
               (provide
                (contract-out
                 [deposit (-> (flat-named-contract
                               'amount
                               (λ (x)
                                 (and (number? x) (integer? x) (>= x 0))))
                              any)]))

               (define total 0)
               (define (deposit a) (set! total (+ a total))))]

@;{With this little change, the error message becomes quite readable:}
用这个小小的更改，这个错误消息就变得相当易读：

@interaction[#:eval 
             contract-eval
             (require 'improved-bank-server)
             (deposit -10)]

@; not sure why, but if I define str directly to be the
@; expression below, then it gets evaluated before the 
@; expressions above it.
@(define str "huh?")

@(begin
   (set! str
         (with-handlers ((exn:fail? exn-message))
           (contract-eval '(deposit -10))))
   "")

@;{@ctc-section[#:tag "dissecting-contract-errors"]{Dissecting a contract error message}}
@ctc-section[#:tag "dissecting-contract-errors"]{解析一个合约错误信息}

@(define (lines a b)
   (define lines (regexp-split #rx"\n" str))
   (table (style #f '())
          (map (λ (x) (list (paragraph error-color x)))
               (take (drop lines a) b))))

@;{In general, each contract error message consists of six sections:}
一般来说，每个合约错误信息由六部分组成：

@itemize[
 @item{
          @;{a name for the function or method associated with the contract
               and either the phrase ``contract violation'' or ``broke its contract''
               depending on whether the contract was violated by the client or the
               server; e.g. in the previous example: @lines[0 1]}
一个用合约关联的函数或方法的名称。而且这个短语“合约违反”或“违反合约”取决于是否这个合约被客户端或服务器违反；例如在前面的示例中：@lines[0 1]；
            }
          @item{
  @;{a description of the precise aspect of the contract that was violated, @lines[1 2]}
一个被违反的合约的准确方面的描述，@lines[1 2]；
    }
          @item{
  @;{the complete contract plus a path into it showing which aspect was violated, @lines[3 2]}
    这个完整的合约加上一个路径显示哪个方面被违反，@lines[3 2]；
    }
          @item{
  @;{the module where the contract was put (or, more generally, the boundary that the contract mediates), @lines[5 1]}
    合约被放置的这个模块（或者更广泛地说，合同所规定的边界），@lines[5 1]；
    }
          @item{
  @;{who was blamed, @lines[6 2]}
    哪个应被归咎，@lines[6 2]；
    }
          @item{
  @;{and the source location where the contract appears. @lines[8 1]}
    以及这个合约出现的源程序位置。 @lines[8 1]。
  }]
