#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/base
                     racket/contract))

@;{@title[#:tag "contract-boundaries"]{Contracts and Boundaries}}
@title[#:tag "contract-boundaries"]{合约和边界}

@;{Like a contract between two business partners, a software
contract is an agreement between two parties. The agreement
specifies obligations and guarantees for each ``product''
(or value) that is handed from one party to the other.}
如同两个商业伙伴之间的合同，软件合约是双方之间的协议。协议规定了从一方传给另一方的每一产品（或价值）的义务和保证。

@;{A contract thus establishes a boundary between the two parties. Whenever a
value crosses this boundary, the contract monitoring system performs contract
checks, making sure the partners abide by the established contract.}
因此，合约确定了双方之间的边界。当值跨越边界时，合约监督系统执行合约检查，确保合作伙伴遵守既定合约。

@;{In this spirit, Racket encourages contracts mainly at module
boundaries. Specifically, programmers may attach contracts to
@racket[provide] clauses and thus impose constraints and promises on the use
of exported values. For example, the export specification }
在这种精神下，Racket支持合约主要在模块边界。具体来说，程序员可以附加合约来@racket[提供（provide）]从句，从而对导出值的使用施加约束和承诺。例如，导出说明

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount ...)
]

@;{promises to all clients of the above module that the value of @racket[amount] will
always be a positive number. The contract system monitors
the module's obligation carefully. Every time a client
refers to @racket[amount], the monitor checks that the value
of @racket[amount] is indeed a positive number.}
承诺上述模块的所有客户，@racket[amount]值将始终是正数。合约系统仔细地监测了该模块的义务。每次客户提到@racket[amount]时，监视器都会检查@racket[amount]值是否确实是正数。

@;{The contracts library is built into the Racket language, but
if you wish to use @racket[racket/base], you can explicitly
require the contracts library like this:}
合约库是建立在Racket语言中内部的，但是如果你想使用@racket[racket/base]，你可以像这样明确地导入合约库：

@racketmod[
racket/base
(require racket/contract) (code:comment @;{"now we can write contracts"}"现在我们可以写合约了。")

(provide (contract-out [amount positive?]))

(define amount ...)
]

@ctc-section[#:tag "amount0"]{Contract Violations}
@ctc-section[#:tag "amount0"]{合同的违反}

@;{If we bind @racket[amount] to a number that is not positive,}
如果我们把@racket[amount]绑定到一个非正的数字上，

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount 0)]

@;{then, when the module is required, the monitoring
system signals a violation of the contract and
blames the module for breaking its promises.}
那么，当需要模块时，监控系统发出违反合同的信号，并指责模块违反了承诺。

@; @ctc-section[#:tag "qamount"]{A Subtle Contract Violation}

@;{An even bigger mistake would be to bind @racket[amount]
to a non-number value:}
一个更大的错误是将@racket[amount]绑定到非数字值上：

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount 'amount)
]

@;{In this case, the monitoring system will apply
@racket[positive?] to a symbol, but @racket[positive?]
reports an error, because its domain is only numbers. To
make the contract capture our intentions for all Racket
values, we can ensure that the value is both a number and is
positive, combining the two contracts with @racket[and/c]:}
在这种情况下，监控系统将应用@racket[positive?]到一个符号，但是@racket[positive?]报告错误，因为它的定义域是数字。为了使合约能取得我们对所有Racket值的意义，我们可以确保这一数值既是一个数字同时也是正的，将两份合约用@racket[and/c]结合：

@racketblock[
(provide (contract-out [amount (and/c number? positive?)]))
]

@;{

==================================================

The section below discusses assigning to variables that are
provide/contract'd. This is currently buggy so this
discussion is elided. Here's the expansion of
the requiring module, just to give an idea:

(module m racket
  (require mzlib/contract)
  (provide/contract [x x-ctc]))

(module n racket (require m) (define (f) ... x ...))
==>
(module n racket
  (require (rename m x x-real))
  (define x (apply-contract x-real x-ctc ...))
  (define (f) ... x ...))

The intention is to only do the work of applying the
contract once (per variable reference to a
provide/contract'd variable). This is a significant
practical savings for the contract checker (this
optimization is motivated by my use of contracts while I was
implementing one of the software construction projects
(scrabble, I think ...))

Of course, this breaks assignment to the provided variable.

==================================================

<question title="Example" tag="example">

<table src="simple.rkt">
<tr><td bgcolor="e0e0fa">
<racket>
;; Language: Pretty Big
(module a racket
  (require mzlib/contract)

  (provide/contract
   [amount positive?])

  (provide
   ;; -> Void
   ;; effect: sets variable a
   do-it)
  
  (define amount 4)
  
  (define (do-it) <font color="red">(set! amount -4)</font>))

(module b racket 
  (require a)
  
  (printf "~s\n" amount)
  <font color="red">(do-it)</font>
  (printf "~s\n" amount))

(require b)
</racket>
<td bgcolor="beige" valign="top">
<pre>

the "server" module 
this allows us to write contracts 

export @racket[amount] with a contract 


export @racket[do-it] without contract 



set amount to 4, 
  which satisfies contract


the "client" module 
requires functionality from a

first reference to @racket[amount] (okay)
a call to @racket[do-it], 
second reference to @racket[amount] (fail)

</pre> </table>

<p><strong>Note:</strong> The above example is mostly self-explanatory. Take a
look at the lines in red, however. Even though the call to @racket[do-it]
sets @racket[amount] to -4, this action is <strong>not</strong> a contract
violation. The contract violation takes place only when the client module
(@racket[b]) refers to @racket[amount] again and the value flows across
the module boundary for a second time. 

</question>
}

@;{@ctc-section{Experimenting with Contracts and Modules}}
@ctc-section[#:tag "Experimenting-with-Contracts-and-Modules"]{合约与模块的测试}

@;{All of the contracts and modules in this chapter (excluding those just
following) are written using the standard @tt{#lang} syntax for
describing modules. Since modules serve as the boundary between
parties in a contract, examples involve multiple modules.}
在这一章中的所有合同和模块（不包括那些只是跟随）是使用标准的@tt{#lang}语法描述的模块。由于模块是合约中各方之间的边界，所以示例涉及多个模块。

@;{To experiment with multiple modules within a single module or within
DrRacket's @tech{definitions area}, use
Racket's submodules. For example, try the example earlier in
this section like this:}
测试与多个模块在一个单一的模块或DrRacket的@tech{定义范围（definitions area）}内，使用Racket的子模块。例如，测试一下本节前面的示例，如下所示：

@racketmod[
racket

(module+ server
  (provide (contract-out [amount (and/c number? positive?)]))
  (define amount 150))
 
(module+ main
  (require (submod ".." server))
  (+ amount 10))
]

@;{Each of the modules and their contracts are wrapped in parentheses
with the @racket[module+] keyword at the front. The first form after
@racket[module] is the name of the module to be used in a subsequent
@racket[require] statement (where each reference through a
@racket[require] prefixes the name with @racket[".."]).}
每个模块及其合约都用前面的@racket[module+]关键字封装在圆括号中。 @racket[module]后面的第一个表是模块的名称，将在随后的@racket[require]语句中使用（其中每个引用都通过一个@racket[require]对名称用@racket[".."]进行前缀）。

@;{@ctc-section[#:tag "intro-nested"]{Experimenting with Nested Contract Boundaries}}
@ctc-section[#:tag "intro-nested"]{嵌套合约的测试}

@;{In many cases, it makes sense to attach contracts at module boundaries.
It is often convenient, however, to be able to use contracts at
a finer granularity than modules. The @racket[define/contract]
form enables this kind of use:}
在许多情况下，在模块边界上附加合约是有意义的。然而，能够以比模块更细致的方式使用合约通常是便利的。@racket[define/contract]表允许这种使用：

@racketmod[
racket

(define/contract amount
  (and/c number? positive?)
  150)

(+ amount 10)
]

@;{In this example, the @racket[define/contract] form establishes a contract
boundary between the definition of @racket[amount] and its surrounding
context. In other words, the two parties here are the definition and
the module that contains it.}
在这个例子中，@racket[define/contract]表在@racket[amount]定义和它周围的上下文之间建立了一个合约边界。换言之，这里的双方是包含它的定义和模块。

@;{Forms that create these @emph{nested contract boundaries} can sometimes
be subtle to use because they may have unexpected performance implications
or blame a party that may seem unintuitive. These subtleties are explained
in @secref["simple-nested"] and @ctc-link["gotcha-nested"].}
创造这些@emph{嵌套的合约边界（nested contract boundaries）}的表的使用有时是微妙的，因为他们可能会有意想不到的影响性能或指责似乎不直观的一方。这些微妙之处在@secref["simple-nested"]和@ctc-link["gotcha-nested"]中得到了解释。
