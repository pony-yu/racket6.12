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
如同两个商业伙伴之间的一个合约，一个软件合约是双方之间的一个协议。这个协议规定了从一方传给另一方的每一”产品“（或值）的义务和保证。

@;{A contract thus establishes a boundary between the two parties. Whenever a
value crosses this boundary, the contract monitoring system performs contract
checks, making sure the partners abide by the established contract.}
因此，一个合约确定了双方之间的一个边界。每当一个值跨越这个边界，这个合约监督系统执行合约检查，确保合作伙伴遵守既定合约。

@;{In this spirit, Racket encourages contracts mainly at module
boundaries. Specifically, programmers may attach contracts to
@racket[provide] clauses and thus impose constraints and promises on the use
of exported values. For example, the export specification }
在这种精神下，Racket主要在模块边界支持合约。具体来说，程序员可以附加合约到@racket[provide]从句从而对输出值的使用施加约束和承诺。例如，输出描述

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
对上述@racket[amount]值的模块的所有客户端的承诺将始终是一个正数。合约系统仔细地监测了该模块的义务。每次一个客户端引用@racket[amount]时，监视器检查@racket[amount]值是否确实是一个正数。

@;{The contracts library is built into the Racket language, but
if you wish to use @racket[racket/base], you can explicitly
require the contracts library like this:}
合约库是建立在Racket语言中内部的，但是如果你希望使用@racket[racket/base]，你可以像这样明确地输入合约库：

@racketmod[
racket/base
(require racket/contract) (code:comment @;{"now we can write contracts"}"现在我们可以写合约了。")

(provide (contract-out [amount positive?]))

(define amount ...)
]

@ctc-section[#:tag "amount0"]{Contract Violations}
@ctc-section[#:tag "amount0"]{合约的违反}

@;{If we bind @racket[amount] to a number that is not positive,}
如果我们把@racket[amount]绑定到一个非正的数字上，

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount 0)]

@;{then, when the module is required, the monitoring
system signals a violation of the contract and
blames the module for breaking its promises.}
那么，当模块被需要时，监控系统发出一个合同违反的信号并将违背承诺归咎于这个模块。

@; @ctc-section[#:tag "qamount"]{A Subtle Contract Violation}

@;{An even bigger mistake would be to bind @racket[amount]
to a non-number value:}
一个更大的错误将是绑定@racket[amount]到一个非数字值上：

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
在这种情况下，监控系统将应用@racket[positive?]到一个符号，但是@racket[positive?]报告一个错误，因为它的定义域仅是数字。为了使合约能取得我们对所有Racket值的意图，我们可以确保这个数值既是一个数值同时也是正的，用@racket[and/c]结合两个合约：

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
在这一章中的所有合约和模块（不包括那些只是跟随的是使用描述模块的标准@tt{#lang}语法编写。由于模块充当一个合约中各方之间的边界，因此示例涉及多个模块。

@;{To experiment with multiple modules within a single module or within
DrRacket's @tech{definitions area}, use
Racket's submodules. For example, try the example earlier in
this section like this:}
为了在一个单一的模块内或者在DrRacket的@tech{定义范围（definitions area）}内用多个模块进行测试，使用Racket的子模块。例如，尝试如下所示本节中早先的示例：

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
每个模块及其合约都用前面的@racket[module+]关键字包裹在圆括号中。@racket[module]后面的第一个表是该模块的名称，它被用在一个随后的@racket[require]语句中（其中通过一个@racket[require]每个引用用@racket[".."]对名称进行前缀）。

@;{@ctc-section[#:tag "intro-nested"]{Experimenting with Nested Contract Boundaries}}
@ctc-section[#:tag "intro-nested"]{嵌套合约边界测试}

@;{In many cases, it makes sense to attach contracts at module boundaries.
It is often convenient, however, to be able to use contracts at
a finer granularity than modules. The @racket[define/contract]
form enables this kind of use:}
在许多情况下，在模块边界上附加合约是有意义的。然而，能够以一个比模块更细致的方式使用合约通常是方便的。这个@racket[define/contract]表提供这种使用的权利：

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
在这个例子中，@racket[define/contract]表确定在@racket[amount]的定义与其周边上下文之间的一个合约边界。换言之，这里的双方是这个定义及包含它的这个模块。

@;{Forms that create these @emph{nested contract boundaries} can sometimes
be subtle to use because they may have unexpected performance implications
or blame a party that may seem unintuitive. These subtleties are explained
in @secref["simple-nested"] and @ctc-link["gotcha-nested"].}
创造这些@emph{嵌套合约边界（nested contract boundaries）}的表有时对使用来说是微妙的，因为它们也许有意想不到的性能影响或归咎于似乎不直观的一方。这些微妙之处在《@secref["simple-nested"]》和《@ctc-link["gotcha-nested"]》中被讲解。
