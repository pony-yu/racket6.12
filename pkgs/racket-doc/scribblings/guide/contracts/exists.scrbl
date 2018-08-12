#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/contract))

@;{@title[#:tag "contracts-exists"]{Abstract Contracts using @racket[#:exists] and @racket[#:∃]}}
@title[#:tag "contracts-exists"]{用@racket[#:exists]和@racket[#:∃]抽象合约}

@;{The contract system provides existential contracts that can
 protect abstractions, ensuring that clients of your module
 cannot depend on the precise representation choices you make
 for your data structures.}
合约系统提供可以保护抽象化的存在性合约，确保你的模块的客户端不能依赖于为你的数据结构所做的精确表示选择。

@; @ctc-section{Getting Started, with a Queue Example}

@margin-note{
 @;{You can type @racket[#:exists] instead of @racket[#:∃] if you 
  cannot easily type unicode characters; in DrRacket, typing
  @litchar{\exists} followed by either alt-\ or control-\ (depending
  on your platform) will produce @racket[∃].}
 如果你不能容易地键入Unicode字符，你可以键入@racket[#:exists]来代替@racket[#:∃]；在DrRacket里，键入@litchar{\exists}后跟着alt-\或control-（取决于你的平台）会生成@racket[∃]。
}

@;{The @racket[contract-out] form allows you to write
 @racketblock[#:∃ _name-of-a-new-contract] as one of its clauses. This declaration
 introduces the variable @racket[_name-of-a-new-contract], binding it to a new
 contract that hides information about the values it protects.}
@racket[contract-out]表允许你编写@racketblock[#:∃ _name-of-a-new-contract]作为其从句之一。这个声明引进这个变量@racket[_name-of-a-new-contract]，将它绑定到一个新的隐藏关于它保护的值的信息的合约。

@;{As an example, consider this (simple) implementation of a queue datastructure:}
作为一个例子，考虑这（简单的）一列数据结构的实现：

@racketmod[racket
           (define empty '())
           (define (enq top queue) (append queue (list top)))
           (define (next queue) (car queue))
           (define (deq queue) (cdr queue))
           (define (empty? queue) (null? queue))
           
           (provide
            (contract-out
             [empty (listof integer?)]
             [enq (-> integer? (listof integer?) (listof integer?))]
             [next (-> (listof integer?) integer?)]
             [deq (-> (listof integer?) (listof integer?))]
             [empty? (-> (listof integer?) boolean?)]))]

@;{This code implements a queue purely in terms of lists, meaning that clients
 of this data structure might use @racket[car] and @racket[cdr] directly on the
 data structure (perhaps accidentally) and thus any change in the representation
 (say to a more efficient representation that supports amortized constant time
 enqueue and dequeue operations) might break client code.}
此代码纯粹按照列表实现一个队列，这意味着数据结构的客户端可以对数据结构直接使用@racket[car]和@racket[cdr]（也许偶然地），从而在描述里的任何改变（例如，对于一个更有效表示，它支持摊销的固定时间队列和队列操作）可能会破坏客户机代码。

@;{To ensure that the queue representation is abstract, we can use @racket[#:∃] in the
 @racket[contract-out] expression, like this:}
为确保这个队列描述是抽象的，我们可以在@racket[contract-out]表达式里使用@racket[#:∃]，就像这样：

@racketblock[(provide
              (contract-out
               #:∃ queue
               [empty queue]
               [enq (-> integer? queue queue)]
               [next (-> queue integer?)]
               [deq (-> queue queue)]
               [empty? (-> queue boolean?)]))]

@;{Now, if clients of the data structure try to use @racket[car] and @racket[cdr], they
 receive an error, rather than mucking about with the internals of the queues.}
现在，如果数据结构的客户端尝试使用@racket[car]和@racket[cdr]，它们会收到一个错误，而不是用队列内部的东西来搞砸。

@;{See also @ctc-link["exists-gotcha"].}
也参见《@ctc-link["exists-gotcha"]》。
