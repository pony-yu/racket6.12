#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/contract))

@;{@title[#:tag "contracts-exists"]{Abstract Contracts using @racket[#:exists] and @racket[#:∃]}}
@title[#:tag "contracts-exists"]{用@racket[#:exists]和@racket[#:∃]抽象合约}

@;{The contract system provides existential contracts that can
protect abstractions, ensuring that clients of your module
cannot depend on the precise representation choices you make
for your data structures.}
合约系统提供了可以保护抽象的存在性合约，确保模块的客户机不能依赖于精确表达选择，以便你有利于你的数据结构。

@; @ctc-section{Getting Started, with a Queue Example}

@margin-note{
  @;{You can type @racket[#:exists] instead of @racket[#:∃] if you 
cannot easily type unicode characters; in DrRacket, typing
@litchar{\exists} followed by either alt-\ or control-\ (depending
on your platform) will produce @racket[∃].}
    如果你无法轻易输入Unicode字符你可以输入@racket[#:exists]代替@racket[#:∃]；在DrRacket里，输入@litchar{\exists}后跟着alt-\或control-（取决于你的平台）会生成@racket[∃]。
    }

@;{The @racket[contract-out] form allows you to write
@racketblock[#:∃ _name-of-a-new-contract] as one of its clauses. This declaration
introduces the variable @racket[_name-of-a-new-contract], binding it to a new
contract that hides information about the values it protects.}
@racket[contract-out]表允许你写作：@racketblock[#:∃ _name-of-a-new-contract]作为其从句之一。这个声明介绍变量@racket[_name-of-a-new-contract]，绑定到一个新的合约，隐藏关于它保护的值的信息。

@;{As an example, consider this (simple) implementation of a queue datastructure:}
作为一个例子，考虑这个（简单）一个队列数据结构的实现：

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
本代码实现了一个单纯的列表成员队列，这意味着数据结构的客户机可能对数据结构直接使用@racket[car]和@racket[cdr]（也许偶然地），从而在描述里的任何改变（用更有效的描述来说是支持分期常量时间入队和出队操作）可能会破坏客户机代码。

@;{To ensure that the queue representation is abstract, we can use @racket[#:∃] in the
@racket[contract-out] expression, like this:}
为确保队列的描述是抽象的，我们可以在@racket[contract-out]表达式里使用@racket[#:∃]，就像这样：

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
现在，如果数据结构的客户机尝试使用@racket[car]和@racket[cdr]，他们会收到一个错误，而不是去摆弄的队列的内部成员。

@;{See also @ctc-link["exists-gotcha"].}
也参见@ctc-link["exists-gotcha"]。
