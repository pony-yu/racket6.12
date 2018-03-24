#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "begin"]{Sequencing}}
@title[#:tag "begin"]{排序}

@;{Racket programmers prefer to write programs with as few side-effects
as possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.}
Racket程序员喜欢编写尽可能少的带副作用的程序，因为纯粹的函数代码更容易测试和组成更大的程序。然而，与外部环境的交互需要进行排序，例如在向显示器写入、打开图形窗口或在磁盘上操作文件时。

@;------------------------------------------------------------------------
@;{@section{Effects Before: @racket[begin]}}
@section[#:tag "Effects-Before-begin"]{前置影响：@racket[begin]}

@;{@refalso["begin"]{@racket[begin]}}
@refalso["begin"]{@racket[begin]}

@;{A @racket[begin] expression sequences expressions:}
一个@racket[begin]表达式排序表达式：

@specform[(begin expr ...+)]{}

@;{The @racket[_expr]s are evaluated in order, and the result of all but
the last @racket[_expr] is ignored. The result from the last
@racket[_expr] is the result of the @racket[begin] form, and it is in
tail position with respect to the @racket[begin] form.}
@racket[_expr]被顺序求值，并且除最后的@racket[_expr]结果外所有都被忽视。来自最后一个@racket[_expr]的结果作为@racket[begin]表的结果，它是相对于@racket[begin]表位于尾部的位置。

@defexamples[
(define (print-triangle height)
  (if (zero? height)
      (void)
      (begin
        (display (make-string height #\*))
        (newline)
        (print-triangle (sub1 height)))))
(print-triangle 4)
]

@;{Many forms, such as @racket[lambda] or @racket[cond] support a
sequence of expressions even without a @racket[begin]. Such positions are
sometimes said to have an @deftech{implicit begin}.}
有多种表，比如@racket[lambda]或@racket[cond]支持一系列表达式甚至没有一个@racket[begin]。这样的状态有时被叫做有一个隐含的@deftech{implicit begin}。

@defexamples[
(define (print-triangle height)
  (cond
    [(positive? height)
     (display (make-string height #\*))
     (newline)
     (print-triangle (sub1 height))]))
(print-triangle 4)
]

@;{The @racket[begin] form is special at the top level, at module level,
or as a @racket[body] after only internal definitions. In those
positions, instead of forming an expression, the content of
@racket[begin] is spliced into the surrounding context.}
@racket[begin]表在顶层（top level）、模块级（module level）或仅在内部定义之后作为@racket[body]是特殊的。在这些状态下，@racket[begin]的上下文被拼接到周围的上下文中，而不是形成一个表达式。

@defexamples[
(let ([curly 0])
  (begin
    (define moe (+ 1 curly))
    (define larry (+ 1 moe)))
  (list larry curly moe))
]

@;{This splicing behavior is mainly useful for macros, as we discuss
later in @secref["macros"].}
这种拼接行为主要用于宏（macro），我们稍后将在@secref["macros"]中讨论它。

@;------------------------------------------------------------------------
@;{@section{Effects After: @racket[begin0]}}
@section[#:tag "Effects-After-begin0"]{后置影响：@racket[begin0]}

@refalso["begin"]{@racket[begin0]}

@;{A @racket[begin0] expression has the same syntax as a @racket[begin]
expression:}
一个@racket[begin0]表达式与有一个@racket[begin]表达式有相同的语法：

@specform[(begin0 expr ...+)]{}

@;{The difference is that @racket[begin0] returns the result of the first
@racket[expr], instead of the result of the last @racket[expr]. The
@racket[begin0] form is useful for implementing side-effects that
happen after a computation, especially in the case where the
computation produces an unknown number of results.}
不同的是@racket[begin0]返回第一个@racket[expr]的结果，而不是最后一个@racket[expr]的结果。@racket[begin0]表对于实现发生在一个计算之后的副作用是有用的，尤其是在计算产生了一个未知的数值结果的情况下。

@defexamples[
(define (log-times thunk)
  (printf "Start: ~s\n" (current-inexact-milliseconds))
  (begin0
    (thunk)
    (printf "End..: ~s\n" (current-inexact-milliseconds))))
(log-times (lambda () (sleep 0.1) 0))
(log-times (lambda () (values 1 2)))
]

@;------------------------------------------------------------------------
@;{@section[#:tag "when+unless"]{Effects If...: @racket[when] and @racket[unless]}}
@section[#:tag "when+unless"]{if影响：@racket[when]和@racket[unless]}

@;{@refalso["when+unless"]{@racket[when] and @racket[unless]}}
@refalso["when+unless"]{@racket[when]和@racket[unless]}

@;{The @racket[when] form combines an @racket[if]-style conditional with
sequencing for the ``then'' clause and no ``else'' clause:}
@racket[when]表将@racket[if]样式条件与“then”子句并且没有“else”子句的排序相结合：

@specform[(when test-expr then-body ...+)]

@;{If @racket[_test-expr] produces a true value, then all of the
@racket[_then-body]s are evaluated. The result of the last
@racket[_then-body] is the result of the @racket[when] form.
Otherwise, no @racket[_then-body]s are evaluated and the
result is @|void-const|.}
如果@racket[_test-expr]产生一个真值，那么所有的@racket[_then-body]被求值。最后一个@racket[_then-body]的结果是@racket[when]表的结果。否则，没有@racket[_then-body]被求值而且结果是@|void-const|。

@;{The @racket[unless] form is similar:}
@racket[unless]是相似的：

@specform[(unless test-expr then-body ...+)]

@;{The difference is that the @racket[_test-expr] result is inverted: the
@racket[_then-body]s are evaluated only if the @racket[_test-expr]
result is @racket[#f].}
不同的是，@racket[_test-expr]结果是相反的：如果@racket[_test-expr]结果为@racket[#f]时@racket[_then-body]被求值。

@defexamples[
(define (enumerate lst)
  (if (null? (cdr lst))
      (printf "~a.\n" (car lst))
      (begin
        (printf "~a, " (car lst))
        (when (null? (cdr (cdr lst)))
          (printf "and "))
        (enumerate (cdr lst)))))
(enumerate '("Larry" "Curly" "Moe"))
]

@def+int[
(define (print-triangle height)
  (unless (zero? height)
    (display (make-string height #\*))
    (newline)
    (print-triangle (sub1 height))))
(print-triangle 4)
]
