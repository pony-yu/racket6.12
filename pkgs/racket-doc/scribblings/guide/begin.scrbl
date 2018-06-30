#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "begin"]{Sequencing}}
@title[#:tag "begin"]{定序}

@;{Racket programmers prefer to write programs with as few side-effects
as possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.}
Racket程序员喜欢编写尽可能少副作用的程序，因为纯粹的函数式代码更容易测试及组成更大的程序。然而，与外部环境的交互需要定序，例如写入一个显示器、打开一个图形窗口或在磁盘上操作一个文件时。

@;------------------------------------------------------------------------
@;{@section{Effects Before: @racket[begin]}}
@section[#:tag "Effects-Before-begin"]{前效应：@racket[begin]}

@;{@refalso["begin"]{@racket[begin]}}
@margin-note{在《Racket参考》的“（begin）”中也有关于@racket[begin]的文档。}

@;{A @racket[begin] expression sequences expressions:}
一个@racket[begin]表达式定序表达式：

@specform[(begin expr ...+)]{}

@;{The @racket[_expr]s are evaluated in order, and the result of all but
the last @racket[_expr] is ignored. The result from the last
@racket[_expr] is the result of the @racket[begin] form, and it is in
tail position with respect to the @racket[begin] form.}
@racket[_expr]被顺序求值，并且除最后的@racket[_expr]结果外所有结果都被忽略。来自最后的@racket[_expr]结果作为@racket[begin]表的结果，并且它是相对于@racket[begin]表来说位于尾部位置。

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
有多种表，比如@racket[lambda]或@racket[cond]支持一系列甚至没有一个@racket[begin]的表达式。这样的状态有时被叫做有一个@deftech{隐含的begin}。

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
@racket[begin]表在顶层（top level）、模块层（module level）或仅在内部定义之后作为一个@racket[body]是特定的。在这些位置，@racket[begin]的上下文被拼接到周围的上下文中，而不是形成一个表达式。

@defexamples[
(let ([curly 0])
  (begin
    (define moe (+ 1 curly))
    (define larry (+ 1 moe)))
  (list larry curly moe))
]

@;{This splicing behavior is mainly useful for macros, as we discuss
later in @secref["macros"].}
这种拼接行为主要用于宏，我们稍后在《@secref["macros"]》中讨论。

@;------------------------------------------------------------------------
@;{@section{Effects After: @racket[begin0]}}
@section[#:tag "Effects-After-begin0"]{后效应：@racket[begin0]}

@;{@refalso["begin"]{@racket[begin0]}}
@margin-note{在《Racket参考》的“（begin）”中也有关于@racket[begin0]的文档。}

@;{A @racket[begin0] expression has the same syntax as a @racket[begin]
expression:}
一个@racket[begin0]表达式具有与一个@racket[begin]表达式相同的语法：

@specform[(begin0 expr ...+)]{}

@;{The difference is that @racket[begin0] returns the result of the first
@racket[expr], instead of the result of the last @racket[expr]. The
@racket[begin0] form is useful for implementing side-effects that
happen after a computation, especially in the case where the
computation produces an unknown number of results.}
不同的是@racket[begin0]返回第一个@racket[expr]的结果，而不是最后的@racket[expr]结果。@racket[begin0]表对于实现发生在一个计算之后的副作用是有用的，尤其是在计算产生结果的一个未知数值的情况下。

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
@section[#:tag "when+unless"]{if效应：@racket[when]和@racket[unless]}

@;{@refalso["when+unless"]{@racket[when] and @racket[unless]}}
@margin-note{在《Racket参考》的“（when+unless）”部分也有关于@racket[when]和@racket[unless]的文档。}

@;{The @racket[when] form combines an @racket[if]-style conditional with
sequencing for the ``then'' clause and no ``else'' clause:}
@racket[when]表将一个@racket[if]样式条件与对“then”子句且无“else”子句的定序组合：

@specform[(when test-expr then-body ...+)]

@;{If @racket[_test-expr] produces a true value, then all of the
@racket[_then-body]s are evaluated. The result of the last
@racket[_then-body] is the result of the @racket[when] form.
Otherwise, no @racket[_then-body]s are evaluated and the
result is @|void-const|.}
如果@racket[_test-expr]产生一个真值，那么所有的@racket[_then-body]被求值。最后的@racket[_then-body]结果是@racket[when]表的结果。否则，没有@racket[_then-body]被求值而且结果是@|void-const|。

@;{The @racket[unless] form is similar:}
@racket[unless]是相似的：

@specform[(unless test-expr then-body ...+)]

@;{The difference is that the @racket[_test-expr] result is inverted: the
@racket[_then-body]s are evaluated only if the @racket[_test-expr]
result is @racket[#f].}
不同的是@racket[_test-expr]结果是相反的：如果@racket[_test-expr]结果为@racket[#f]，@racket[_then-body]被求值。

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
