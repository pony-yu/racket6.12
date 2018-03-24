#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "set!"]{Assignment: @racket[set!]}}
@title[#:tag "set!"]{赋值：@racket[set!]}

@refalso["set!"]{@racket[set!]}

@;{Assign to a variable using @racket[set!]:}
使用@racket[set!]赋值给变量：

@specform[(set! id expr)]

@;{A @racket[set!] expression evaluates @racket[_expr] and changes
@racket[_id] (which must be bound in the enclosing environment) to the
resulting value. The result of the @racket[set!]  expression itself is
@|void-const|.}
一个@racket[set!]表达式对@racket[_expr]求值并改变@racket[_id]（它必须限制在闭括号的环境内）为结果值。@racket[set!]表达式自己返回的结果是@|void-const|。

@defexamples[
(define greeted null)
(define (greet name)
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))

(greet "Athos")
(greet "Porthos")
(greet "Aramis")
greeted
]

@defs+int[
[(define (make-running-total)
   (let ([n 0])
     (lambda ()
       (set! n (+ n 1))
       n)))
 (define win (make-running-total))
 (define lose (make-running-total))]
(win)
(win)
(lose)
(win)
]

@;------------------------------------------------------------------------
@;{@section[#:tag "using-set!"]{Guidelines for Using Assignment}}
@section[#:tag "using-set!"]{使用赋值的指导原则}

@;{Although using @racket[set!] is sometimes appropriate, Racket style
generally discourages the use of @racket[set!]. The following
guidelines may help explain when using @racket[set!] is appropriate.}
虽然使用@racket[set!]有时是适当的，Racket风格通常建议不使用@racket[set!]。下面的准则有助于解释什么时候使用@racket[set!]是适当的。

@itemize[

 @item{
  @;{As in any modern language, assigning to a shared identifier is no
       substitute for passing an argument to a procedure or getting
       its result.}
    与任何现代语言一样，分配给共享标识符不是将参数传递给过程或获得结果的替代。

       @as-examples[@t{@bold{@;{@italic{Really awful} example:}@italic{事实上很糟糕的}例子：}}
       @defs+int[
       [(define name "unknown")
        (define result "unknown")
        (define (greet)
          (set! result (string-append "Hello, " name)))]
        (set! name "John")
        (greet)
        result
       ]]

      @as-examples[@t{@;{Ok example:}好的例子：}
      @def+int[
        (define (greet name)
          (string-append "Hello, " name))
        (greet "John")
        (greet "Anna")
      ]]}

@;-- FIXME: explain more _why_ it's inferior
 @item{
  @;{A sequence of assignments to a local variable is far inferior
       to nested bindings.}
    对局部变量的赋值序列远比嵌套绑定差。

       @as-examples[@t{@;{@bold{Bad} example:}@bold{差的}例子：}
       @interaction[
       (let ([tree 0])
         (set! tree (list tree 1 tree))
         (set! tree (list tree 2 tree))
         (set! tree (list tree 3 tree))
         tree)]]

       @as-examples[@t{@;{Ok example:}好的例子：}
       @interaction[
       (let* ([tree 0]
              [tree (list tree 1 tree)]
              [tree (list tree 2 tree)]
              [tree (list tree 3 tree)])
         tree)]]}

 @item{
  @;{Using assignment to accumulate results from an iteration is
       bad style. Accumulating through a loop argument is better.}
    使用赋值来从迭代中积累结果是不好的风格。通过循环参数积累更好。

       @as-examples[@t{@;{Somewhat bad example:}略差的示例：}
       @def+int[
       (define (sum lst)
         (let ([s 0])
           (for-each (lambda (i) (set! s (+ i s)))
                     lst)
           s))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{@;{Ok example:}好的示例：}
       @def+int[
       (define (sum lst)
         (let loop ([lst lst] [s 0])
           (if (null? lst)
               s
               (loop (cdr lst) (+ s (car lst))))))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{@;{Better (use an existing function) example:}更好（使用现有函数）示例：}
       @def+int[
       (define (sum lst)
         (apply + lst))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{@;{Good (a general approach) example:}好的（一般方法）例子：}
       @def+int[
       (define (sum lst)
         (for/fold ([s 0])
                   ([i (in-list lst)])
           (+ s i)))
       (sum '(1 2 3))
       ]]  }

 @item{For cases where stateful objects are necessary or appropriate,
       then implementing the object's state with @racket[set!] is
       fine.

       @as-examples[@t{Ok example:}
       @def+int[
       (define next-number!
         (let ([n 0])
           (lambda ()
             (set! n (add1 n))
             n)))
       (next-number!)
       (next-number!)
       (next-number!)]]}

]

@;{All else being equal, a program that uses no assignments or mutation
is always preferable to one that uses assignments or mutation. While
side effects are to be avoided, however, they should be used if the
resulting code is significantly more readable or if it implements a
significantly better algorithm."}
所有其它的情况都相同，则不使用赋值或变化的程序总是优于使用赋值或变化的程序。虽然应该避免副作用，但如果结果代码可读性更高，或者实现了更好的算法，则应该使用这些副作用。

@;{The use of mutable values, such as vectors and hash tables, raises
fewer suspicions about the style of a program than using @racket[set!]
directly. Nevertheless, simply replacing @racket[set!]s in a program
with @racket[vector-set!]s obviously does not improve the style of
the program.}
可变值的使用，如向量和哈希表，对程序风格比直接使用@racket[set!]提出了较少怀疑。不过，在一个用@racket[vector-set!]的程序中简单更换@racket[set!]显然没有改善程序风格。

@;------------------------------------------------------------------------
@;{@section{Multiple Values: @racket[set!-values]}}
@section{多值赋值：@racket[set!-values]}

@refalso["set!"]{@racket[set!-values]}

@;{The @racket[set!-values] form assigns to multiple variables at once,
given an expression that produces an appropriate number of values:}
@racket[set!-values]表一次赋值给多个变量，给出一个生成适当的数值的表达式：

@specform[(set!-values (id ...) expr)]

@;{This form is equivalent to using @racket[let-values] to receive
multiple results from @racket[_expr], and then assigning the results
individually to the @racket[_id]s using @racket[set!].}
这个表等价于使用@racket[let-values]从@racket[_expr]接收多个结果，然后将结果使用@racket[set!]单独赋值给@racket[_id]。

@defexamples[
(define game
  (let ([w 0]
        [l 0])
    (lambda (win?)
      (if win?
          (set! w (+ w 1))
          (set! l (+ l 1)))
      (begin0
        (values w l)
        (code:comment @#,t{swap sides...})
        (set!-values (w l) (values l w))))))
(game #t)
(game #t)
(game #f)]
