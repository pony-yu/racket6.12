#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define param-eval (make-base-eval))

@;{@title[#:tag "parameterize"]{Dynamic Binding: @racket[parameterize]}}
@title[#:tag "parameterize"]{动态绑定：@racket[parameterize]}

@;{@refalso["parameters"]{@racket[parameterize]}}
@margin-note{在《Racket参考》中的“（parameterize）”部分也有关于@racket[parameterize]的文档。}

@;{The @racket[parameterize] form associates a new value with a
@deftech{parameter} during the evaluation of @racket[_body]
expressions:}
@racket[parameterize]表把一个新值和@racket[_body]表达式的求值过程中的一个参数@deftech{parameter}相结合：

@specform[(parameterize ([parameter-expr value-expr] ...)
            body ...+)]

@margin-note{
 @;{The term ``parameter'' is sometimes used to refer to the
             arguments of a function, but ``parameter'' in Racket
             has the more specific meaning described here.}
   术语“参数”有时用于指一个函数的参数，但Racket中的“参数”在这里有更具体的意义描述。
   }

@;{For example, the @racket[error-print-width] parameter controls how
many characters of a value are printed in an error message:}
例如，@racket[error-print-width]参数控制在错误消息中打印一个值的字符数：

@interaction[
(parameterize ([error-print-width 5])
  (car (expt 10 1024)))
(parameterize ([error-print-width 10])
  (car (expt 10 1024)))
]

@;{More generally, parameters implement a kind of dynamic binding. The
@racket[make-parameter] function takes any value and returns a new
parameter that is initialized to the given value. Applying the
parameter as a function returns its current value:}
一般来说，参数实现了一种动态绑定。@racket[make-parameter]函数接受任何值并返回一个初始化为给定值的新参数。应用参数作为一个函数返回它的其当前值：

@interaction[
#:eval param-eval
(define location (make-parameter "here"))
(location)
]

@;{In a @racket[parameterize] form, each @racket[_parameter-expr] must
produce a parameter. During the evaluation of the @racket[body]s, each
specified parameter is given the result of the corresponding
@racket[_value-expr]. When control leaves the @racket[parameterize]
form---either through a normal return, an exception, or some other
escape---the parameter reverts to its earlier value:}
在一个@racket[parameterize]表里，每个@racket[_parameter-expr]必须产生一个参数。在对@racket[body]求值过程中，每一个指定的参数给出对应于@racket[_value-expr]的结果。当控制离开@racket[parameterize]表——无论是通过正常的返回，一个例外，或其它逃避——参数恢复到其先前的值：

@interaction[
#:eval param-eval
(parameterize ([location "there"])
  (location))
(location)
(parameterize ([location "in a house"])
  (list (location)
        (parameterize ([location "with a mouse"])
          (location))
        (location)))
(parameterize ([location "in a box"])
  (car (location)))
(location)
]

@;{The @racket[parameterize] form is not a binding form like
@racket[let]; each use of @racket[location] above refers directly to
the original definition. A @racket[parameterize] form adjusts the
value of a parameter during the whole time that the
@racket[parameterize] body is evaluated, even for uses of the
parameter that are textually outside of the @racket[parameterize]
body:}
@racket[parameterize]表不是一个像@racket[let]那样的绑定表；每次@racket[location]的使用向上都直接指向原来的定义。在@racket[parameterize]主体被求值的整个时间内@racket[parameterize]表调整参数的值，甚至是文本以外的@racket[parameterize]主体的参数使用：

@interaction[
#:eval param-eval
(define (would-you-could-you?)
  (and (not (equal? (location) "here"))
       (not (equal? (location) "there"))))

(would-you-could-you?)
(parameterize ([location "on a bus"])
  (would-you-could-you?))
]

@;{If a use of a parameter is textually inside the body of a
@racket[parameterize] but not evaluated before the
@racket[parameterize] form produces a value, then the use does not see
the value installed by the @racket[parameterize] form:}
如果参数的使用是在一个@racket[parameterize]主体内部进行的，但是在@racket[parameterize]表产生一个值之前没有被求值，那么这个用法看不到@racket[parameterize]表所安装的值：

@interaction[
#:eval param-eval
(let ([get (parameterize ([location "with a fox"])
             (lambda () (location)))])
  (get))
]

@;{The current binding of a parameter can be adjusted imperatively by
calling the parameter as a function with a value. If a
@racket[parameterize] has adjusted the value of the parameter, then
directly applying the parameter procedure affects only the value
associated with the active @racket[parameterize]:}
参数的当前绑定可以通过将该参数作为具有值的函数进行调用以进行必要的调整。 如果@racket[parameterize]已经调整了参数的值，那么直接应用参数过程只会影响与活动@racket[parameterize]相关的值：

@interaction[
#:eval param-eval
(define (try-again! where)
  (location where))

(location)
(parameterize ([location "on a train"])
  (list (location)
        (begin (try-again! "in a boat")
               (location))))
(location)
]

@;{Using @racket[parameterize] is generally preferable to updating a
parameter value imperatively---for much the same reasons that binding
a fresh variable with @racket[let] is preferable to using
@racket[set!]  (see @secref["set!"]).}
使用@racket[parameterize]通常更适合于更新参数值，这与使用@racket[let]绑定新变量的原因相同，最好使用@racket[set!] （见@secref["set!"]）。

@;{It may seem that variables and @racket[set!] can solve many of the
same problems that parameters solve. For example, @racket[lokation]
could be defined as a string, and @racket[set!] could be used
to adjust its value:}
似乎变量和@racket[set!]可以解决很多参数解决的相同问题。例如，@racket[lokation]可以被定义为一个字符串，以及@racket[set!]可以用来调整它的价值：

@interaction[
#:eval param-eval
(define lokation "here")

(define (would-ya-could-ya?)
  (and (not (equal? lokation "here"))
       (not (equal? lokation "there"))))

(set! lokation "on a bus")
(would-ya-could-ya?)
]

@;{Parameters, however, offer several crucial advantages over
@racket[set!]:}
然而，参数与@racket[set!]相比，参数提供了几个关键的优点：

@itemlist[

 @item{
  @;{The @racket[parameterize] form helps automatically reset the
       value of a parameter when control escapes due to an exception.
       Adding exception handlers and other forms to rewind a
       @racket[set!] is relatively tedious.}
@racket[parameterize]表有助于在正确避免异常时自动重置参数的值。 添加异常处理程序和其它表去实现转回一个@racket[set!]是比较繁琐的。
    }

 @item{
  @;{Parameters work nicely with tail calls (see
       @secref["tail-recursion"]). The last @racket[_body] in a
       @racket[parameterize] form is in @tech{tail position} with
       respect to the @racket[parameterize] form.}
参数可以和尾（tail）调用很好的一致（请参阅@secref["tail-recursion"]）。@racket[parameterize]表中的最后一个@racket[_body]相对于@racket[parameterize]表处于尾部位置。
    }

 @item{
  @;{Parameters work properly with threads (see
       @refsecref["threads"]). The @racket[parameterize] form adjusts
       the value of a parameter only for evaluation in the current
       thread, which avoids race conditions with other threads.}
    参数与线程正常工作（请参阅@refsecref["threads"]）。 @racket[parameterize]表仅调整当前线程中的参数值，以避免与其他线程竞争。
    }

]

@; ----------------------------------------

@close-eval[param-eval]
