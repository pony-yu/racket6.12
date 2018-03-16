#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum racket/place))

@;{@title[#:tag "effective-places"]{Parallelism with Places}}
@title[#:tag "effective-places"]{现场（place）并行}

@;{The @racketmodname[racket/place] library provides support for
performance improvement through parallelism with the @racket[place]
form. The @racket[place] form creates a @deftech{place}, which is
effectively a new Racket instance that can run in parallel to other
places, including the initial place.  The full power of the Racket
language is available at each place, but places can communicate only
through message passing---using the @racket[place-channel-put] and
@racket[place-channel-get] functions on a limited set of
values---which helps ensure the safety and independence of parallel
computations.}
@racketmodname[racket/place]库通过与@racket[place]表的并行来提供性能改进的支持。@racket[place]表创造了一个@deftech{现场（place）}，这实际上是一个新的Racket实例，可以平行于其它现场，包括初始现场。在每一个现场都可以使用Racket语言的全部功能，但只能通过消息传递来传递现场——使用@racket[place-channel-put]和@racket[place-channel-get]函数在有限的值集上——这有助于确保并行计算的安全性和独立性。

@;{As a starting example, the racket program below uses a @tech{place} to
determine whether any number in the list has a double that is also in
the list:}
作为一个开始的例子，下面的racket程序使用一个@tech{现场（place）}来确定列表中的任何一个数是否有一个也在列表中的双数：

@codeblock{
#lang racket

(provide main)

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define (main)
  (define p 
    (place ch
      (define l (place-channel-get ch))
      (define l-double? (any-double? l))
      (place-channel-put ch l-double?)))

  (place-channel-put p (list 1 2 4 8))
  
  (place-channel-get p))
}

@;{The identifier @racket[ch] after @racket[place] is bound to a @deftech{place
channel}. The remaining body expressions within the @racket[place] form
are evaluated in a new place, and the body expressions use @racket[ch]
to communicate with the place that spawned the new place.}
@racket[place]后的标识符@racket[ch]绑定到 @deftech{现场通道（place
channel）}。在@racket[place]表中的剩余主体表达式在一个新的现场被求值，这个主体表达式使用@racket[ch]与产生新位置的位置来表达。

@;{In the body of the @racket[place] form above, the new place receives a
list of numbers over @racket[ch] and binds the list to @racket[l].  It
then calls @racket[any-double?] on the list and binds the result to
@racket[l-double?]. The final body expression sends the
@racket[l-double?] result back to the original place over @racket[ch].}
在上面的@racket[place]表的主体中，新的位置接收到一个超过@racket[ch]的数字列表，并将列表绑定到@racket[l]。它接着调用表上的@racket[any-double?]并且绑定这个结果到@racket[l-double?]。最终的主体表达式发送@racket[l-double?]结果越过@racket[ch]回到原来的现场。

@;{In DrRacket, after saving and running the above program, evaluate
@racket[(main)] in the interactions window to create the new
place. @margin-note*{When using @tech{places} inside DrRacket, the
module containg place code must be saved to a file before it will
execute.}  Alternatively, save the program as @filepath{double.rkt}
and run from a command line with}
在DrRacket里，保存并运行上面的程序后，在交互窗口对@racket[(main)]求值以创建新的现场。@margin-note*{当在DrRacket内使用@tech{现场（places）}，包含现场代码的模块在它被执行之前必须被保存到一个文件。}另外，作为@filepath{double.rkt}保存该程序并且用以下内容从一个命令行运行

@commandline{racket -tm double.rkt}

@;{where the @Flag{t} flag tells @exec{racket} to load the
@tt{double.rkt} module, the @Flag{m} flag calls the exported
@racket[main] function, and @Flag{tm} combines the two flags.}
在@Flag{t}标志告诉@exec{racket}加载@tt{double.rkt}模块的地方，@Flag{m}标志调用导出的@racket[main]函数，同时@Flag{tm}组合这两个标志。

@;{The @racket[place] form has two subtle features. First, it lifts the
@racket[place] body to an anonymous, module-level function.  This
lifting means that any binding referenced by the @racket[place] body
must be available in the module's top level. Second, the
@racket[place] form @racket[dynamic-require]s the enclosing module in
a newly created place. As part of the @racket[dynamic-require], the
current module body is evaluated in the new place.  The consequence of
this second feature is that @racket[place] should not appear immediately
in a module or in a function that is called in a module's top level;
otherwise, invoking the module will invoke the same module in a new
place, and so on, triggering a cascade of place creations that will
soon exhaust memory.}
@racket[place]表有两个微妙的特点。首先，它将@racket[place]主体提升为一个匿名的模块级的函数。这种提升意味着，@racket[place]主体引用的任何绑定都必须在模块的顶层级可用。第二，@racket[place]表@racket[dynamic-require]在新创建的现场中的封闭模块。作为@racket[dynamic-require]的一部分，当前模块主体将在新的现场被求值。第二个特性的后果是，该@racket[place]不应立即出现在一个模块中或在模块的顶层调用的函数中；否则，调用模块将在一个新的现场调用相同的模块，诸如此类，触发一系列将很快耗尽内存的现场创建。

@codeblock{
#lang racket

(provide main)

; Don't do this!
(define p (place ch (place-channel-get ch)))

(define (indirect-place-invocation)
  (define p2 (place ch (place-channel-get ch))))

; Don't do this, either!
(indirect-place-invocation)
}

