#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "binding"]{Identifiers and Binding}}
@title[#:tag "binding"]{标识符和绑定}

@;{The context of an expression determines the meaning of identifiers
that appear in the expression. In particular, starting a module with
the language @racketmodname[racket], as in}
一个表达式的上下文决定表达式中出现的标识符的含义。特别是，用语言@racketmodname[racket]开始一个模块时，如：

@racketmod[racket]

@;{means that, within the module, the identifiers described in this guide
start with the meaning described here: @racket[cons] refers to the
function that creates a pair, @racket[car] refers to the function
that extracts the first element of a pair, and so on.}
意味着，在模块中，标识符在本指南中的描述开始于这里意义的描述：@racket[cons]引用创建了一个配对的函数，@racket[car]引用提取了一个配对的第一个元素的函数，等等。

@;{@guideother{@secref["symbols"] introduces the syntax of
identifiers.}}
@guideother{《@secref["symbols"]》介绍了标识符语法。}

@;{Forms like @racket[define], @racket[lambda], and @racket[let]
associate a meaning with one or more identifiers; that is, they
@defterm{bind} identifiers. The part of the program for which the
binding applies is the @defterm{scope} of the binding. The set of
bindings in effect for a given expression is the expression's
@defterm{environment}.}
诸如@racket[define]、@racket[lambda]和@racket[let]之类的表，用一个或多个标识符关联一个意义；也就是说，它们@defterm{绑定（bind）}标识符。绑定应用的程序部分是绑定的@defterm{范围（scope）}。对一个给定的表达式有效的绑定集是表达式的@defterm{环境（environment）}。

@;{For example, in}
例如，有以下内容：

@racketmod[
racket

(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))

(f 10)
]

@;{the @racket[define] is a binding of @racket[f], the @racket[lambda]
has a binding for @racket[x], and the @racket[let] has a binding for
@racket[y]. The scope of the binding for @racket[f] is the entire
module; the scope of the @racket[x] binding is @racket[(let ([y 5]) (+
x y))]; and the scope of the @racket[y] binding is just @racket[(+ x
y)]. The environment of @racket[(+ x y)] includes bindings for
@racket[y], @racket[x], and @racket[f], as well as everything in
@racketmodname[racket].}
@racket[define]是@racket[f]的绑定，@racket[lambda]有一个对@racket[x]的绑定，@racket[let]有一个对@racket[y]的绑定，对@racket[f]的绑定范围是整个模块；@racket[x]绑定的范围是@racket[(let ([y 5]) (+
x y))]；@racket[y]绑定的范围仅仅是@racket[(+ x
y)]的环境包括对@racket[y]、@racket[x]和@racket[f]的绑定，以及所有在@racketmodname[racket]中的绑定。

@;{A module-level @racket[define] can bind only identifiers that are not
already defined or @racket[require]d into the module. A local
@racket[define] or other binding forms, however, can give a new local
binding for an identifier that already has a binding; such a binding
@deftech{shadows} the existing binding.}
一个模块级的@racket[define]仅能够绑定没有被定义过或者@racket[require]进模块的标识符。然而，一个局部@racket[define]或其它绑定表，能够给一个已经有一个绑定的标志符以一个新的局部绑定；这样的一个绑定@deftech{覆盖（shadows）}已经存在的绑定。

@defexamples[
(define f
  (lambda (append)
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])
      (list append cons))))
(f list)
]

@;{Similarly, a module-level @racket[define] can @tech{shadow} a binding
from the module's language. For example, @racket[(define cons 1)] in a
@racketmodname[racket] module shadows the @racket[cons] that is
provided by @racketmodname[racket]. Intentionally shadowing a language
binding is rarely a good idea---especially for widely used bindings
like @racket[cons]---but shadowing relieves a programmer from having
to avoid every obscure binding that is provided by a language.}
类似地，一个模块级@racket[define]可以从这个模块的语言@tech{覆盖}一个绑定。例如，一个@racketmodname[racket]模块里的@racket[(define cons 1)]覆盖被@racketmodname[racket]提供的@racket[cons]。故意覆盖一个语言绑定绝对是一个好主意——尤其对于像@racket[cons]这种被广泛使用的绑定——但是覆盖把一个程序员从不得不去避免每一个晦涩的通过一个语言提供的绑定中解脱出来。

@;{Even identifiers like @racket[define] and @racket[lambda] get their
meanings from bindings, though they have @defterm{transformer}
bindings (which means that they indicate syntactic forms) instead of
value bindings. Since @racket[define] has a transformer binding, the
identifier @racketidfont{define} cannot be used by itself to get a
value. However, the normal binding for @racketidfont{define} can be
shadowed.}
即使像@racket[define]和@racket[lambda]这些从绑定中得到它们的意义，尽管它们有@defterm{转换器（transformer）}绑定（这意味着它们表明语法表）而不是值绑定。由于@racketidfont{define}有一个转换器绑定，这个标识符@racketidfont{define}不能被它自己使用于获取一个值。然而，对@racketidfont{define}的常规绑定可以被覆盖。

@examples[
define
(eval:alts (let ([@#,racketidfont{define} 5]) @#,racketidfont{define}) (let ([define 5]) define))
]

@;{Again, shadowing standard bindings in this way is rarely a good idea, but the
possibility is an inherent part of Racket's flexibility.}
同样，用这种方式来覆盖标准绑定绝对是一个好主意，但这种可能性是Racket的灵活性一个固有部分。
