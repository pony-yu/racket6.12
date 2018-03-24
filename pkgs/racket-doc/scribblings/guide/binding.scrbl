#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "binding"]{Identifiers and Binding}}
@title[#:tag "binding"]{标识符和绑定}

@;{The context of an expression determines the meaning of identifiers
that appear in the expression. In particular, starting a module with
the language @racketmodname[racket], as in}
表达式的上下文决定表达式中出现的标识符的含义。特别是，用语言@racketmodname[racket]开始一个模块时，如：

@racketmod[racket]

@;{means that, within the module, the identifiers described in this guide
start with the meaning described here: @racket[cons] refers to the
function that creates a pair, @racket[car] refers to the function
that extracts the first element of a pair, and so on.}
意味着，在模块中，本指南中描述的标识符从这里描述的含义开始：@racket[cons]指创建一个配对的函数，@racket[car]指的是提取一个配对的第一个元素的函数，等等。

@;{@guideother{@secref["symbols"] introduces the syntax of
identifiers.}}
@guideother{@secref["symbols"]介绍了标识符语法。}

@;{Forms like @racket[define], @racket[lambda], and @racket[let]
associate a meaning with one or more identifiers; that is, they
@defterm{bind} identifiers. The part of the program for which the
binding applies is the @defterm{scope} of the binding. The set of
bindings in effect for a given expression is the expression's
@defterm{environment}.}
诸如@racket[define]、@racket[lambda]和@racket[let]的表，并让一个意义与一个或多个标识符相关联，也就是说，它们@defterm{绑定（bind）}标识符。绑定应用的程序的一部分是绑定的@defterm{范围（scope）}。对给定表达式有效的绑定集是表达式的@defterm{环境（environment）}。

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
即使像@racket[define]和@racket[lambda]这些从绑定中得到它们的含义，尽管它们有@defterm{转换（transformer）}绑定（这意味着它们表示语法表）而不是值绑定。由于@racketidfont{define}具有一个转换绑定，因此标识符本身不能用于获取值。但是，对@racketidfont{define}的常规绑定可以被屏蔽。

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
类似地，模块级@racket[define]可以从模块的语言中@tech{屏蔽（shadow）}一个绑定。例如，一个@racketmodname[racket]模块里的@racket[(define cons 1)]屏蔽被@racketmodname[racket]所提供的@racket[cons]。有意屏蔽一个语言绑定是一个绝佳的主意——尤其是像@racket[cons]这种被广泛使用的绑定——但是屏蔽消除了程序员应该避免使用的语言提供的所有模糊绑定。

@;{Even identifiers like @racket[define] and @racket[lambda] get their
meanings from bindings, though they have @defterm{transformer}
bindings (which means that they indicate syntactic forms) instead of
value bindings. Since @racket[define] has a transformer binding, the
identifier @racketidfont{define} cannot be used by itself to get a
value. However, the normal binding for @racketidfont{define} can be
shadowed.}
即使像@racket[define]和@racket[lambda]这些从绑定中得到它们的含义，尽管它们有@defterm{转换（transformer）}绑定（这意味着它们表示语法表）而不是值绑定。由于@racketidfont{define}具有一个转换绑定，因此标识符本身不能用于获取值。但是，对@racketidfont{define}的常规绑定可以被屏蔽。

@examples[
define
(eval:alts (let ([@#,racketidfont{define} 5]) @#,racketidfont{define}) (let ([define 5]) define))
]

@;{Again, shadowing standard bindings in this way is rarely a good idea, but the
possibility is an inherent part of Racket's flexibility.}
同样，用这种方式来隐藏标准绑定是一个绝佳主意，但这种可能性是Racket灵活性的与生俱来的部分。
