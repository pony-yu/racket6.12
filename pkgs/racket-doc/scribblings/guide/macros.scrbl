#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "macros" #:style 'toc]{Macros}}
@title[#:tag "macros" #:style 'toc]{宏}

@;{A @deftech{macro} is a syntactic form with an associated
@deftech{transformer} that @deftech{expands} the original form
into existing forms. To put it another way, a macro is an
extension to the Racket compiler. Most of the syntactic forms of
@racketmodname[racket/base] and @racketmodname[racket] are
actually macros that expand into a small set of core constructs.}
一个@deftech{宏（macro）}是一种语法表，它有一个关联的@deftech{转换器（transformer）}，它将原有的表@deftech{扩展（expand）}为现有的表。换句话说，宏是Racket编译器的扩展。@racketmodname[racket/base]和@racketmodname[racket]的大部分句法表实际上是宏，扩展成一小部分核心结构。

@;{Like many languages, Racket provides pattern-based macros that
make simple transformations easy to implement and reliable to
use. Racket also supports arbitrary macro transformers that are
implemented in Racket---or in a macro-extended variant of Racket.}
像许多语言一样，Racket提供基于模式的宏，使得简单的转换易于实现和可靠使用。Racket还支持任意的宏转换器，它在Racket中实现，或在Racket中的宏扩展变体中实现。

@;{(For a bottom-up introduction of Racket macro, you may refer to: @(hyperlink "http://www.greghendershott.com/fear-of-macros/" "Fear of Macros"))}
（对于自下而上的Racket宏的介绍，你可以参考：@(hyperlink "http://www.greghendershott.com/fear-of-macros/" "宏的担忧")）

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["pattern-macros.scrbl"]
@include-section["proc-macros.scrbl"]

