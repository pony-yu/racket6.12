#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@;@title[#:tag "languages" #:style 'toc]{Creating Languages}
@title[#:tag "languages" #:style 'toc]{创造语言}

@;{The @tech{macro} facilities defined in the preceding chapter let a
programmer define syntactic extensions to a language, but a macro is
limited in two ways:}
前一章中定义的@tech{宏（macro）}工具让一个程序员定义语言的语法扩展，但一个宏在以下两个方面是有限的：

@itemlist[

 @;{@item{a macro cannot restrict the syntax available in its context or
       change the meaning of surrounding forms; and}}
   @item{一个宏不能限制上下文中可用的语法或改变包围的表的意义；}

 @;{@item{a macro can extend the syntax of a language only within the
       parameters of the language's lexical conventions, such as using
       parentheses to group the macro name with its subforms and using
       the core syntax of identifiers, keywords, and literals.}}
@item{一个宏仅在语言词汇约定的参数范围内能够扩展语言中的语法，如用括号对带子表的宏名分组和用标识符、关键字和原意的核心语法。}

]

@;{@guideother{The distinction between the @tech{reader} and
@tech{expander} layer is introduced in @secref["lists-and-syntax"].}}
@guideother{读取器层和扩展器层之间的区别在《@secref [”lists-and-syntax”]》中介绍。}

@;{That is, a macro can only extend a language, and it can do so only at
the @tech{expander} layer. Racket offers additional facilities for
defining a starting point of the @tech{expander} layer, for extending
the @tech{reader} layer, for defining the starting point of the
@tech{reader} layer, and for packaging a @tech{reader} and
@tech{expander} starting point into a conveniently named language.}
也就是说，一个宏只能扩展一种语言，它只能在 @tech{扩展器（expander）}层起作用。Racket为定义@tech{扩展器（expander）}层的起始点提供额外的工具，以便扩展@tech{读取器（reader）}层，以便定义@tech{读取器（reader）}层的起始点，也以便封装一个@tech{读取器（reader）}和@tech{扩展器（expander）} 起始点为一个方便命名的语言。

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["module-languages.scrbl"]
@include-section["reader-extension.scrbl"]
@include-section["hash-languages.scrbl"]
