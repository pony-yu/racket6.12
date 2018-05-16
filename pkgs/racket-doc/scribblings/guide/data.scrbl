#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "datatypes" #:style 'toc]{Built-In Datatypes}}
@title[#:tag "datatypes" #:style 'toc]{内置的数据类型}

@;{The @seclink["to-scheme"]{previous chapter} introduced some of
Racket's built-in datatypes: numbers, booleans, strings, lists, and
procedures. This section provides a more complete coverage of the
built-in datatypes for simple forms of data.}
@seclink["to-scheme"]{上一章}介绍了一些Racket的内建数据类型：数字、布尔值、字符串、列表、和程序。本节为数据的简单表提供一个内建数据类型的更完整的覆盖。

@local-table-of-contents[]

@include-section["booleans.scrbl"]
@include-section["numbers.scrbl"]
@include-section["chars.scrbl"]
@include-section["char-strings.scrbl"]
@include-section["byte-strings.scrbl"]
@include-section["symbols.scrbl"]
@include-section["keywords.scrbl"]
@include-section["pairs.scrbl"]
@include-section["vectors.scrbl"]
@include-section["hash-tables.scrbl"]
@include-section["boxes.scrbl"]
@include-section["void-and-undef.scrbl"]

@; @include-section["paths.scrbl"]
@; @include-section["regexps-data.scrbl"]
