#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "modules" #:style 'toc]{Modules}}
@title[#:tag "modules" #:style 'toc]{模块}

@;{Modules let you organize Racket code into multiple files and reusable
libraries.}
模块让你把Racket代码组织成多个文件和可重用的库。

@local-table-of-contents[]

@include-section["module-basics.scrbl"]
@include-section["module-syntax.scrbl"]
@include-section["module-paths.scrbl"]
@include-section["module-require.scrbl"]
@include-section["module-provide.scrbl"]
@include-section["module-set.scrbl"]
