#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "to-scheme" #:style 'toc]{Racket Essentials}}
@title[#:tag "to-scheme" #:style 'toc]{Racket概要}

@;{This chapter provides a quick introduction to Racket as background for
the rest of the guide. Readers with some Racket experience can safely
skip to @secref["datatypes"].}
本章提供了一个对Racket的快速入门作为给这个指南余下部分的背景。有一些Racket经验的读者可以直接跳到《@secref["datatypes"]》部分。

@local-table-of-contents[]

@include-section["simple-data.scrbl"]
@include-section["simple-syntax.scrbl"]
@include-section["lists.scrbl"]
@include-section["truth.scrbl"]
