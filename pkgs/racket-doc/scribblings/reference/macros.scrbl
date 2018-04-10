#lang scribble/doc
@(require "mz.rkt")

@;@title[#:style 'toc]{Macros}
@title[#:style 'toc]{宏}

@;{@guideintro["macros"]{Macros}}
@guideintro["macros"]{宏}

@;{See @secref["syntax-model"] for general information on how programs
are parsed. In particular, the subsection @secref["expand-steps"]
describes how parsing triggers macros, and
@secref["transformer-model"] describes how macro transformers are
called.}
有关程序如何解析的一般信息，请参见@secref["syntax-model"]。特别是，@secref["expand-steps"]小节描述了解析如何触发宏，而@secref["transformer-model"]描述了如何调用宏转换器。

@local-table-of-contents[]

@include-section["stx-patterns.scrbl"]
@include-section["stx-ops.scrbl"]
@include-section["stx-comp.scrbl"]
@include-section["stx-trans.scrbl"]
@include-section["stx-param.scrbl"]
@include-section["splicing.scrbl"]
@include-section["stx-props.scrbl"]
@include-section["stx-taints.scrbl"]
@include-section["stx-expand.scrbl"]
@include-section["include.scrbl"]
@include-section["syntax-util.scrbl"]
