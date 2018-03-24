#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "scheme-forms" #:style 'toc]{Expressions and Definitions}}
@title[#:tag "scheme-forms" #:style 'toc]{表达式和定义}

@;{The @secref["to-scheme"] chapter introduced some of Racket's syntactic
forms: definitions, procedure applications, conditionals, and so
on. This section provides more details on those forms, plus a few
additional basic forms.}
@secref["to-scheme"]这一章介绍了一些基本的Racket的句法形式：定义、程序、条件表达式等。本节提供这些形式的更详细的信息，以及一些附加的基本形式。

@local-table-of-contents[]

@;{@section[#:tag "syntax-notation"]{Notation}}
@section[#:tag "syntax-notation"]{标记法}

@;{This chapter (and the rest of the documentation) uses a slightly
different notation than the character-based grammars of the
@secref["to-scheme"] chapter. The grammar for a use of a syntactic
form @racketkeywordfont{something} is shown like this:}
这一章（和其余的文档）使用了一个稍微不同的标记法，而不是基于字符的@secref["to-scheme"]章节语法。使用语法表@racketkeywordfont{something}的语法如下所示：

@specform[(#,(racketkeywordfont "something") [id ...+] an-expr ...)]

@;{The italicized meta-variables in this specification, such as
@racket[_id] and @racket[_an-expr], use the syntax of Racket
identifiers, so @racket[_an-expr] is one meta-variable. A naming
convention implicitly defines the meaning of many meta-variables:}
斜体的元变量在本规范中，如@racket[_id]和@racket[_an-expr]，使用Racket标识符的语法，所以@racket[_an-expr]是一元变量。命名约定隐式定义了许多元变量的含义：

@itemize[

 @item{
  @;{A meta-variable that ends in @racket[_id] stands for an
       identifier, such as @racketidfont{x} or
       @racketidfont{my-favorite-martian}.}
    以@racket[_id]结尾的一个元变量表示标识符，如@racketidfont{x}或@racketidfont{my-favorite-martian}。
    }

 @item{
  @;{A meta-identifier that ends in @racket[_keyword] stands
       for a keyword, such as @racket[#:tag].}
    一个一元标识符以@racket[_keyword]结束代表一个关键字，如@racket[#:tag]。
    }

 @item{
  @;{A meta-identifier that ends with @racket[_expr] stands for any
       sub-form, and it will be parsed as an expression.}
    一元标识符以@racket[_expr]结束表达代表任何子表，它将被解析为一个表达式。
    }

 @item{
  @;{A meta-identifier that ends with @racket[_body] stands for any
       sub-form; it will be parsed as either a local definition or an
       expression. A @racket[_body] can parse as a definition only if
       it is not preceded by any expression, and the last
       @racket[_body] must be an expression; see also @secref["intdefs"].}
一个一元标识符以@racket[_body]结束代表任何子表；它将被解析为局部定义或表达式。只有在没有任何表达式之前，一个@racket[_body]才能解析为一个定义，而最后一个@racket[_body]必须是一个表达式；参见《内部定义》（@secref["intdefs"]）部分。
    }

]

@;{Square brackets in the grammar indicate a parenthesized sequence of
forms, where square brackets are normally used (by convention). That
is, square brackets @italic{do not} mean optional parts of the
syntactic form.}
在语法的方括号表示形式的括号序列，其中方括号通常用于（约定）。也就是说，方括号@italic{并不（do not）}意味着是句法表的可选部分。

@;{A @racketmetafont{...} indicates zero or more repetitions of the
preceding form, and @racketmetafont{...+} indicates one or more
repetitions of the preceding datum. Otherwise, non-italicized
identifiers stand for themselves.}
一个@racketmetafont{...}表示前一个表的零个或多个重复，@racketmetafont{...+}表示前面数据的一个或多个重复。否则，非斜体标识代表自己。

@;{Based on the above grammar, then, here are a few conforming uses of
@racketkeywordfont{something}:}
根据上面的语法，这里有一些@racketkeywordfont{something}的合乎逻辑的用法：

@racketblock[
(#,(racketkeywordfont "something") [x])
(#,(racketkeywordfont "something") [x] (+ 1 2))
(#,(racketkeywordfont "something") [x my-favorite-martian x] (+ 1 2) #f)
]

@;{Some syntactic-form specifications refer to meta-variables that are
not implicitly defined and not previously defined. Such meta-variables
are defined after the main form, using a BNF-like format for
alternatives:}
一些语法表规范指的是不隐式定义而不是预先定义的元变量。这样的元变量在主表定义后面使用BNF-like格式提供选择：

@specform/subs[(#,(racketkeywordfont "something-else") [thing ...+] an-expr ...)
               ([thing thing-id
                       thing-keyword])]

@;{The above example says that, within a @racketkeywordfont{something-else}
form, a @racket[_thing] is either an identifier or a keyword.}
上面的例子表明，在@racketkeywordfont{something-else}表中，一个@racket[_thing]要么是标识符要么是关键字。

@;------------------------------------------------------------------------

@include-section["binding.scrbl"]
@include-section["apply.scrbl"]
@include-section["lambda.scrbl"]
@include-section["define.scrbl"]
@include-section["let.scrbl"]
@include-section["cond.scrbl"]
@include-section["begin.scrbl"]
@include-section["set.scrbl"]
@include-section["quote.scrbl"]
@include-section["qq.scrbl"]
@include-section["case.scrbl"]
@include-section["parameterize.scrbl"]
