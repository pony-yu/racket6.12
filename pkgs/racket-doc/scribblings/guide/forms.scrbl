#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "scheme-forms" #:style 'toc]{Expressions and Definitions}}
@title[#:tag "scheme-forms" #:style 'toc]{表达式和定义}

@;{The @secref["to-scheme"] chapter introduced some of Racket's syntactic
forms: definitions, procedure applications, conditionals, and so
on. This section provides more details on those forms, plus a few
additional basic forms.}
《@secref["to-scheme"]》这一章介绍了一些基本的Racket的句法表：定义、过程程序、条件表达式等等。本节提供这些形式的更详细信息，以及一些附加的基本表。

@local-table-of-contents[]

@;{@section[#:tag "syntax-notation"]{Notation}}
@section[#:tag "syntax-notation"]{标记法}

@;{This chapter (and the rest of the documentation) uses a slightly
different notation than the character-based grammars of the
@secref["to-scheme"] chapter. The grammar for a use of a syntactic
form @racketkeywordfont{something} is shown like this:}
这一章（以及其余的文档）使用了一个稍微不同的标记法，而不是基于字符的《@secref["to-scheme"]》章里的语法。对于一个句法表@racketkeywordfont{something}的使用表现为如下方式：

@specform[(#,(racketkeywordfont "something") [id ...+] an-expr ...)]

@;{The italicized meta-variables in this specification, such as
@racket[_id] and @racket[_an-expr], use the syntax of Racket
identifiers, so @racket[_an-expr] is one meta-variable. A naming
convention implicitly defines the meaning of many meta-variables:}
在本规范中斜体的元变量，如@racket[_id]和@racket[_an-expr]，使用Racket标识符的语法，所以@racket[_an-expr]是一元变量。一个命名约定隐式地定义了许多元变量的含义：

@itemize[

 @item{
  @;{A meta-variable that ends in @racket[_id] stands for an
       identifier, such as @racketidfont{x} or
       @racketidfont{my-favorite-martian}.}
    一个以@racket[_id]结束的元变量代表一个标识符，如@racketidfont{x}或@racketidfont{my-favorite-martian}。
    }

 @item{
  @;{A meta-identifier that ends in @racket[_keyword] stands
       for a keyword, such as @racket[#:tag].}
    一个以@racket[_keyword]结束的元标识符代表一个关键字，如@racket[#:tag]。
    }

 @item{
  @;{A meta-identifier that ends with @racket[_expr] stands for any
       sub-form, and it will be parsed as an expression.}
    一个以@racket[_expr]结束的元标识符代表任意子表，它将被解析为一个表达式。
    }

 @item{
  @;{A meta-identifier that ends with @racket[_body] stands for any
       sub-form; it will be parsed as either a local definition or an
       expression. A @racket[_body] can parse as a definition only if
       it is not preceded by any expression, and the last
       @racket[_body] must be an expression; see also @secref["intdefs"].}
一个以@racket[_body]结束的元标识符代表任意子表；它将被解析为一个局部定义或者一个表达式。一个@racket[_body]只有不被任何表达式前置时才能解析为一个定义，并且最后一个@racket[_body]必须是一个表达式；参见《@secref["intdefs"]》部分。
    }
]

@;{Square brackets in the grammar indicate a parenthesized sequence of
forms, where square brackets are normally used (by convention). That
is, square brackets @italic{do not} mean optional parts of the
syntactic form.}
在语法中的方括号表示表的一个括号序列，这里方括号通常被使用（约定）。也就是说，方括号@italic{并不}表示是句法表的可选部分。

@;{A @racketmetafont{...} indicates zero or more repetitions of the
preceding form, and @racketmetafont{...+} indicates one or more
repetitions of the preceding datum. Otherwise, non-italicized
identifiers stand for themselves.}
一个@racketmetafont{...}表示前置表的零个或多个重复，@racketmetafont{...+}表示前置数据的一个或多个重复。另外，非斜体标识符代表它们自己。

@;{Based on the above grammar, then, here are a few conforming uses of
@racketkeywordfont{something}:}
那么，基于上面的语法，这里有一些@racketkeywordfont{something}的与以上相符合的用法：

@racketblock[
(#,(racketkeywordfont "something") [x])
(#,(racketkeywordfont "something") [x] (+ 1 2))
(#,(racketkeywordfont "something") [x my-favorite-martian x] (+ 1 2) #f)
]

@;{Some syntactic-form specifications refer to meta-variables that are
not implicitly defined and not previously defined. Such meta-variables
are defined after the main form, using a BNF-like format for
alternatives:}
一些语法表规范指既不是隐式定义的也不是预定义的元变量。这样的元变量在主表后面定义，使用一个BNF-like表提供选择：

@specform/subs[(#,(racketkeywordfont "something-else") [thing ...+] an-expr ...)
               ([thing thing-id
                       thing-keyword])]

@;{The above example says that, within a @racketkeywordfont{something-else}
form, a @racket[_thing] is either an identifier or a keyword.}
上面的例子表明，在一个@racketkeywordfont{something-else}表中，一个@racket[_thing]要么是一个标识符要么是一个关键字。

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
