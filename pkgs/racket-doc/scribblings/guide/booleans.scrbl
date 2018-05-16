#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "booleans"]{Booleans}}
@title[#:tag "booleans"]{布尔值（Boolean）}

@;{Racket has two distinguished constants to represent boolean values:
@racket[#t] for true and @racket[#f] for false. Uppercase
@racketvalfont{#T} and @racketvalfont{#F} are parsed as the same
values, but the lowercase forms are preferred.}
Racket有表示布尔值的两个重要的常数：@racket[#t]表示真，@racket[#f]表示假。大写的@racketvalfont{#T}和@racketvalfont{#F}解析为同样的值，但小写形式是首选。

@;{The @racket[boolean?] procedure recognizes the two boolean
constants. In the result of a test expression for @racket[if],
@racket[cond], @racket[and], @racket[or], etc., however, any value
other than @racket[#f] counts as true.}
@racket[boolean?]程序识别两个布尔常量。然而，在对@racket[if]、@racket[cond]、 @racket[and]、@racket[or]等等的一个测试表达式的结果里，除了@racket[#f]之外，任何值都是记为真。

@examples[
(= 2 (+ 1 1))
(boolean? #t)
(boolean? #f)
(boolean? "no")
(if "no" 1 0)
]

