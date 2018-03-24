#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "quote"]{Quoting: @racket[quote] and @racketvalfont{@literal{'}}}}
@title[#:tag "quote"]{引用：@racket[quote]和@racketvalfont{@literal{'}}}

@refalso["quote"]{@racket[quote]}

@;{The @racket[quote] form produces a constant:}
引用（@racket[quote]）表产生一个常数：

@specform[(#,(racketkeywordfont "quote") datum)]

@;{The syntax of a @racket[datum] is technically specified as anything
that the @racket[read] function parses as a single element. The value
of the @racket[quote] form is the same value that @racket[read] would
produce given @racket[_datum].}
@racket[datum]的语法在技术上被指定为@racket[read]函数解析为单个元素的任何内容。@racket[quote]表的值与@racket[read]将产生给定的@racket[_datum]的值相同。

@;{The @racket[_datum] can be a symbol, a boolean, a number, a (character
or byte) string, a character, a keyword, an empty list, a pair (or
list) containing more such values, a vector containing more such
values, a hash table containing more such values, or a box containing
another such value.}
@racket[_datum]可以是一个符号、一个布尔值、一个数字、一个（字符或字节）字符串、一个字符、一个关键字、一个空列表、一个包含更多类似值的配对（或列表），一个包含更多类似值的向量，一个包含更多类似值的哈希表，或者一个包含其它类似值的格子。

@examples[
(eval:alts (#,(racketkeywordfont "quote") apple) 'apple)
(eval:alts (#,(racketkeywordfont "quote") #t) #t)
(eval:alts (#,(racketkeywordfont "quote") 42) 42)
(eval:alts (#,(racketkeywordfont "quote") "hello") "hello")
(eval:alts (#,(racketkeywordfont "quote") ()) '())
(eval:alts (#,(racketkeywordfont "quote") ((1 2 3) #2("z" x) . the-end)) '((1 2 3) #2("z" x) . the-end))
(eval:alts (#,(racketkeywordfont "quote") (1 2 #,(racketparenfont ".") (3))) '(1 2 . (3)))
]

@;{As the last example above shows, the @racket[_datum] does not have to
match the normalized printed form of a value. A @racket[_datum] cannot
be a printed representation that starts with @litchar{#<}, so it
cannot be @|void-const|, @|undefined-const|, or a procedure.}
正如上面最后一个示例所示，@racket[_datum]不需要匹配一个值的格式化的打印表。一个@racket[_datum]不能作为从@litchar{#<}开始的打印呈现，所以不能是@|void-const|、@|undefined-const|或一个过程。

@;{The @racket[quote] form is rarely used for a @racket[_datum] that is a
boolean, number, or string by itself, since the printed forms of those
values can already be used as constants. The @racket[quote] form is
more typically used for symbols and lists, which have other meanings
(identifiers, function calls, etc.) when not quoted.}
@racket[quote]表很少用于@racket[_datum]的布尔值、数字或字符串本身，因为这些值的打印表可以用作常量。@racket[quote]表更常用于符号和列表，当没有被引用时，它具有其他含义（标识符、函数调用等）。

@;{An expression}
一个表达式：

@specform[(quote @#,racketvarfont{datum})]

@;{is a shorthand for}
是以下内容的简写

@racketblock[
(#,(racketkeywordfont "quote") #,(racket _datum))
]

@;{and this shorthand is almost always used instead of
@racket[quote]. The shorthand applies even within the @racket[_datum],
so it can produce a list containing @racket[quote].}
这个简写几乎总是用来代替@racket[quote]。简写甚至应用于@racket[_datum]中，因此它可以生成包含@racket[quote]的列表。

@;{@refdetails["parse-quote"]{the @racketvalfont{@literal{'}} shorthand}}
@refdetails["parse-quote"]{@racketvalfont{@literal{'}}简写}

@examples[
'apple
'"hello"
'(1 2 3)
(display '(you can 'me))
]
