#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "symbols"]{Symbols}}
@title[#:tag "symbols"]{符号（Symbol）}

@;{A @deftech{symbol} is an atomic value that prints like an identifier
preceded with @litchar{'}.  An expression that starts with @litchar{'}
and continues with an identifier produces a symbol value.}
一个@deftech{符号（symbol）}是一个原子值，它像前面的标识符那样以@litchar{'}前缀打印。一个表达式以@litchar{'}开始并以标识符继续表达式产生一个符号值。

@examples[
'a
(symbol? 'a)
]

@;{For any sequence of characters, exactly one corresponding symbol is
@defterm{interned}; calling the @racket[string->symbol] procedure, or
@racket[read]ing a syntactic identifier, produces an interned
symbol. Since interned symbols can be cheaply compared with
@racket[eq?] (and thus @racket[eqv?] or @racket[equal?]), they serve
as a convenient values to use for tags and enumerations.}
对于任何字符序列，一个相应的符号被@defterm{保留（interned）}；调用@racket[string->symbol]程序，或@racket[read]一个语法标识，产生一个保留符号。由于互联网的符号可以方便地用@racket[eq?]（或这样：@racket[eqv?]或@racket[equal?]）进行比较，所以他们作为一个易于使用的标签和枚举值提供。

@;{Symbols are case-sensitive. By using a @racketfont{#ci} prefix or in
other ways, the reader can be made to case-fold character sequences to
arrive at a symbol, but the reader preserves case by default.}
符号是区分大小写的。通过使用一个@racketfont{#ci}前缀或其它方式，在读者保留默认情况下，读者可以将大小写字符序列生成一个符号。

@examples[
(eq? 'a 'a)
(eq? 'a (string->symbol "a"))
(eq? 'a 'b)
(eq? 'a 'A)
(eval:alts @#,elem{@racketfont{#ci}@racketvalfont{@literal{'A}}} #ci'A)
]

@;{Any string (i.e., any character sequence) can be supplied to
@racket[string->symbol] to obtain the corresponding symbol. For reader
input, any character can appear directly in an identifier, except for
whitespace and the following special characters:}
任何字符串（即，任何字符序列）都可以提供给@racket[string->symbol]以获得相应的符号。读者输入任何字符都可以直接出现在一个标识符里，除了空白和以下特殊字符：

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar{|} @litchar{\}
}

@;{Actually, @litchar{#} is disallowed only at the beginning of a symbol,
and then only if not followed by @litchar{%}; otherwise, @litchar{#} is
allowed, too. Also, @litchar{.} by itself is not a symbol.}
实际上，@litchar{#}只有在一个符号开始是不允许的，或者仅仅如果随后是@litchar{%}；然而，@litchar{#}也被允许。同样。.它本身不是一个符号。

@;{Whitespace or special characters can be included in an identifier by
quoting them with @litchar{|} or @litchar{\}. These quoting
mechanisms are used in the printed form of identifiers that contain
special characters or that might otherwise look like numbers.}
空格或特殊字符可以通过用@litchar{|}或@litchar{\}引用包含标识符。这些引用机制用于包含特殊字符或可能看起来像数字的标识符的打印形式中。

@examples[
(string->symbol "one, two")
(string->symbol "6")
]

@;{@refdetails/gory["parse-symbol"]{the syntax of symbols}}
@refdetails/gory["parse-symbol"]{符号语法}

@;{The @racket[write] function prints a symbol without a @litchar{'}
prefix. The @racket[display] form of a symbol is the same as the
corresponding string.}
@racket[write]函数打印一个没有@litchar{'}前缀的符号。一个符号的@racket[display]表与相应的字符串相同。

@examples[
(write 'Apple)
(display 'Apple)
(write '|6|)
(display '|6|)
]

@;{The @racket[gensym] and @racket[string->uninterned-symbol] procedures
generate fresh @defterm{uninterned} symbols that are not equal
(according to @racket[eq?]) to any previously interned or uninterned
symbol. Uninterned symbols are useful as fresh tags that cannot be
confused with any other value.}
@racket[gensym]和@racket[string->uninterned-symbol]过程产生新的非保留（@defterm{uninterned}）符号，那不等同于（比照@racket[eq?]）任何先前的保留或非保留符号。非保留符号是可用的新标签，不能与任何其它值混淆。

@examples[
(define s (gensym))
(eval:alts s 'g42)
(eval:alts (eq? s 'g42) #f)
(eq? 'a (string->uninterned-symbol "a"))
]


@;{@refdetails["symbols"]{symbols}}
@refdetails["symbols"]{符号}
