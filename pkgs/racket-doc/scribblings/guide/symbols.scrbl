#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "symbols"]{Symbols}}
@title[#:tag "symbols"]{符号（Symbol）}

@;{A @deftech{symbol} is an atomic value that prints like an identifier
preceded with @litchar{'}.  An expression that starts with @litchar{'}
and continues with an identifier produces a symbol value.}
一个@deftech{符号（symbol）}是一个原子值，它像一个前面的标识符那样以@litchar{'}前缀打印。一个以@litchar{'}开始并带一个标识符的表达式产生一个符号值。

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
对于字符的任何序列，正好有一个相应的符号被@defterm{保留（interned）}；调用@racket[string->symbol]过程或者@racket[read]一个语法标识符，产生一个保留符号。由于保留符号可以被用@racket[eq?]（或这样：@racket[eqv?]或@racket[equal?]）方便地比较，所以它们作为方便的值用于标签和枚举。

@;{Symbols are case-sensitive. By using a @racketfont{#ci} prefix or in
other ways, the reader can be made to case-fold character sequences to
arrive at a symbol, but the reader preserves case by default.}
符号是区分大小写的。通过使用一个@racketfont{#ci}前缀或其它方式，读取器能够被要求去折叠容器序列以获得一个符号，但是读取器通过默认方式保护容器。

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
任何字符串（或者说，任何字符序列）都可以提供给@racket[string->symbol]以获得对应的符号。对于读取器输入来说，任何字符都可以直接出现在一个标识符里，空白和以下特殊字符除外：

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar{|} @litchar{\}
}

@;{Actually, @litchar{#} is disallowed only at the beginning of a symbol,
and then only if not followed by @litchar{%}; otherwise, @litchar{#} is
allowed, too. Also, @litchar{.} by itself is not a symbol.}
实际上，@litchar{#}仅仅不允许在一个符号开始位置，并且也仅仅不允许@litchar{%}在最后位置；除此之外，#是被允许的。此外，@litchar{.}本身不是一个符号。

@;{Whitespace or special characters can be included in an identifier by
quoting them with @litchar{|} or @litchar{\}. These quoting
mechanisms are used in the printed form of identifiers that contain
special characters or that might otherwise look like numbers.}
空格或特殊字符可以通过用@litchar{|}或@litchar{\}引用包含进一个标识符里。这些引用机制用于包含特殊字符或可能额外看起来像数字的标识符的打印表中。

@examples[
(string->symbol "one, two")
(string->symbol "6")
]

@;{@refdetails/gory["parse-symbol"]{the syntax of symbols}}
@margin-note{在《Racket参考》中的“读取符号（Reading Symbols）”文档有关于符号语法的更好的知识点。}

@;{The @racket[write] function prints a symbol without a @litchar{'}
prefix. The @racket[display] form of a symbol is the same as the
corresponding string.}
@racket[write]函数打印一个没有一个@litchar{'}前缀的符号。一个符号的@racket[display]表与对应的字符串相同。

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
@racket[gensym]和@racket[string->uninterned-symbol]过程生成新的@defterm{非保留（uninterned）}符号，它不等同于（比照@racket[eq?]）任何先前的保留或非保留符号。非保留符号作为新标签是有用的，它不会与其它任何值混淆。

@examples[
(define s (gensym))
(eval:alts s 'g42)
(eval:alts (eq? s 'g42) #f)
(eq? 'a (string->uninterned-symbol "a"))
]

@;{@refdetails["symbols"]{symbols}}
@margin-note{在《Racket参考》中的“符号（Symbols）”文档有关于符号的更多信息。}
