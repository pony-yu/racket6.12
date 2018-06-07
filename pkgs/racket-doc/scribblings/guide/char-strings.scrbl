#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "strings"]{Strings (Unicode)}}
@title[#:tag "strings"]{字符串（Unicode）}

@;{A @deftech{string} is a fixed-length array of
@seclink["characters"]{characters}. It prints using doublequotes,
where doublequote and backslash characters within the string are
escaped with backslashes. Other common string escapes are supported,
including @litchar{\n} for a linefeed, @litchar{\r} for a
carriage return, octal escapes using @litchar{\} followed by up
to three octal digits, and hexadecimal escapes with @litchar{\u}
(up to four digits).  Unprintable characters in a string are normally
shown with @litchar{\u} when the string is printed.}
一个@deftech{字符串（string）}是一个固定长度的@seclink["characters"]{字符（characters）}数组。它使用双引号打印，在字符串中的双引号和反斜杠字符是用反斜杠转义。其它普通的字符串转义被支持，包括@litchar{\n}用于一个换行，@litchar{\r}用于一个回车，使用@litchar{\}后边跟着多达三个八进制数字实现八进制转义，以及用@litchar{\u}（多达四位数）实现十六进制转义。在打印字符串时通常用@litchar{\u}显示一个字符串中的不可打印字符。

@;{@refdetails/gory["parse-string"]{the syntax of strings}}
@margin-note{在《Racket参考》中的“读取字符串（Reading Strings）”文档有关于字符串语法的更好的知识点。}

@;{The @racket[display] procedure directly writes the characters of a
string to the current output port (see @secref["i/o"]), in contrast
to the string-constant syntax used to print a string result.}
@racket[display]过程直接将一个字符串中的字符写入当前输出端口（见《@secref["i/o"]》），在字符串常量语法对比中用于打印一个字符串结果。

@examples[
"Apple"
(eval:alts @#,racketvalfont{"\u03BB"} "\u03BB")
(display "Apple")
(display "a \"quoted\" thing")
(display "two\nlines")
(eval:alts (display @#,racketvalfont{"\u03BB"}) (display "\u03BB"))
]

@;{A string can be mutable or immutable; strings written directly as
expressions are immutable, but most other strings are mutable. The
@racket[make-string] procedure creates a mutable string given a length
and optional fill character. The @racket[string-ref] procedure
accesses a character from a string (with 0-based indexing); the
@racket[string-set!]  procedure changes a character in a mutable
string.}
一个字符串可以是可变的也可以是不可变的；作为表达式直接编写的字符串是不可变的，但大多数其它字符串是可变的。@racket[make-string]过程创建一个给定一个长度和可选填充字符的可变字符串。@racket[string-ref]过程从一个字符串（用基于0的索引）中访问一个字符。@racket[string-set!]过程在一个可变字符串中更改一个字符。

@examples[
(string-ref "Apple" 0)
(define s (make-string 5 #\.))
s
(string-set! s 2 #\u03BB)
s
]

@;{String ordering and case operations are generally
@defterm{locale-independent}; that is, they work the same for all
users. A few @defterm{locale-dependent} operations are provided that
allow the way that strings are case-folded and sorted to depend on the
end-user's locale. If you're sorting strings, for example, use
@racket[string<?] or @racket[string-ci<?] if the sort result should be
consistent across machines and users, but use @racket[string-locale<?]
or @racket[string-locale-ci<?] if the sort is purely to order strings
for an end user.}
字符串排序和状态操作通常是区域无关（@defterm{locale-independent}）的；也就是说，它们对所有用户都采用相同的工作方式。一些区域相关（@defterm{locale-dependent}）的操作被提供，它们允许字符串折叠和排序的方式取决于最终用户的区域设置。如果你在排序字符串，例如，如果排序结果应该在机器和用户之间保持一致，使用@racket[string<?]或@racket[string-ci<?]，但如果排序纯粹是为一个最终用户整理字符串，使用@racket[string-locale<?]或@racket[string-locale-ci<?]。

@examples[
(string<? "apple" "Banana")
(string-ci<? "apple" "Banana")
(string-upcase "Stra\xDFe")
(parameterize ([current-locale "C"])
  (string-locale-upcase "Stra\xDFe"))
]

@;{For working with plain ASCII, working with raw bytes, or
encoding/decoding Unicode strings as bytes, use
@seclink["bytestrings"]{byte strings}.}
对于使用纯粹的ASCII、使用原始字节或编码/解码Unicode字符串为字节，使用@seclink["bytestrings"]{字节字符串（byte strings）}。

@;{@refdetails["strings"]{strings and string procedures}}
@margin-note{在《Racket参考》中的字符串（strings）部分提供更多字符串和字符串过程的信息。}
