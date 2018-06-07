#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "bytestrings"]{Bytes and Byte Strings}}
@title[#:tag "bytestrings"]{字节（Byte）和字节字符串（Byte String）}

@;{A @deftech{byte} is an exact integer between @racket[0] and
@racket[255], inclusive. The @racket[byte?] predicate recognizes
numbers that represent bytes.}
一个@deftech{字节（byte）}是一个在@racket[0]到@racket[255]之间的精确整数。@racket[byte?]判断识别表示字节的数字。

@examples[
(byte? 0)
(byte? 256)
]

@;{A @deftech{byte string} is similar to a string---see
@secref["strings"]---but its content is a sequence of bytes
instead of characters. Byte strings can be used in applications that
process pure ASCII instead of Unicode text. The printed form of a
byte string supports such uses in particular, because a byte string
prints like the ASCII decoding of the byte string, but prefixed with a
@litchar{#}. Unprintable ASCII characters or non-ASCII bytes in the
byte string are written with octal notation.}
一个@deftech{字节字符串（byte string）}类似于一个字符串——参见《@secref["strings"]》，但它的内容是字节序列而不是字符。字节字符串可用于处理纯ASCII文本而不是Unicode文本的应用程序中。一个字节字符串的打印形式特别支持这样使用，因为一个字节字符串打印像字节字符串的ASCII解码，但有一个@litchar{#}前缀。在字节字符串中不可打印的ASCII字符或非ASCII字节用八进制表示法编写。

@;{@refdetails/gory["parse-string"]{the syntax of byte strings}}
@margin-note{在《Racket参考》中的“读取字符串（Reading Strings）”文档有关于字节字符串语法的更好的知识点。}

@examples[
#"Apple"
(bytes-ref #"Apple" 0)
(make-bytes 3 65)
(define b (make-bytes 2 0))
b
(bytes-set! b 0 1)
(bytes-set! b 1 255)
b
]

@;{The @racket[display] form of a byte string writes its raw bytes to the
current output port (see @secref["i/o"]). Technically,
@racket[display] of a normal (i.e,. character) string prints the UTF-8
encoding of the string to the current output port, since output is
ultimately defined in terms of bytes; @racket[display] of a byte
string, however, writes the raw bytes with no encoding. Along the same
lines, when this documentation shows output, it technically shows the
UTF-8-decoded form of the output.}
一个字节字符串的@racket[display]表写入其原始字节到当前输出端口（详见《@secref["i/o"]》部分）。从技术上讲，一个通常（即，字符）的@racket[display]字符串打印字符串的UTF-8编码到当前输出端口，因为输出是以字节为单位的最终定义；然而一个字节字符串的@racket[display]用无编码的方式写入原始字节。按同样的思路，当这个文档显示输出时，它严格说来是显示输出的UTF-8编码格式。

@examples[
(display #"Apple")
(eval:alts (code:line (display @#,racketvalfont{"\316\273"})  (code:comment @#,t{@;{same as }等同于@racket["\316\273"]}))
           (display "\316\273"))
(code:line (display #"\316\273") (code:comment @#,t{@;{UTF-8 encoding of @elem["\u03BB"]}@elem["\u03BB"]的UTF-8编码}))
]

@;{For explicitly converting between strings and byte strings, Racket
supports three kinds of encodings directly: UTF-8, Latin-1, and the
current locale's encoding. General facilities for byte-to-byte
conversions (especially to and from UTF-8) fill the gap to support
arbitrary string encodings.}
对于在字符串和字节字符串之间的显式转换，Racket直接支持三种编码：UTF-8，Latin-1和当前的本地编码。字节到字节转换（特别是转换到UTF-8和从UTF-8转换来）的通用工具弥合了支持任意字符串编码的差异分歧。

@examples[
(bytes->string/utf-8 #"\316\273")
(bytes->string/latin-1 #"\316\273")
(code:line
 (parameterize ([current-locale "C"])  (code:comment @#,elem{@;{C locale supports ASCII,}C局部支持ASCII，})
   (bytes->string/locale #"\316\273")) (code:comment @#,elem{@;{only, so...}仅仅，这样……}))
(let ([cvt (bytes-open-converter "cp1253" (code:comment @#,elem{@;{Greek code page}希腊代码页})
                                 "UTF-8")]
      [dest (make-bytes 2)])
  (bytes-convert cvt #"\353" 0 1 dest)
  (bytes-close-converter cvt)
  (bytes->string/utf-8 dest))
]

@;{@refdetails["bytestrings"]{byte strings and byte-string procedures}}
@(require scriblib/footnote)
@margin-note{在《Racket参考》里的“字节字符串（Byte Strings）”部分提供了关于字节字符串和字节字符串函数的更详尽内容。}
