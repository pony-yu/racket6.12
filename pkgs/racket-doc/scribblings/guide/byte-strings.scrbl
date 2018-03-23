#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "bytestrings"]{Bytes and Byte Strings}}
@title[#:tag "bytestrings"]{字节（Byte）和字节字符串（Byte String）}

@;{A @deftech{byte} is an exact integer between @racket[0] and
@racket[255], inclusive. The @racket[byte?] predicate recognizes
numbers that represent bytes.}
一个@deftech{字节（byte）}是包含@racket[0]到@racket[255]之间的精确整数。@racket[byte?]判断表示字节的数字。

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
一个@deftech{字节字符串（byte string）}类似于字符串——参见@secref["strings"]，但它的内容是字节序列而不是字符。字节字符串可用于处理纯ASCII而不是Unicode文本的应用程序中。一个字节的字符串打印形式特别支持这样的用途，因为一个字节的字符串打印的ASCII的字节字符串解码，但有一个@litchar{#}前缀。在字节字符串不可打印的ASCII字符或非ASCII字节用八进制表示法。

@;{@refdetails/gory["parse-string"]{the syntax of byte strings}}
@refdetails/gory["parse-string"]{字节字符串语法}

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
一个字节字符串的@racket[display]表写入其原始字节的电流输出端口（看《输入和输出》（@secref["i/o"]）部分）。从技术上讲，一个正常的@racket[display]（即，字符编码的字符串）字符串打印到当前输出端口的UTF-8，因为产出的最终依据字节的定义；然而一个字节字符串的@racket[display]，没有编码写入原始字节。同样，当这个文件显示输出，技术上显示输出的utf-8编码格式。

@examples[
(display #"Apple")
(eval:alts (code:line (display @#,racketvalfont{"\316\273"})  (code:comment @#,t{same as @racket["\316\273"]}))
           (display "\316\273"))
(code:line (display #"\316\273") (code:comment @#,t{UTF-8 encoding of @elem["\u03BB"]}))
]

@;{For explicitly converting between strings and byte strings, Racket
supports three kinds of encodings directly: UTF-8, Latin-1, and the
current locale's encoding. General facilities for byte-to-byte
conversions (especially to and from UTF-8) fill the gap to support
arbitrary string encodings.}
字符串和字节字符串之间的显式转换，Racket直接支持三种编码：UTF-8，Latin-1，和当前的本地编码。字节到字节的通用转换器（特别是从UTF-8）弥合了支持任意字符串编码的差异分歧。

@examples[
(bytes->string/utf-8 #"\316\273")
(bytes->string/latin-1 #"\316\273")
(code:line
 (parameterize ([current-locale "C"])  (code:comment @#,elem{C locale supports ASCII,})
   (bytes->string/locale #"\316\273")) (code:comment @#,elem{only, so...}))
(let ([cvt (bytes-open-converter "cp1253" (code:comment @#,elem{Greek code page})
                                 "UTF-8")]
      [dest (make-bytes 2)])
  (bytes-convert cvt #"\353" 0 1 dest)
  (bytes-close-converter cvt)
  (bytes->string/utf-8 dest))
]

@;{@refdetails["bytestrings"]{byte strings and byte-string procedures}}
@refdetails["bytestrings"]{字节字符串和字节字符串函数}
