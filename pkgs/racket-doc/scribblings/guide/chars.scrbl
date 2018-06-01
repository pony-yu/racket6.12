#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "characters"]{Characters}}
@title[#:tag "characters"]{字符（Character）}

@;{A Racket @deftech{character} corresponds to a Unicode @defterm{scalar
value}. Roughly, a scalar value is an unsigned integer whose
representation fits into 21 bits, and that maps to some notion of a
natural-language character or piece of a character. Technically, a
scalar value is a simpler notion than the concept called a
``character'' in the Unicode standard, but it's an approximation that
works well for many purposes. For example, any accented Roman letter
can be represented as a scalar value, as can any common Chinese character.}
Racket @deftech{字符（character）}对应于Unicode@defterm{标量值（scalar
value）}。粗略地说，一个标量值是一个无符号整数，表示为21位，并且映射到某种自然语言字符或字符块的某些概念。从技术上讲，一个标量值是一个比Unicode标准中的一个“字符”更简单的概念，但它是一种有许多作用的近似值。例如，任何重音罗马字母都可以表示为一个标量值，就像任何普通的汉字字符一样。

@;{Although each Racket character corresponds to an integer, the
character datatype is separate from numbers. The
@racket[char->integer] and @racket[integer->char] procedures convert
between scalar-value numbers and the corresponding character.}
虽然每个Racket字符对应一个整数，但字符数据类型和数值是有区别的。@racket[char->integer]和@racket[integer->char]过程在标量值和相应字符之间转换。

@;{A printable character normally prints as @litchar{#\} followed
by the represented character. An unprintable character normally prints
as @litchar{#\u} followed by the scalar value as hexadecimal
number. A few characters are printed specially; for example, the space
and linefeed characters print as @racket[#\space] and
@racket[#\newline], respectively.}
一个可打印字符通常打印为以@litchar{#\}后跟着代表字符的形式。一个非打印字符通常打印为以@litchar{#\u}后跟着十六进制数值的标量值的形式。几个字符以特殊方式打印；例如，空格和换行符分别打印为@racket[#\space]和@racket[#\newline]。

@;{@refdetails/gory["parse-character"]{the syntax of characters}}
@margin-note{在《The Racket Reference（Racket参考）》的字符解析文档有字符语法的更好的知识点。}

@examples[
(integer->char 65)
(char->integer #\A)
#\u03BB
(eval:alts @#,racketvalfont["#\\u03BB"] #\u03BB)
(integer->char 17)
(char->integer #\space)
]

@;{The @racket[display] procedure directly writes a character to the
current output port (see @secref["i/o"]), in contrast to the
character-constant syntax used to print a character result.}
@racket[display]过程直接将一个字符写入到当前输出端口（详见《@secref["i/o"]》)，与用于打印一个字符结果的字符常量语法形成对照。

@examples[
#\A
(display #\A)
]

@;{Racket provides several classification and conversion procedures on
characters. Beware, however, that conversions on some Unicode
characters work as a human would expect only when they are in a string
(e.g., upcasing ``@elem["\uDF"]'' or downcasing ``@elem["\u03A3"]'').}
Racket提供了几种对字符的分类和转换的过程。然而，注意某些Unicode字符要只有它们在一个字符串中和一个人所希望的那样转换才行（例如，”@elem["\uDF"]”的大写转换或者”@elem["\u03A3"]”的小写转换）。

@examples[
(char-alphabetic? #\A)
(char-numeric? #\0)
(char-whitespace? #\newline)
(char-downcase #\A)
(char-upcase #\uDF)
]

@;{The @racket[char=?] procedure compares two or more characters, and
@racket[char-ci=?] compares characters ignoring case. The
@racket[eqv?] and @racket[equal?] procedures behave the same as
@racket[char=?] on characters; use @racket[char=?] when you want to
more specifically declare that the values being compared are
characters.}
@racket[char=?]过程比较两个或多个字符，@racket[char-ci=?]比较字符但忽略大写。@racket[eqv?]和@racket[equal?]过程在字符方面的行为与@racket[char=?]表现一样；当你更具体地声明正在比较的值是字符时使用@racket[char=?]。

@examples[
(char=? #\a #\A)
(char-ci=? #\a #\A)
(eqv? #\a #\A)
]

@;{@refdetails["characters"]{characters and character procedures}}
@margin-note{在《The Racket Reference（Racket参考）》的字符部分中提供字符和字符过程的更多信息。}
