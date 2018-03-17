#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title{Simple Values}}
@title{简单的值}

@;{Racket values include numbers, booleans, strings, and byte strings. In
DrRacket and documentation examples (when you read the documentation
in color), value expressions are shown in green.}
Racket值包括数字、布尔值、字符串和字节字符串。DrRacket和文档示例中(当你在着色状态下阅读文档时)，值表达式显示为绿色。

@;{@defterm{Numbers} are written in the usual way, including fractions
and imaginary numbers:}
@defterm{数字（number）}以通常的方式写成，包括以通常的方式写成数字，包括分数和虚数，分数和虚数：

@;{@moreguide["numbers"]{number}}
@moreguide["numbers"]{数值（Numbers）}

@racketblock[
1       3.14
1/2     6.02e+23
1+2i    9999999999999999999999
]

@;{@defterm{Booleans} are @racket[#t] for true and @racket[#f] for
false. In conditionals, however, all non-@racket[#f] values are
treated as true.}
@defterm{布尔值（boolean）}是@racket[#t]表示真和@racket[#f]表示假。然而，在条件句，所有非@racket[#f]值被视为真。

@;{@moreguide["booleans"]{booleans}}
@moreguide["booleans"]{布尔值（boolean）}

@;{@defterm{Strings} are written between doublequotes. Within a string,
backslash is an escaping character; for example, a backslash followed
by a doublequote includes a literal doublequote in the string. Except
for an unescaped doublequote or backslash, any Unicode character can
appear in a string constant.}
@defterm{字符串（string)}写在双引号（""）之间。在一个字符串中，反斜杠（/）是一个转义字符；例如,一个反斜杠之后的双引号为包括文字双引号的字符串。除了一个保留的双引号或反斜杠，任何Unicode字符都可以在字符串常量中出现。

@;{@moreguide["strings"]{strings}}
@moreguide["strings"]{字符串（string）}

@racketblock[
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"
"\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx"
]

@;{When a constant is evaluated in the @tech{REPL}, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrRacket's @tech{REPL},
results are printed in blue instead of green to highlight the
difference between an input expression and a printed result.}
当一个常量在@tech{REPL}中被求值，通常它的打印结果与输入的语法相同。在某些情况下，打印格式是输入语法的标准化版本。在文档和DrRacket的@tech{REPL}中，结果打印为蓝色而不是绿色以强调打印结果与输入表达式之间的区别。

@examples[
(eval:alts (unsyntax (racketvalfont "1.0000")) 1.0000)
(eval:alts (unsyntax (racketvalfont "\"Bugs \\u0022Figaro\\u0022 Bunny\"")) "Bugs \u0022Figaro\u0022 Bunny")
]
