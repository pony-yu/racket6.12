#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "vectors"]{Vectors}}
@title[#:tag "vectors"]{向量（Vector）}

@;{A @deftech{vector} is a fixed-length array of arbitrary
values. Unlike a list, a vector supports constant-time access and
update of its elements.}
一个@deftech{向量（vector）}是任意值的一个固定长度数组。与一个列表不同，一个向量支持常量时间访问和它的元素更新。

@;{A vector prints similar to a list---as a parenthesized sequence of its
elements---but a vector is prefixed with @litchar{#} after
@litchar{'}, or it uses @racketresult[vector] if one of its elements
cannot be expressed with @racket[quote].}
一个向量打印类似于一个列表——作为其元素的一个括号序列——但一个向量要在@litchar{'}之后加前缀@litchar{#}，或如果它的元素不能用引号表示则使用@racketresult[vector]表示。

@;{For a vector as an expression, an optional length can be
supplied. Also, a vector as an expression implicitly @racket[quote]s
the forms for its content, which means that identifiers and
parenthesized forms in a vector constant represent symbols and lists.}
对于作为一个表达式的一个向量，可以提供一个可选长度。同时，一个向量作为一个表达式隐式地为它的内容@racket[quote]（引用）这个表，这意味着在一个向量常数中的标识符和括号表代表符号和列表。

@;{@refdetails/gory["parse-vector"]{the syntax of vectors}}
@margin-note{在《Racket参考》中的“读取向量（Reading Vectors）”文档有向量的语法更好的知识点。}

@examples[
(eval:alts @#,racketvalfont{#("a" "b" "c")} #("a" "b" "c"))
(eval:alts @#,racketvalfont{#(name (that tune))} #(name (that tune)))
(eval:alts @#,racketvalfont{#4(baldwin bruce)} #4(baldwin bruce))
(vector-ref #("a" "b" "c") 1)
(vector-ref #(name (that tune)) 1)
]

@;{Like strings, a vector is either mutable or immutable, and vectors
written directly as expressions are immutable.}
像字符串一样，一个向量要么是可变的，要么是不可变的，向量直接编写为表达式是不可变的。

@;{Vectors can be converted to lists and vice versa via
@racket[vector->list] and @racket[list->vector]; such conversions are
particularly useful in combination with predefined procedures on
lists. When allocating extra lists seems too expensive, consider
using looping forms like @racket[for/fold], which recognize vectors as
well as lists.}
向量可以通过@racket[vector->list]和@racket[list->vector]转换成列表，反之亦然。这种转换在与对列表的预定义过程相结合中是特别有用的。当分配额外的列表似乎太昂贵时，考虑使用像@racket[for/fold]的循环表，它像列表一样识别向量。

@examples[
(list->vector (map string-titlecase
                   (vector->list #("three" "blind" "mice"))))
]

@;{@refdetails["vectors"]{vectors and vector procedures}}
@margin-note{在《Racket参考》中的“向量（vectors）”部分提供有关向量和向量过程的更多内容。}
