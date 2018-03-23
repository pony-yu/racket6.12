#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "numbers"]{Numbers}}
@title[#:tag "numbers"]{数值（Number）}

@;{A Racket @deftech{number} is either exact or inexact:}
一个Racket的@deftech{数值（number）}可以是精确的也可以是不精确的：

@itemize[

 @item{
  @;{An @defterm{exact} number is either}
    一个@defterm{精确}的数字是：

       @itemize[

       @item{
    @;{an arbitrarily large or small integer, such as @racket[5],
             @racket[99999999999999999], or @racket[-17];}
      一个任意大的或小的整数，比如：@racket[5]，@racket[99999999999999999]或@racket[-17]；
   }

       @item{
    @;{a rational that is exactly the ratio of two arbitrarily
             small or large integers, such as @racket[1/2],
             @racket[99999999999999999/2], or @racket[-3/4]; or}
一个有理数，即精确的两个任意小的或大的整数比，比如：@racket[1/2]，@racket[99999999999999999/2]或@racket[-3/4]；
      }

       @item{
    @;{a complex number with exact real and imaginary parts
             (where the imaginary part is not zero), such as @racket[1+2i] or
             @racket[1/2+3/4i].}
      一个复数，带有精确的实部和虚部（即虚部不为零），比如：@racket[1+2i]或@racket[1/2+3/4i]。
   }

       ]}

 @item{
  @;{An @defterm{inexact} number is either}
    一个 @defterm{不精确}的数字是：

       @itemize[

        @item{
    @;{an IEEE floating-point representation of a number, such
              as @racket[2.0] or @racket[3.14e87], where the IEEE
              infinities and not-a-number are written
              @racket[+inf.0], @racket[-inf.0], and @racket[+nan.0]
              (or @racketvalfont{-nan.0}); or}
一个数的一个IEEE浮点表示，比如：@racket[2.0]或@racket[3.14e87]，其中IEEE无穷大和非数书写为：@racket[+inf.0]，@racket[-inf.0]和@racket[+nan.0]（或@racketvalfont{-nan.0}）；
      }

        @item{
    @;{a complex number with real and imaginary parts that are
              IEEE floating-point representations, such as
              @racket[2.0+3.0i] or @racket[-inf.0+nan.0i]; as a
              special case, an inexact complex number can have an
              exact zero real part with an inexact imaginary part.}
一个带有实部和虚部配对的复数的IEEE浮点表示，比如：@racket[2.0+3.0i]或@racket[-inf.0+nan.0i]；一种特殊情况是，一个不精确的复数可以有一个精确的零实部和一个不精确的虚部。
      }

        ]}
]

@;{Inexact numbers print with a decimal point or exponent specifier, and
exact numbers print as integers and fractions.  The same conventions
apply for reading number constants, but @litchar{#e} or
@litchar{#i} can prefix a number to force its parsing as an exact
or inexact number. The prefixes @litchar{#b}, @litchar{#o}, and
@litchar{#x} specify binary, octal, and hexadecimal
interpretation of digits.}
对一个小数点或指数的说明符进行不精确数字打印，对整数和分数进行精确数字打印。用同样的约定申请读入数值常数，但@litchar{#e}或@litchar{#i}可以前缀数值以解析一个精确的或不精确的数值。前缀@litchar{#b}、@litchar{#o}和@litchar{#x}指定二进制、八进制和十六进制数值的解释。

@;{@refdetails/gory["parse-number"]{the syntax of numbers}}
@refdetails/gory["parse-number"]{数字的语法}

@examples[
0.5
(eval:alts @#,racketvalfont{#e0.5} 1/2)
(eval:alts @#,racketvalfont{#x03BB} #x03BB)
]

@;{Computations that involve an inexact number produce inexact results,
so that inexactness acts as a kind of taint on numbers. Beware,
however, that Racket offers no ``inexact booleans,'' so computations
that branch on the comparison of inexact numbers can nevertheless
produce exact results. The procedures @racket[exact->inexact] and
@racket[inexact->exact] convert between the two
types of numbers.}
计算涉及到精确的数值产生不精确的结果，这样的情况对数据造成一种污染。注意，然而，Racket没有提供"不精确的布尔值"，所以对不精确的数字的比较分支计算却能产生精确的结果。@racket[exact->inexact]和@racket[inexact->exact]程序在两种类型的数值之间转换。

@examples[
(/ 1 2)
(/ 1 2.0)
(if (= 3.0 2.999) 1 2)
(inexact->exact 0.1)
]

@;{Inexact results are also produced by procedures such as @racket[sqrt],
@racket[log], and @racket[sin] when an exact result would require
representing real numbers that are not rational. Racket can represent
only rational numbers and complex numbers with rational parts.}
当精确的结果需要表达实际的非有理数数值，不精确的结果也由像@racket[sqrt]、@racket[log]和@racket[sin]这样的程序产生。Racket只能代表有理数和有理数配对的复数。

@examples[
(code:line (sin 0)   (code:comment @#,t{@;{rational...}有理数...}))
(code:line (sin 1/2) (code:comment @#,t{@;{not rational...}非有理数...}))
]

@;{In terms of performance, computations with small integers are
typically the fastest, where ``small'' means that the number fits into
one bit less than the machine's word-sized representation for signed
numbers. Computation with very large exact integers or with
non-integer exact numbers can be much more expensive than computation
with inexact numbers.}
在性能方面，小整数的计算通常是最快的，其中“小”意味着这个数字比有符号数值的机器字长要小一点。具有非常大的精确整数或非整精确数的计算要比不精确数的计算代价要高昂得多。

@def+int[
(define (sigma f a b)
  (if (= a b)
      0
      (+ (f a) (sigma f (+ a 1) b))))

(time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
(time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
]

@;{The number categories @deftech{integer}, @deftech{rational},
@deftech{real} (always rational), and @deftech{complex} are defined in
the usual way, and are recognized by the procedures @racket[integer?],
@racket[rational?], @racket[real?], and @racket[complex?], in addition
to the generic @racket[number?]. A few mathematical procedures accept
only real numbers, but most implement standard extensions to complex
numbers.}
在针对通常的@racket[number?]的加法中，@deftech{整数类（integer）}、@deftech{有理数类（rational）}、@deftech{实类（real）}（总是有理数）和复数都以通常的方式定义，并被程序 @racket[integer?]、@racket[rational?]、@racket[real?]和@racket[complex?]所识别。一些数学过程只接受实数，但大多数实现了对复数的标准扩展。

@examples[
(integer? 5)
(complex? 5)
(integer? 5.0)
(integer? 1+2i)
(complex? 1+2i)
(complex? 1.0+2.0i)
(abs -5)
(abs -5+2i)
(sin -5+2i)
]

@;{The @racket[=] procedure compares numbers for numerical equality. If
it is given both inexact and exact numbers to compare, it essentially
converts the inexact numbers to exact before comparing. The
@racket[eqv?] (and therefore @racket[equal?]) procedure, in contrast,
compares numbers considering both exactness and numerical equality.}
@racket[=]过程比较数值相等的数值。如果给定不精确和精确的数字进行比较，它实际上会在比较之前将不精确数字转换为精确数字。@racket[eqv?]（乃至 @racket[equal?]）程序，相反，程序比较既是精确数而且数值上相等的数值。

@examples[
(= 1 1.0)
(eqv? 1 1.0)
]

@;{Beware of comparisons involving inexact numbers, which by their nature
can have surprising behavior. Even apparently simple inexact numbers
may not mean what you think they mean; for example, while a base-2
IEEE floating-point number can represent @racket[1/2] exactly, it
can only approximate @racket[1/10]:}
当心涉及不精确的数字比较，由于其性质会有令人惊讶的行为。即使是简单的不精确的数字也许并不意味着你能想到他们的意思；例如，当一个二进制IEEE浮点数可以表示为@racket[1/2]精确数，它只能近似于@racket[1/10]：

@examples[
(= 1/2 0.5)
(= 1/10 0.1)
(inexact->exact 0.1)
]

@;{@refdetails["numbers"]{numbers and number procedures}}
@refdetails["numbers"]{数字和数字程序}
