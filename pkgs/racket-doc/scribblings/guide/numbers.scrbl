#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "numbers"]{Numbers}}
@title[#:tag "numbers"]{数值（Number）}

@;{A Racket @deftech{number} is either exact or inexact:}
一个Racket的@deftech{数值（number）}既可以是精确的也可以是不精确的：

@itemize[

 @item{
  @;{An @defterm{exact} number is either}
    一个@defterm{精确}的数值是：

       @itemize[

       @item{
    @;{an arbitrarily large or small integer, such as @racket[5],
             @racket[99999999999999999], or @racket[-17];}
      一个任意大的或任意小的整数，比如：@racket[5]，@racket[99999999999999999]或@racket[-17]；
   }

       @item{
    @;{a rational that is exactly the ratio of two arbitrarily
             small or large integers, such as @racket[1/2],
             @racket[99999999999999999/2], or @racket[-3/4]; or}
一个有理数，它是精确的两个任意小的或任意大的整数比，比如：@racket[1/2]，@racket[99999999999999999/2]或@racket[-3/4]；
      }

       @item{
    @;{a complex number with exact real and imaginary parts
             (where the imaginary part is not zero), such as @racket[1+2i] or
             @racket[1/2+3/4i].}
      一个带有精确的实部和虚部（即虚部不为零）的复数，比如：@racket[1+2i]或@racket[1/2+3/4i]。
   }

       ]}

 @item{
  @;{An @defterm{inexact} number is either}
    一个 @defterm{不精确}的数值是：

       @itemize[

        @item{
    @;{an IEEE floating-point representation of a number, such
              as @racket[2.0] or @racket[3.14e87], where the IEEE
              infinities and not-a-number are written
              @racket[+inf.0], @racket[-inf.0], and @racket[+nan.0]
              (or @racketvalfont{-nan.0}); or}
一个数值的一个IEEE浮点表示，比如：@racket[2.0]或@racket[3.14e87]，其中IEEE无穷大和一个非数值编写为：@racket[+inf.0]，@racket[-inf.0]和@racket[+nan.0]（或@racketvalfont{-nan.0}）；
      }

        @item{
    @;{a complex number with real and imaginary parts that are
              IEEE floating-point representations, such as
              @racket[2.0+3.0i] or @racket[-inf.0+nan.0i]; as a
              special case, an inexact complex number can have an
              exact zero real part with an inexact imaginary part.}
一个带有IEEE浮点表示的实部和虚部的复数，比如：@racket[2.0+3.0i]或@racket[-inf.0+nan.0i]；作为一种特例，一个带有一个不精确的虚部的不精确的复数可以有一个精确的零实部。
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
带有一个小数点或指数说明符的不精确数字打印，以及作为整数和分数的精确数字打印。同样的的惯例申请读取数值常量，但@litchar{#e}或@litchar{#i}能够前缀一个数值以强制其解析为一个精确的或不精确的数值。前缀@litchar{#b}、@litchar{#o}和@litchar{#x}指定二进制、八进制和十六进制数的解释。

@;{@refdetails/gory["parse-number"]{the syntax of numbers}}
@margin-note{《The Racket Reference（Racket参考）》文档（4.2 Numbers（数值））有数值的语法的细微之处。}

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
包含一个精确数值的计算产生不精确的结果，以致不精确充当了一种数值方面的污染。注意，然而，Racket没有提供"不精确的布尔值"，所以对不精确的数字的比较分支计算却仍然能产生精确的结果。过程@racket[exact->inexact]和@racket[inexact->exact]在两种数值类型之间转换。

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
当精确的结果需要作为非有理数实数时，不精确的结果也由像@racket[sqrt]、@racket[log]和@racket[sin]这样的过程产生。Racket仅能表示有理数和带有理数部分的复数。

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
在性能而言，带小整数的计算通常是最快的，其中“小”意味着这个合二为一的数值小于有符号数值的机器字长。具有非常大的精确整数或具有非整精确数的计算会比不精确数的计算代价要高昂得多。

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
数值类别@deftech{整数（integer）}、@deftech{有理数（rational）}、@deftech{实数（real）}（总是有理数）以及@deftech{复数（complex）}用通常的方法定义，并被过程@racket[integer?]、@racket[rational?]、@racket[real?]以及@racket[complex?]所验证。一些数学过程只接受实数，但大多数实现了对复数的标准扩展。

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
@racket[=]过程为了数值相等而比较数值。如果给定不精确数和精确数去作比较，它在比较之前从本质上将不精确数转换为精确数。相反，@racket[eqv?]（乃至 @racket[equal?]）过程比较数值既考虑精确性又考虑数值的相等。

@examples[
(= 1 1.0)
(eqv? 1 1.0)
]

@;{Beware of comparisons involving inexact numbers, which by their nature
can have surprising behavior. Even apparently simple inexact numbers
may not mean what you think they mean; for example, while a base-2
IEEE floating-point number can represent @racket[1/2] exactly, it
can only approximate @racket[1/10]:}
当心涉及不精确数的比较，由于其天性会有出人意料的行为。甚至实际上简单的不精确数也许并不意味着你能想到的和他们的意义一致；例如，当一个二进制IEEE浮点数可以精确地表示为@racket[1/2]时，它可能近似于@racket[1/10]：

@examples[
(= 1/2 0.5)
(= 1/10 0.1)
(inexact->exact 0.1)
]

@;{@refdetails["numbers"]{numbers and number procedures}}
@margin-note{《The Racket Reference（Racket参考）》文档（4.2 Numbers（数值））有关于数值和数值过程的更多内容。}
