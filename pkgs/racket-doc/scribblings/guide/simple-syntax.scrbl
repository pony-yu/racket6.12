#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt")

@(define ex-eval (make-base-eval))

@;{@title[#:tag "syntax-overview"]{Simple Definitions and Expressions}}
@title[#:tag "syntax-overview"]{简单的定义与表达式}

@;{A program module is written as}
一个程序模块一般被写作

@racketblock[
@#,BNF-seq[@litchar{#lang} @nonterm{langname} @kleenestar{@nonterm{topform}}]
]

@;{where a @nonterm{topform} is either a @nonterm{definition} or an
@nonterm{expr}. The @tech{REPL} also evaluates @nonterm{topform}s.}
@nonterm{topform}既是一个@nonterm{definition}也是一个@nonterm{expr}。@tech{REPL}也对@nonterm{topform}求值。

@;{In syntax specifications, text with a gray background, such as
@litchar{#lang}, represents literal text. Whitespace must appear
between such literals and nonterminals like @nonterm{id},
except that whitespace is not required before or after @litchar{(},
@litchar{)}, @litchar{[}, or @litchar{]}.  A @index['("comments")]{comment}, which starts
with @litchar{;} and runs until the end of the line, is treated the
same as whitespace.}
在语法规范里，文本使用灰色背景，比如@litchar{#lang}，代表文本。文本与非结束符（像@nonterm{ID}）之间必须有空格，除了@litchar{(}、@litchar{)}及@litchar{[}、@litchar{]}之前或之后不需要空格。注释以@litchar{;}开始，直至这一行结束，空白也做相同处理。

@;{@refdetails["parse-comment"]{different forms of comments}}
@refdetails["parse-comment"]{注释的不同形式}

@;{Following the usual conventions, @kleenestar{} in a grammar means zero
or more repetitions of the preceding element, @kleeneplus{} means one
or more repetitions of the preceding element, and @BNF-group{} groups
a sequence as an element for repetition.}
后边遵从如下惯例：@kleenestar{}在程序中表示零个或多个前面元素的重复，@kleeneplus{}表示前一个或多个前面元素的重复，@BNF-group{} 组合一个序列作为一个元素的重复。

@(define val-defn-stx
   @BNF-seq[@litchar{(}@litchar{define} @nonterm{id} @nonterm{expr} @litchar{)}])
@(define fun-defn-stx
   @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
                  @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define fun-defn2-stx
   @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
            @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define app-expr-stx @BNF-seq[@litchar{(} @nonterm{id} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define app2-expr-stx @BNF-seq[@litchar{(} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define if-expr-stx @BNF-seq[@litchar{(} @litchar{if} @nonterm{expr} @nonterm{expr} @nonterm{expr} @litchar{)}])

@(define lambda-expr-stx @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
                                              @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define lambda2-expr-stx
   @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
            @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define and-expr-stx @BNF-seq[@litchar{(} @litchar{and} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define or-expr-stx @BNF-seq[@litchar{(} @litchar{or} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define cond-expr-stx @BNF-seq[@litchar{(} @litchar{cond}
                                @kleenestar{@BNF-group[@litchar{[} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{]}]}
                                @litchar{)}])
@(define (make-let-expr-stx kw)
   @BNF-seq[@litchar{(} kw @litchar{(}
            @kleenestar{@BNF-group[@litchar{[} @nonterm{id} @nonterm{expr} @litchar{]}]}
            @litchar{)}
            @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define let-expr-stx (make-let-expr-stx @litchar{let}))
@(define let*-expr-stx (make-let-expr-stx @litchar{let*}))

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Definitions}}
@section{定义}

@;{A definition of the form}
表的一个定义：

@moreguide["define"]{定义}

@racketblock[@#,val-defn-stx]

@;{binds @nonterm{id} to the result of @nonterm{expr}, while}
绑定@nonterm{id}到@nonterm{expr}的结果，而

@racketblock[@#,fun-defn-stx]

@;{binds the first @nonterm{id} to a function (also called a
@defterm{procedure}) that takes arguments as named by the remaining
@nonterm{id}s. In the function case, the @nonterm{expr}s are the body
of the function. When the function is called, it returns the result of
the last @nonterm{expr}.}
绑定第一个定@nonterm{id}到一个函数（也叫程序），以参数作为命名定@nonterm{id}，对函数的实例，该定@nonterm{expr}是函数的函数体。当函数被调用时，它返回最后一个定@nonterm{expr}的结果。

@defexamples[
#:eval ex-eval
(code:line (define pie 3)             (code:comment @#,t{定义@racket[pie]为@racket[3]}))
(code:line (define (piece str)        (code:comment @#,t{定义@racket[piece]为一个})
             (substring str 0 pie))   (code:comment @#,t{带一个参数的函数}))
pie
(piece "key lime")
]

@;{Under the hood, a function definition is really the same as a
non-function definition, and a function name does not have to be
used in a function call. A function is just another kind of value,
though the printed form is necessarily less complete than the printed
form of a number or string.}
在封装下，函数定义实际上与非函数定义相同，函数名不需要在函数调用中使用。函数只是另一种类型的值，尽管打印形式必须比数字或字符串的打印形式更不完整。

@examples[
#:eval ex-eval
piece
substring
]

@;{A function definition can include multiple expressions for the
function's body. In that case, only the value of the last expression
is returned when the function is called. The other expressions are
evaluated only for some side-effect, such as printing.}
一个函数定义可以包含函数体的多个表达式。在这种情况下，在调用函数时只返回最后一个表达式的值。其他表达式只对一些副作用进行求值，比如打印这些。

@defexamples[
#:eval ex-eval
(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))
(bake "apple")
]

@;{Racket programmers prefer to avoid side-effects, so a definition usually
has just one expression in its body. It's
important, though, to understand that multiple expressions are allowed
in a definition body, because it explains why the following
@racket[nobake] function fails to include its argument in its result:}
Racket程序员更喜欢避免副作用，所以一个定义通常只有一个表达式。这是重要的，但是，了解多个表达式在定义体内是被允许的，因为它解释了为什么以下@racket[nobake]函数未在其结果中包含它的参数：

@def+int[
#:eval ex-eval
(define (nobake flavor)
  string-append flavor "jello")
(nobake "green")
]

@;{Within @racket[nobake], there are no parentheses around
@racket[string-append flavor "jello"], so they are three separate
expressions instead of one function-call expression. The expressions
@racket[string-append] and @racket[flavor] are evaluated, but the
results are never used. Instead, the result of the function is just
the result of the final expression, @racket["jello"].}
在nobake中，没有括号包括string-append给"jello"，那么他们是三个单独的表达而不是函数调用表达式。string-append表达式和flavor被求值，但结果没有被使用。相反，该函数的结果是最终的表达式"jello"。

@; ----------------------------------------------------------------------
@;{@section[#:tag "indentation"]{An Aside on Indenting Code}}
@section[#:tag "indentation"]{代码缩进}

@;{Line breaks and indentation are not significant for parsing Racket
programs, but most Racket programmers use a standard set of conventions
to make code more readable. For example, the body of a definition is
typically indented under the first line of the definition. Identifiers
are written immediately after an open parenthesis with no extra space,
and closing parentheses never go on their own line.}
换行和缩进对于解析Racket程序来说并不重要，但大多数Racket程序员使用一套标准的约定来使代码更易读。例如，定义的主体通常在定义的第一行下缩进。标识符是在一个没有额外空格的括号内立即写出来的，而闭括号则从不自己独立一行。


@;{DrRacket automatically indents according to the standard style when
you type Enter in a program or @tech{REPL} expression. For example, if you
hit Enter after typing @litchar{(define (greet name)}, then DrRacket
automatically inserts two spaces for the next line.  If you change a
region of code, you can select it in DrRacket and hit Tab, and
DrRacket will re-indent the code (without inserting any line breaks).
Editors like Emacs offer a Racket or Scheme mode with similar indentation
support.}
DrRacket会根据标准风格自动缩进，当你输入一个程序或@tech{REPL}表达式。例如，如果你点击进入后输入 @litchar{(define (greet name)}，那么DrRacket自动为下一行插入两个空格。如果你改变了代码区域，你可以在DrRacket打Tab选择它，并且DrRacket将重新缩进代码（没有插入任何换行）。象Emacs这样的编辑器提供Racket或Scheme类似的缩进模式。

@;{Re-indenting not only makes the code easier to read, it gives you
extra feedback that your parentheses match in the way that you
intended. For example, if you leave out a closing parenthesis after
the last argument to a function, automatic indentation starts the
next line under the first argument, instead of under the
@racket[define] keyword:}
重新缩进不仅使代码更易于阅读，它还会以你希望的方式给你更多的反馈，象你的括号是否匹配等等。例如，如果在函数的最后一个参数之后省略一个结束括号，则自动缩进在第一个参数下开始下一行，而不是在@racket{define}关键字下：

@racketblock[
(define (halfbake flavor
                  (string-append flavor " creme brulee")))
]

@;{In this case, indentation helps highlight the mistake. In other cases,
where the indentation may be normal while an open parenthesis has no
matching close parenthesis, both @exec{racket} and DrRacket use the
source's indentation to suggest where a parenthesis might be missing.}
在这种情况下，缩进有助于突出错误。在其他情况下，在缩进可能是正常的，一个开括号没有匹配的闭括号，@exec{racket}和DrRacket都在源程序的缩进中提示括号丢失。

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Identifiers}}
@section{标识符}

@;{Racket's syntax for identifiers is especially liberal. Excluding the
special characters}
Racket的标识符语法特别自由。不含特殊字符。

@;{@moreguide["binding"]{identifiers}}
@moreguide["binding"]{标识符}

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar{|} @litchar{\}
}

@;{and except for the sequences of characters that make number constants,
almost any sequence of non-whitespace characters forms an
@nonterm{id}. For example @racketid[substring] is an
identifier. Also, @racketid[string-append] and @racketid[a+b] are
identifiers, as opposed to arithmetic expressions. Here are several
more examples:}
除了文字，使常数数字序列，几乎任何非空白字符序列形成一个@nonterm{ID}。例如，@racketid[substring]是一个标识符。另外，@racketid[string-append]和@racketid[a+b]是标识符，而不是算术表达式。这里还有几个例子：

@racketblock[
@#,racketid[+]
@#,racketid[Hfuhruhurr]
@#,racketid[integer?]
@#,racketid[pass/fail]
@#,racketid[john-jacob-jingleheimer-schmidt]
@#,racketid[a-b-c+1-2-3]
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Function Calls@aux-elem{ (Procedure Applications)}}}
@section{函数调用@aux-elem{(过程应用程序)}}
@;??????????????????????????????????????????????????????????????

@;{We have already seen many function calls, which are called
@defterm{procedure applications} in more traditional
terminology. The syntax of a function call is}
我们已经看到过许多函数调用，更传统的术语称之为@defterm{过程应用程序}。函数调用的语法是：

@;{@moreguide["application"]{function calls}}
@moreguide["application"]{函数调用}

@racketblock[
#,app-expr-stx
]

@;{where the number of @nonterm{expr}s determines the number of
arguments supplied to the function named by @nonterm{id}.}
在一些@nonterm{expr}的个数决定提供给由@nonterm{id}命名的函数的参数个数。

@;{The @racketmodname[racket] language pre-defines many function
identifiers, such as @racket[substring] and
@racket[string-append]. More examples are below.}
@racketmodname[racket]语言预定义了许多函数标识符，比如@racket[substring]和@racket[string-append]。下面有更多的例子。

@;{In example Racket code throughout the documentation, uses of
pre-defined names are hyperlinked to the reference manual. So, you can
click on an identifier to get full details about its use.}
Racket代码例子贯穿整个文档，预定义的名称的使用链接到参考手册。因此，你可以单击标识符来获得关于其使用的详细信息。

@interaction[
(code:line (string-append "rope" "twine" "yarn")  (code:comment @#,t{添加字符串}))
(code:line (substring "corduroys" 0 4)            (code:comment @#,t{提取子字符串}))
(code:line (string-length "shoelace")             (code:comment @#,t{获取字符串长度}))
(code:line (string? "Ceci n'est pas une string.") (code:comment @#,t{识别字符串}))
(string? 1)
(code:line (sqrt 16)                              (code:comment @#,t{找一个平方根}))
(sqrt -16)
(code:line (+ 1 2)                                (code:comment @#,t{数字相加}))
(code:line (- 2 1)                                (code:comment @#,t{数字相减}))
(code:line (< 2 1)                                (code:comment @#,t{数字比较}))
(>= 2 1)
(code:line (number? "c'est une number")           (code:comment @#,t{识别数字}))
(number? 1)
(code:line (equal? 6 "half dozen")                (code:comment @#,t{任意比较}))
(equal? 6 6)
(equal? "half dozen" "half dozen")
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Conditionals with @racket[if], @racket[and], @racket[or], and @racket[cond]}}
@section{条件表达式@racket[if]、@racket[and]、@racket[or]和@racket[cond]}

@;{The next simplest kind of expression is an @racket[if] conditional:}
以下是一个最简单的表达式是@racket[if]条件：

@racketblock[
#,if-expr-stx
]

@;{@moreguide["conditionals"]{conditionals}}
@moreguide["conditionals"]{条件句}

@;{The first @nonterm{expr} is always evaluated. If it produces a
non-@racket[#f] value, then the second @nonterm{expr} is
evaluated for the result of the whole @racket[if] expression, otherwise
the third @nonterm{expr} is evaluated for the result.}
第一个@nonterm{expr}总是被求值。如果它产生一个非@racket[#f]值，那么第二个@nonterm{expr}被求值并作为整个@racket[if]表达式的结果，否则第三个@nonterm{expr}被求值并作为结果。

@examples[
(if (> 2 3)
    "bigger"
    "smaller")
]

@def+int[
(define (reply s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      "huh?"))
(reply "hello racket")
(reply "\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx")
]

@;{Complex conditionals can be formed by nesting @racket[if]
expressions. For example, you could make the @racket[reply] function
work when given non-strings:}
复杂的条件可以由嵌套的@racket[if]表达式构成。例如，当给定非字符串时，可以使@racket[reply]函数工作：

@racketblock[
(define (reply s)
  (if (string? s)
      (if (equal? "hello" (substring s 0 5))
          "hi!"
          "huh?")
      "huh?"))
]

@;{Instead of duplicating the @racket["huh?"] case, this function is
better written as}
而不是复制@racket["huh?"]事例，这个函数这样写会更好：

@racketblock[
(define (reply s)
  (if (if (string? s)
          (equal? "hello" (substring s 0 5))
          #f)
      "hi!"
      "huh?"))
]

@;{but these kinds of nested @racket[if]s are difficult to read.  Racket
provides more readable shortcuts through the @racket[and] and
@racket[or] forms, which work with any number of expressions:}
是这些嵌套的@racket[if]很难读。Racket通过@racket[and]和@racket[or]表提供了更多的更易读的快捷表示，它可以和任意数量的表达式搭配：

@moreguide["and+or"]{@racket[and] and @racket[or]}

@racketblock[
#,and-expr-stx
#,or-expr-stx
]

@;{The @racket[and] form short-circuits: it stops and returns @racket[#f]
when an expression produces @racket[#f], otherwise it keeps
going. The @racket[or] form similarly short-circuits when it
encounters a true result.}
@racket[and]表中断情况：当表达式返回@racket[#f]，它停止并返回@racket[#f]，否则它会运行。@racket[or]表遇到一个真的结果时，同样的产生中断情况。

@defexamples[
(define (reply s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))
(reply "hello racket")
(reply 17)
]

@;{Another common pattern of nested @racket[if]s involves a sequence of
tests, each with its own result:}
嵌套@racket[if]的另一种常见模式是一个序列测试，每个测试都有自己的结果：

@racketblock[
(define (reply-more s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      (if (equal? "goodbye" (substring s 0 7))
          "bye!"
          (if (equal? "?" (substring s (- (string-length s) 1)))
              "I don't know"
              "huh?"))))
]

@;{The shorthand for a sequence of tests is the @racket[cond] form:}
对一个序列的测试的快捷形式是@racket[cond]表：

@moreguide["cond"]{@racket[cond]}

@racketblock[
#,cond-expr-stx
]

@;{A @racket[cond] form contains a sequence of clauses between square
brackets. In each clause, the first @nonterm{expr} is a test
expression. If it produces true, then the clause's remaining
@nonterm{expr}s are evaluated, and the last one in the clause provides
the answer for the entire @racket[cond] expression; the rest of the
clauses are ignored. If the test @nonterm{expr} produces @racket[#f],
then the clause's remaining @nonterm{expr}s are ignored, and
evaluation continues with the next clause. The last clause can use
@racket[else] as a synonym for a @racket[#t] test expression.}
一个@racket[cond]表包含了括号之间的一个序列的分句表。在每一个分句表，第一个@nonterm{expr}是测试表达式。如果它产生真值，那么剩下的 @nonterm{expr}从句表被求值，并在这些分从句的最后一个提供整个@racket[cond]表达结果，同时其余的从句表被忽略。如果测试 @nonterm{expr}产生@racket[#f]，那么从句表剩余的 @nonterm{expr}被忽视，并继续下一个从句表求值。最后一项分句表可以使用@racket[else]作为一个@racket[#t]测试表达式的相同意义。

@;{Using @racket[cond], the @racket[reply-more] function can be more
clearly written as follows:}
使用@racket[cond]，@racket[reply-more]函数可以更清楚地写成如下形式：

@def+int[
(define (reply-more s)
  (cond
   [(equal? "hello" (substring s 0 5))
    "hi!"]
   [(equal? "goodbye" (substring s 0 7))
    "bye!"]
   [(equal? "?" (substring s (- (string-length s) 1)))
    "I don't know"]
   [else "huh?"]))
(reply-more "hello racket")
(reply-more "goodbye cruel world")
(reply-more "what is your favorite color?")
(reply-more "mine is lime green")
]

@;{The use of square brackets for @racket[cond] clauses is a
convention. In Racket, parentheses and square brackets are actually
interchangeable, as long as @litchar{(} is matched with @litchar{)} and
@litchar{[} is matched with @litchar{]}. Using square brackets in a
few key places makes Racket code even more readable.}
对于@racket[cond]从句表的方括号的使用是一种惯例。在Racket中，圆括号和方括号实际上是可互换的，只要@litchar{(}匹配@litchar{)}或@litchar{[}匹配@litchar{]}即可。在一些关键的地方使用方括号可以使Racket代码更易读。

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Function Calls, Again}}
@section{函数重复调用}

@;{In our earlier grammar of function calls, we oversimplified.  The
actual syntax of a function call allows an arbitrary
expression for the function, instead of just an @nonterm{id}:}
在我们早期的函数语法调用，我们是过分简单化的。一个函数调用的语法允许任意的函数表达式，而不是一个@nonterm{ID}：

@;{@moreguide["application"]{function calls}}
@moreguide["application"]{函数调用}

@racketblock[
#,app2-expr-stx
]

@;{The first @nonterm{expr} is often an @nonterm{id}, such
as @racket[string-append] or @racket[+], but it can be anything that
evaluates to a function. For example, it can be a conditional
expression:}
第一个@nonterm{expr}常常是一个@nonterm{id}，比如@racket[string-append]或@racket[+]，但它可以是对一个函数的求值的任意情况。例如，它可以是一个条件表达式：

@def+int[
(define (double v)
  ((if (string? v) string-append +) v v))
(double "mnah")
(double 5)
]

@;{Syntactically, the first expression in a function call could
even be a number---but that leads to an error, since a number is not a
function.}
在语法上，在一个函数调用的第一个表达甚至可以是一个数——但那会导致一个错误，因为一个数不是一个函数。

@interaction[(1 2 3 4)]

@;{When you accidentally omit a function name or when you use
extra parentheses around an expression, you'll most often get an ``expected
a procedure'' error like this one.}
当您意外地忽略函数名或在表达式中使用额外的括号时，你通常会得到像这样“expected a procedure”的错误。

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Anonymous Functions with @racket[lambda]}}
@section{匿名函数与@racket[lambda]}

@;{Programming in Racket would be tedious if you had to name all of your
numbers. Instead of writing @racket[(+ 1 2)], you'd have to write}
如果你必须命名你所有的数值，那Racket的编程就太乏味了。替换@racket[(+ 1 2)]的写法，你得这样写：

@;{@moreguide["lambda"]{@racket[lambda]}}
@moreguide["lambda"]{@racket[lambda]}

@interaction[
(define a 1)
(define b 2)
(+ a b)
]

@;{It turns out that having to name all your functions can be tedious,
too. For example, you might have a function @racket[twice] that takes
a function and an argument. Using @racket[twice] is convenient if you
already have a name for the function, such as @racket[sqrt]:}
事实证明，要命名所有函数也可能很乏味。例如，您可能有个函数 @racket[twice]，带了一个函数和一个参数。如果你已经有了函数的名字那么使用 @racket[twice]是比较方便的，如@racket[sqrt]：

@def+int[
#:eval ex-eval
(define (twice f v)
  (f (f v)))
(twice sqrt 16)
]

@;{If you want to call a function that is not yet defined, you could
define it, and then pass it to @racket[twice]:}
如果您想调用尚未定义的函数，您可以定义它，然后将其传递给@racket[twice]：

@def+int[
#:eval ex-eval
(define (louder s)
  (string-append s "!"))
(twice louder "hello")
]

@;{But if the call to @racket[twice] is the only place where
@racket[louder] is used, it's a shame to have to write a whole
definition. In Racket, you can use a @racket[lambda] expression to
produce a function directly. The @racket[lambda] form is followed by
identifiers for the function's arguments, and then the function's
body expressions:}
但是如果对 @racket[twice]的调用是唯一使用 @racket[louder]的地方，却还要写一个完整的定义是很可惜的。在Racket中，可以使用 @racket[lambda]表达式直接生成函数。 @racket[lambda]表后面是函数参数的标识符，然后是函数的体表达式：

@racketblock[
#,lambda-expr-stx
]

@;{Evaluating a @racket[lambda] form by itself produces a function:}
求值@racket[lambda]表本身即产生一个函数：

@interaction[(lambda (s) (string-append s "!"))]

@;{Using @racket[lambda], the above call to @racket[twice] can be
re-written as}
使用@racket[lambda]，上述对@racket[twice]的调用可以重写为：

@interaction[
#:eval ex-eval
(twice (lambda (s) (string-append s "!"))
       "hello")
(twice (lambda (s) (string-append s "?!"))
       "hello")
]

@;{Another use of @racket[lambda] is as a result for a function that
generates functions:}
@racket[lambda]的另一个用途是作为生成函数的函数结果：

@def+int[
#:eval ex-eval
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
(twice (make-add-suffix "!") "hello")
(twice (make-add-suffix "?!") "hello")
(twice (make-add-suffix "...") "hello")
]

@;{Racket is a @defterm{lexically scoped} language, which means that
@racket[s2] in the function returned by @racket[make-add-suffix]
always refers to the argument for the call that created the
function. In other words, the @racket[lambda]-generated function
``remembers'' the right @racket[s2]:}
Racket是一个词法作用域（@defterm{lexically scoped}）的语言，这意味着函数中的@racket[s2]通过@racket[make-add-suffix]总是指创建该函数调用的参数返回。换句话说，@racket[lambda]生成的函数“记住”了右边的@racket[s2]：

@interaction[
#:eval ex-eval
(define louder (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))
(twice less-sure "really")
(twice louder "really")
]

@;{We have so far referred to definitions of the form @racket[(define
@#,nonterm{id} @#,nonterm{expr})] as ``non-function
definitions.'' This characterization is misleading, because the
@nonterm{expr} could be a @racket[lambda] form, in which case
the definition is equivalent to using the ``function'' definition
form. For example, the following two definitions of @racket[louder]
are equivalent:}
到目前为止我们已经提到了表@racket[(define
@#,nonterm{id} @#,nonterm{expr})]的定义。作为“非函数的定义“。“这种表征是误导性的，因为@nonterm{expr}可以是一个@racket[lambda]表，在这种情况下，定义是等效于使用“函数（function）”的定义形式。例如，下面两个@racket[louder]的定义是等价的：

@defs+int[
#:eval ex-eval
[(define (louder s)
   (string-append s "!"))
 code:blank
 (define louder
   (lambda (s)
     (string-append s "!")))]
louder
]

@;{Note that the expression for @racket[louder] in the second case is an
``anonymous'' function written with @racket[lambda], but, if
possible, the compiler infers a name, anyway, to make printing and
error reporting as informative as possible.}
注意，第二种情况下@racket[louder]表达式是用@racket[lambda]写成的“匿名（anonymous）”函数，但如果可能的话，编译器推断出一个名称，无论如何，使打印和错误报告尽可能地有信息。

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section[#:tag "local-binding-intro"]{Local Binding with
         @racket[define], @racket[let], and @racket[let*]}}
@section[#:tag "local-binding-intro"]{用@racket[define]、@racket[let]和@racket[let*]实现局部绑定}

@;{It's time to retract another simplification in our grammar of
Racket. In the body of a function, definitions can appear before the
body expressions:}
现在是收起我们的Racket语法的另一个简化的时候了。在函数体中，定义可以出现在函数体表达式之前：

@;{@moreguide["intdefs"]{local (internal) definitions}}
@moreguide["intdefs"]{局部（内部）定义}

@racketblock[
#,fun-defn2-stx
#,lambda2-expr-stx
]

@;{Definitions at the start of a function body are local to the
function body.}
函数主体开始时的定义在函数体中是局部的。

@defexamples[
(define (converse s)
  (define (starts? s2) (code:comment @#,t{local to @racket[converse]})
    (define len2 (string-length s2))  (code:comment @#,t{local to @racket[starts?]})
    (and (>= (string-length s) len2)
         (equal? s2 (substring s 0 len2))))
  (cond
   [(starts? "hello") "hi!"]
   [(starts? "goodbye") "bye!"]
   [else "huh?"]))
(converse "hello!")
(converse "urp")
(eval:alts (code:line starts? (code:comment @#,t{outside of @racket[converse], so...}))
           (parameterize ([current-namespace (make-base-namespace)]) (eval 'starts?)))
]

@;{Another way to create local bindings is the @racket[let] form. An
advantage of @racket[let] is that it can be used in any expression
position. Also, @racket[let] binds many identifiers at once, instead
of requiring a separate @racket[define] for each identifier.}
创建本地绑定的另一种方法是@racket[let]表。@racket[let]的一个优点是它可以在任何表达式位置使用。另外，@racket[let]同时绑定多个标识符，而不是每个标识符都需要一个单独@racket[define]。

@moreguide["intdefs"]{@racket[let] and @racket[let*]}

@racketblock[
#,let-expr-stx
]

@;{Each binding clause is an @nonterm{id} and an
@nonterm{expr} surrounded by square brackets, and the
expressions after the clauses are the body of the @racket[let]. In
each clause, the @nonterm{id} is bound to the result of the
@nonterm{expr} for use in the body.}
每个约束条款是一个@nonterm{id}和@nonterm{expr}方括号包围，和之后的从句表达的@racket[let]函数体。在每一个条款，该@nonterm{id}势必对应与函数体的@nonterm{expr}结果。

@interaction[
(let ([x (random 4)]
      [o (random 4)])
  (cond
   [(> x o) "X wins"]
   [(> o x) "O wins"]
   [else "cat's game"]))
]

@;{The bindings of a @racket[let] form are available only in the body of
the @racket[let], so the binding clauses cannot refer to each
other. The @racket[let*] form, in contrast, allows later clauses to
use earlier bindings:}
@racket[let]表的绑定仅在@racket[let]的函数体中可用，因此绑定子句不能互相引用。相反，@racket[let*]表允许后面的子句使用前面的绑定：

@interaction[
(let* ([x (random 4)]
       [o (random 4)]
       [diff (number->string (abs (- x o)))])
  (cond
   [(> x o) (string-append "X wins by " diff)]
   [(> o x) (string-append "O wins by " diff)]
   [else "cat's game"]))
]

@; ----------------------------------------------------------------------

@close-eval[ex-eval]
