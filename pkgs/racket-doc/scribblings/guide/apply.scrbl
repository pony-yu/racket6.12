#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "application"]{Function Calls@aux-elem{ (Procedure Applications)}}}
@title[#:tag "application"]{函数调用@aux-elem{（过程程序）}}

@;{An expression of the form}
表的一个表达式：

@specsubform[
(proc-expr arg-expr ...)
]

@;{is a function call---also known as a @defterm{procedure
application}---when @racket[_proc-expr] is not an identifier that is
bound as a syntax transformer (such as @racket[if] or
@racket[define]).}
是一个函数调用——也被称为一个@defterm{应用程序（procedure
application）}——@racket[_proc-expr]不是标识符，而是作为一个语法翻译器（如@racket[if]或@racket[define]）。

@;{@section{Evaluation Order and Arity}}
@section[#:tag "Evaluation-Order-and-Arity"]{求值顺序和元数}

@;{A function call is evaluated by first evaluating the
@racket[_proc-expr] and all @racket[_arg-expr]s in order (left to
right). Then, if @racket[_proc-expr] produces a function that accepts
as many arguments as supplied @racket[_arg-expr]s, the function is
called. Otherwise, an exception is raised.}
一个函数调用求值是首先求值@racket[_proc-expr]和为所有@racket[_arg-expr]（由左至右）。然后，如果@racket[_arg-expr]产生一个函数接受@racket[_arg-expr]提供的所有参数，这个函数被调用。否则，将引发异常。

@examples[
(cons 1 null)
(+ 1 2 3)
(cons 1 2 3)
(1 2 3)
]

@;{Some functions, such as @racket[cons], accept a fixed number of
arguments. Some functions, such as @racket[+] or @racket[list], accept
any number of arguments. Some functions accept a range of argument
counts; for example @racket[substring] accepts either two or three
arguments. A function's @idefterm{arity} is the number of arguments
that it accepts.}
某些函数，如@racket[cons]，接受固定数量的参数。某些函数，如@racket[+]或@racket[list]，接受任意数量的参数。一些函数接受一系列参数计数；例如@racket[substring]接受两个或三个参数。一个函数的元数@idefterm{arity}是它接受参数的数量。

@;------------------------------------------------------------------------
@;{@section[#:tag "keyword-args"]{Keyword Arguments}}
@section[#:tag "keyword-args"]{关键字参数}

@;{Some functions accept @defterm{keyword arguments} in addition to
by-position arguments. For that case, an @racket[_arg] can be an
@racket[_arg-keyword _arg-expr] sequence instead of just a
@racket[_arg-expr]:}
除了通过位置参数外，有些函数接受@defterm{关键字参数（keyword arguments）}。因此，@racket[_arg]可以是一个@racket[_arg-keyword _arg-expr]序列而不只是一个@racket[_arg-expr]：

@;{@guideother{@secref["keywords"] introduces keywords.}}
@guideother{@secref["keywords"]介绍了关键词。}

@specform/subs[
(_proc-expr arg ...)
([arg arg-expr
      (code:line arg-keyword arg-expr)])
]

@;{For example,}
例如：

@racketblock[(go "super.rkt" #:mode 'fast)]

@;{calls the function bound to @racket[go] with @racket["super.rkt"] as a
by-position argument, and with @racket['fast] as an argument
associated with the @racket[#:mode] keyword. A keyword is implicitly
paired with the expression that follows it.}
用@racket["super.rkt"]调用函数绑定到 @racket[go] 作为位置参数，并用@racket['fast]通过@racket[#:mode]关键字作为相关参数。关键字隐式地与后面的表达式配对。

@;{Since a keyword by itself is not an expression, then}
既然关键字本身不是一个表达式，那么

@racketblock[(go "super.rkt" #:mode #:fast)]

@;{is a syntax error. The @racket[#:mode] keyword must be followed by an
expression to produce an argument value, and @racket[#:fast] is not an
expression.}
就是语法错误。@racket[#:mode]关键字必须跟着一个表达式以产生一个参数值，并@racket[#:fast]不是一个表达式。

@;{The order of keyword @racket[_arg]s determines the order in which
@racket[_arg-expr]s are evaluated, but a function accepts keyword
arguments independent of their position in the argument list. The
above call to @racket[go] can be equivalently written}
关键字@racket[_arg]的顺序决定@racket[_arg-expr]的求值顺序，而一个函数接受关键字参数与在参数列表中的位置无关。上面对@racket[go]的调用可以等价地写为：

@racketblock[(go #:mode 'fast "super.rkt")]

@;{@refdetails["application"]{procedure applications}}
@refdetails["application"]{程序中的应用}

@;------------------------------------------------------------------------
@;{@section[#:tag "apply"]{The @racket[apply] Function}}
@section[#:tag "apply"]{@racket[apply]函数}

@;{The syntax for function calls supports any number of arguments, but a
specific call always specifies a fixed number of arguments. As a
result, a function that takes a list of arguments cannot directly
apply a function like @racket[+] to all of the items in a list:}
函数调用的语法支持任意数量的参数，但是一个特定的调用总是指定一个固定数量的参数。因此，一个带参数列表的函数不能直接将一个类似于@racket[+]的函数应用到列表中的所有项中：

@def+int[
(define (avg lst) (code:comment @#,elem{doesn't work...})
  (/ (+ lst) (length lst)))
(avg '(1 2 3))
]

@def+int[
(define (avg lst) (code:comment @#,elem{doesn't always work...})
  (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
     (length lst)))
(avg '(1 2 3))
(avg '(1 2))
]

@;{The @racket[apply] function offers a way around this restriction. It
takes a function and a @italic{list} argument, and it applies the
function to the values in the list:}
@racket[apply]函数提供了一种绕过这种限制的方法。它使用一个函数和一个@italic{list}参数，并将函数应用到列表中的值：

@def+int[
(define (avg lst)
  (/ (apply + lst) (length lst)))
(avg '(1 2 3))
(avg '(1 2))
(avg '(1 2 3 4))
]

@;{As a convenience, the @racket[apply] function accepts additional
arguments between the function and the list. The additional arguments
are effectively @racket[cons]ed onto the argument list:}
为方便起见，@racket[apply]函数接受函数和列表之间的附加参数。额外的参数被有效地加入参数列表：

@def+int[
(define (anti-sum lst)
  (apply - 0 lst))
(anti-sum '(1 2 3))
]

@;{The @racket[apply] function accepts keyword arguments, too, and it
passes them along to the called function:}
@racket[apply]函数也接受关键字参数，并将其传递给调用函数：

@racketblock[
(apply go #:mode 'fast '("super.rkt"))
(apply go '("super.rkt") #:mode 'fast)
]

@;{Keywords that are included in @racket[apply]'s list argument do not
count as keyword arguments for the called function; instead, all arguments in
this list are treated as by-position arguments. To pass a list of
keyword arguments to a function, use
the @racket[keyword-apply] function, which accepts a function to apply
and three lists. The first two lists are in parallel, where the first
list contains keywords (sorted by @racket[keyword<?]), and the second
list contains a corresponding argument for each keyword. The third
list contains by-position function arguments, as for @racket[apply].}
包含在@racket[apply]的列表参数中的关键字不算作调用函数的关键字参数；相反，这个列表中的所有参数都被位置参数处理。要将一个关键字参数列表传递给函数，使用@racket[keyword-apply]函数，它接受一个要应用的函数和三个列表。前两个列表是平行的，其中第一个列表包含关键字（按@racket[keyword<?]排序），第二个列表包含每个关键字的对应参数。第三个列表包含位置函数参数，就像@racket[apply]。

@racketblock[
(keyword-apply go
               '(#:mode)
               '(fast)
               '("super.rkt"))
]
