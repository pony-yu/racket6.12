#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define greet-eval (make-base-eval))

@;{@title[#:tag "lambda"]{Functions@aux-elem{ (Procedures)}: @racket[lambda]}}
@title[#:tag "lambda"]{lambda函数（程序）}

@;{A @racket[lambda] expression creates a function. In the simplest
case, a @racket[lambda] expression has the form}
一个@racket[lambda]表达式创建函数。在最简单的情况，@racket[lambda]表达式具有的形式：

@specform[
(lambda (arg-id ...)
  body ...+)
]

@;{A @racket[lambda] form with @math{n} @racket[_arg-id]s accepts
@math{n} arguments:}
一个具有@math{n}个@racket[_arg-id]的@racket[lambda]表接受@math{n}个参数：

@interaction[
((lambda (x) x)
 1)
((lambda (x y) (+ x y))
 1 2)
((lambda (x y) (+ x y))
 1)
]

@;------------------------------------------------------------------------
@;{@section[#:tag "rest-args"]{Declaring a Rest Argument}}
@section[#:tag "rest-args"]{申明剩余（rest）参数}

@;{A @racket[lambda] expression can also have the form}
一个@racket[lambda]表达式也可以有这种形式：

@specform[
(lambda rest-id
  body ...+)
]

@;{That is, a @racket[lambda] expression can have a single
@racket[_rest-id] that is not surrounded by parentheses. The resulting
function accepts any number of arguments, and the arguments are put
into a list bound to @racket[_rest-id].}
也就是说，@racket[lambda]表达式可以有一个没有被圆括号包围的单个@racket[_rest-id]。所得到的函数接受任意数目的参数，并且这个参数放入一个绑定到@racket[_rest-id]的列表：

@examples[
((lambda x x)
 1 2 3)
((lambda x x))
((lambda x (car x))
 1 2 3)
]

@;{Functions with a @racket[_rest-id] often use @racket[apply] to call
another function that accepts any number of arguments.}
带有一个@racket[_rest-id]的函数经常使用@racket[apply]函数调用另一个函数，它接受任意数量的参数。

@;{@guideother{@secref["apply"] describes @racket[apply].}}
@guideother{@secref["apply"]描述@racket[apply]。}

@defexamples[
(define max-mag
  (lambda nums
    (apply max (map magnitude nums))))

(max 1 -2 0)
(max-mag 1 -2 0)
]

@;{The @racket[lambda] form also supports required arguments combined
with a @racket[_rest-id]:}
@racket[lambda]表还支持必需参数与@racket[_rest-id]相结合：

@specform[
(lambda (arg-id ...+ . rest-id)
  body ...+)
]

@;{The result of this form is a function that requires at least as many
arguments as @racket[_arg-id]s, and also accepts any number of
additional arguments.}
这个表的结果是一个函数，它至少需要与@racket[_arg-id]一样多的参数，并且还接受任意数量的附加参数。

@defexamples[
(define max-mag
  (lambda (num . nums)
    (apply max (map magnitude (cons num nums)))))

(max-mag 1 -2 0)
(max-mag)
]

@;{A @racket[_rest-id] variable is sometimes called a @deftech{rest
argument}, because it accepts the ``rest'' of the function arguments.}
一个@racket[_rest-id]变量有时称为@deftech{rest参数（rest
argument）}，因为它接受函数参数的“rest”。

@;------------------------------------------------------------------------
@;{@section{Declaring Optional Arguments}}
@section[#:tag "Declaring-Optional-Arguments"]{声明可选（optional）参数}

@;{Instead of just an identifier, an argument (other than a rest
argument) in a @racket[lambda] form can be specified with an
identifier and a default value:}
不只是标识符，一个@racket[lambda]表的参数（不仅是剩余参数）可以用标识符和缺省值指定：

@specform/subs[
(lambda gen-formals
  body ...+)
([gen-formals (arg ...)
              rest-id
              (arg ...+ . rest-id)]
 [arg arg-id
      [arg-id default-expr]])
]{}

@;{An argument of the form @racket[[arg-id default-expr]] is
optional. When the argument is not supplied in an application,
@racket[_default-expr] produces the default value. The
@racket[_default-expr] can refer to any preceding @racket[_arg-id],
and every following @racket[_arg-id] must have a default as well.}
表的参数@racket[[arg-id default-expr]]是可选的。当参数不在应用程序中提供，@racket[_default-expr]产生默认值。@racket[_default-expr]可以引用任何前面的@racket[_arg-id]，并且下面的每个@racket[_arg-id]也必须应该有一个默认值。

@defexamples[
(define greet
  (lambda (given [surname "Smith"])
    (string-append "Hello, " given " " surname)))

(greet "John")
(greet "John" "Doe")
]

@def+int[
(define greet
  (lambda (given [surname (if (equal? given "John")
                              "Doe"
                              "Smith")])
    (string-append "Hello, " given " " surname)))

(greet "John")
(greet "Adam")
]

@;{@section[#:tag "lambda-keywords"]{Declaring Keyword Arguments}}
@section[#:tag "lambda-keywords"]{声明关键字（keyword）参数}

@;{A @racket[lambda] form can declare an argument to be passed by
keyword, instead of position. Keyword arguments can be mixed with
by-position arguments, and default-value expressions can be supplied
for either kind of argument:}
一个@racket[lambda]表可以声明要通过关键字传递的参数，而不是位置。关键字参数可以与位置参数混合，也可以为两种参数提供默认值表达式：

@;{@guideother{@secref["keyword-args"] introduces function
calls with keywords.}}
@guideother{@secref["keyword-args"]用关键字介绍函数调用。}

@specform/subs[
(lambda gen-formals
  body ...+)
([gen-formals (arg ...)
              rest-id
              (arg ...+ . rest-id)]
 [arg arg-id
      [arg-id default-expr]
      (code:line arg-keyword arg-id)
      (code:line arg-keyword [arg-id default-expr])])
]{}

@;{An argument specified as @racket[(code:line _arg-keyword _arg-id)] is
supplied by an application using the same @racket[_arg-keyword].  The
position of the keyword--identifier pair in the argument list does not
matter for matching with arguments in an application, because it will
be matched to an argument value by keyword instead of by position.}
由一个应用程序使用同一个@racket[(code:line _arg-keyword _arg-id)]关键字提供一个参数，该参数指定为@racket[_arg-keyword]。在参数列表中关键字标识符对的位置与应用程序中的参数匹配并不重要，因为它将通过关键字而不是位置与参数值匹配。

@def+int[
(define greet
  (lambda (given #:last surname)
    (string-append "Hello, " given " " surname)))

(greet "John" #:last "Smith")
(greet #:last "Doe" "John")
]

@;{An @racket[(code:line _arg-keyword [_arg-id _default-expr])] argument
specifies a keyword-based argument with a default value.}
一个@racket[(code:line _arg-keyword [_arg-id _default-expr])]参数指定一个带默认值的关键字参数。

@defexamples[
#:eval greet-eval
(define greet
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))

(greet "John")
(greet "Karl" #:last "Marx")
(greet "John" #:hi "Howdy")
(greet "Karl" #:last "Marx" #:hi "Guten Tag")
]

@;{The @racket[lambda] form does not directly support the creation
of a function that accepts ``rest'' keywords. To construct a
function that accepts all keyword arguments, use
@racket[make-keyword-procedure]. The function supplied to
@racket[make-keyword-procedure] receives keyword arguments
through parallel lists in the first two (by-position) arguments,
and then all by-position arguments from an application as the
remaining by-position arguments.}
@racket[lambda]表不支持创建一个接受“rest”关键字的函数。要构造一个接受所有关键字参数的函数，请使用@racket[make-keyword-procedure]函数。这个函数支持@racket[make-keyword-procedure]通过前两个（位置）参数中的并行列表接受关键字参数，然后由应用程序的所有位置参数作为剩余位置参数。

@;{@guideother{@secref["apply"] introduces @racket[keyword-apply].}}
@guideother{@secref["apply"]介绍了@racket[keyword-apply]。}

@defexamples[
#:eval greet-eval
(define (trace-wrap f)
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (printf "Called with ~s ~s ~s\n" kws kw-args rest)
     (keyword-apply f kws kw-args rest))))
((trace-wrap greet) "John" #:hi "Howdy")
]

@;{@refdetails["lambda"]{function expressions}}
@refdetails["lambda"]{函数表达式}

@;------------------------------------------------------------------------
@;{@section[#:tag "case-lambda"]{Arity-Sensitive Functions: @racket[case-lambda]}}
@section[#:tag "case-lambda"]{多解函数：case-lambda}

@;{The @racket[case-lambda] form creates a function that can have
completely different behaviors depending on the number of arguments
that are supplied. A case-lambda expression has the form}
@racket[case-lambda]表创建一个函数，该函数可以根据所提供的参数的数量具有完全不同的行为。一个case-lambda表达式有以下形式：

@specform/subs[
(case-lambda
  [formals body ...+]
  ...)
([formals (arg-id ...)
          rest-id
          (arg-id ...+ . rest-id)])
]

@;{where each @racket[[_formals _body ...+]] is analogous to @racket[(lambda
_formals _body ...+)]. Applying a function produced by
@racket[case-lambda] is like applying a @racket[lambda] for the first
case that matches the number of given arguments.}
每个@racket[[_formals _body ...+]]类似于@racket[(lambda
_formals _body ...+)]。通过@racket[case-lambda]应用函数生成类似于应用一个@racket[lambda]匹配给定参数数量的第一种情况。

@defexamples[
(define greet
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]))

(greet "John")
(greet "John" "Smith")
(greet)
]

@;{A @racket[case-lambda] function cannot directly support optional or
keyword arguments.}
一个@racket[case-lambda]函数不能直接支持可选参数或关键字参数。

@; ----------------------------------------------------------------------

@close-eval[greet-eval]
