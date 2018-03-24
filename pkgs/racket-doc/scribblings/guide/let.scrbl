#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "let"]{Local Binding}}
@title[#:tag "let"]{局部绑定}

@;{Although internal @racket[define]s can be used for local binding,
Racket provides three forms that give the programmer more
control over bindings: @racket[let], @racket[let*], and
@racket[letrec].}
虽然内部@racket[define]可用于局部绑定，Racket提供了三种形式给予程序员在绑定方面的更多控制：@racket[let]、@racket[let*]和@racket[letrec]。

@;------------------------------------------------------------------------
@;{@section{Parallel Binding: @racket[let]}}
@section[#:tag "parallel-binding-let"]{平行绑定：let}

@refalso["let"]{@racket[let]}

@;{A @racket[let] form binds a set of identifiers, each to the result of
some expression, for use in the @racket[let] body:}
一个@racket[let]表绑定一组标识符，每个标识符都是某个表达式的结果，用于@racket[let]主体：

@specform[(let ([id expr] ...) body ...+)]{}

@;{The @racket[_id]s are bound ``in parallel.'' That is, no @racket[_id]
is bound in the right-hand side @racket[_expr] for any @racket[_id],
but all are available in the @racket[_body]. The @racket[_id]s must be
different from each other.}
@racket[_id]绑定处于”平行”状态，即对于任何@racket[_id]，没有一个@racket[_id]绑定到右边的@racket[_expr]，但都可在@racket[_body]内找到。@racket[_id]必须被定义为彼此不同的形式。

@examples[
(let ([me "Bob"])
  me)
(let ([me "Bob"]
      [myself "Robert"]
      [I "Bobby"])
  (list me myself I))
(let ([me "Bob"]
      [me "Robert"])
  me)
]

@;{The fact that an @racket[_id]'s @racket[_expr] does not see its own
binding is often useful for wrappers that must refer back to the old
value:}
事实上，一个@racket[_id]的@racket[_expr]不会明白自己的绑定通常对封装有用，必须转回到旧的值：

@interaction[
(let ([+ (lambda (x y)
           (if (string? x)
               (string-append x y)
               (+ x y)))]) (code:comment @#,t{use original @racket[+]})
  (list (+ 1 2)
        (+ "see" "saw")))
]

@;{Occasionally, the parallel nature of @racket[let] bindings is
convenient for swapping or rearranging a set of bindings:}
偶尔，@racket[let]绑定的并行性可以方便地交换或重排一组绑定：

@interaction[
(let ([me "Tarzan"]
      [you "Jane"])
  (let ([me you]
        [you me])
    (list me you)))
]

@;{The characterization of @racket[let] bindings as ``parallel'' is not
meant to imply concurrent evaluation. The @racket[_expr]s are
evaluated in order, even though the bindings are delayed until all
@racket[_expr]s are evaluated.}
@racket[let]绑定为“并行（parallel）”的特性并不意味着并行求值。@racket[_expr]按顺序求值，尽管绑定延迟到所有@racket[_expr]被求值。。

@;------------------------------------------------------------------------
@;{@section{Sequential Binding: @racket[let*]}}
@section[#:tag "Sequential-Binding-let]"]{相继绑定：let*}

@refalso["let"]{@racket[let*]}

@;{The syntax of @racket[let*] is the same as @racket[let]:}
@racket[let*]的语法和@racket[let]的一样：

@specform[(let* ([id expr] ...) body ...+)]{}

@;{The difference is that each @racket[_id] is available for use in later
@racket[_expr]s, as well as in the @racket[_body]. Furthermore, the
@racket[_id]s need not be distinct, and the most recent binding is the
visible one.}
不同的是，每个@racket[_id]可用于以后的@racket[_expr]，以及@racket[_body]内。此外，@racket[_id]不需要有区别，最新的绑定可见。

@examples[
(let* ([x (list "Burroughs")]
       [y (cons "Rice" x)]
       [z (cons "Edgar" y)])
  (list x y z))
(let* ([name (list "Burroughs")]
       [name (cons "Rice" name)]
       [name (cons "Edgar" name)])
  name)
]

@;{In other words, a @racket[let*] form is equivalent to nested
@racket[let] forms, each with a single binding:}
换言之， @racket[let*]表是相当于嵌套的@racket[let]表，每一个都有一个单独的绑定：

@interaction[
(let ([name (list "Burroughs")])
  (let ([name (cons "Rice" name)])
    (let ([name (cons "Edgar" name)])
      name)))
]

@;------------------------------------------------------------------------
@;{@section{Recursive Binding: @racket[letrec]}}
@section[#:tag "Recursive-Binding-letrec"]{递归绑定：letrec}

@refalso["let"]{@racket[letrec]}

@;{The syntax of @racket[letrec] is also the same as @racket[let]:}
@racket[letrec]的语法也和@racket[let]相同：

@specform[(letrec ([id expr] ...) body ...+)]{}

@;{While @racket[let] makes its bindings available only in the
@racket[_body]s, and @racket[let*] makes its bindings available to any
later binding @racket[_expr], @racket[letrec] makes its bindings
available to all other @racket[_expr]s---even earlier ones. In other
words, @racket[letrec] bindings are recursive.}
而@racket[let]使其其绑定只在@racket[_body]内被提供，@racket[let*]使其绑定提供给任何后来的绑定@racket[_expr]， @racket[letrec]使其绑定提供给所有其它@racket[_expr]，甚至更早的。换句话说，@racket[letrec]绑定是递归的。

@;{The @racket[_expr]s in a @racket[letrec] form are most often
@racket[lambda] forms for recursive and mutually recursive functions:}
在一个@racket[letrec]表中的@racket[letrec]经常大都是递归或互相递归的@racket[lambda]表函数：

@interaction[
(letrec ([swing
          (lambda (t)
            (if (eq? (car t) 'tarzan)
                (cons 'vine
                      (cons 'tarzan (cddr t)))
                (cons (car t)
                      (swing (cdr t)))))])
  (swing '(vine tarzan vine vine)))
]

@interaction[
(letrec ([tarzan-near-top-of-tree?
          (lambda (name path depth)
            (or (equal? name "tarzan")
                (and (directory-exists? path)
                     (tarzan-in-directory? path depth))))]
         [tarzan-in-directory?
          (lambda (dir depth)
            (cond
              [(zero? depth) #f]
              [else
               (ormap
                (λ (elem)
                  (tarzan-near-top-of-tree? (path-element->string elem)
                                            (build-path dir elem)
                                            (- depth 1)))
                (directory-list dir))]))])
  (tarzan-near-top-of-tree? "tmp" 
                            (find-system-path 'temp-dir)
                            4))
]

@;{While the @racket[_expr]s of a @racket[letrec] form are typically
@racket[lambda] expressions, they can be any expression. The
expressions are evaluated in order, and after each value is obtained,
it is immediately associated with its corresponding @racket[_id]. If
an @racket[_id] is referenced before its value is ready, an
error is raised, just as for internal definitions.}
而一个@racket[letrec]表的@racket[_expr]是典型的@racket[lambda]表达式，它们可以是任何表达式。表达式按顺序求值，在获得每个值之后，它立即与相应的@racket[_id]相关联。如果@racket[_id]在其值准备就绪之前被引用，则会引发一个错误，就像内部定义一样。

@interaction[
(letrec ([quicksand quicksand])
  quicksand)
]

@; ----------------------------------------
@include-section["named-let.scrbl"]

@; ----------------------------------------
@section{Multiple Values: @racket[let-values], @racket[let*-values], @racket[letrec-values]}
@section[#:tag "Multiple-Values-let"]{多值绑定：let-values，let*-values，letrec-values}

@;{@refalso["let"]{multiple-value binding forms}}
@refalso["let"]{多只绑定表}

@;{In the same way that @racket[define-values] binds multiple
results in a definition (see @secref["multiple-values"]),
@racket[let-values], @racket[let*-values], and
@racket[letrec-values] bind multiple results locally.}
以@racket[define-values]同样的方式绑定定义的多个结果（见《多值和define-values》）（@secref["multiple-values"]），@racket[let-values]、@racket[let*-values]和@racket[letrec-values]值绑定多个局部结果。

@specform[(let-values ([(id ...) expr] ...)
            body ...+)]
@specform[(let*-values ([(id ...) expr] ...)
            body ...+)]
@specform[(letrec-values ([(id ...) expr] ...)
            body ...+)]

@;{Each @racket[_expr] must produce as many values as corresponding
@racket[_id]s. The binding rules are the same for the forms
without @racketkeywordfont{-values} forms: the @racket[_id]s of
@racket[let-values] are bound only in the @racket[_body]s, the
@racket[_id]s of @racket[let*-values]s are bound in
@racket[_expr]s of later clauses, and the @racket[_id]s of
@racket[letrec-value]s are bound for all @racket[_expr]s.}
每个@racket[_expr]必须产生许多值作为@racket[_id]的对应。绑定的规则是和没有@racketkeywordfont{-values}的形式的表相同：@racket[let-values]的@racket[_id]只绑定在@racket[_body]里，@racket[let*-values]的@racket[_id]绑定在后面从句的@racket[_expr]里，@racket[letrec-value]的@racket[_id]绑定是针对对所有的@racket[_expr]。

@examples[
(let-values ([(q r) (quotient/remainder 14 3)])
  (list q r))
]
