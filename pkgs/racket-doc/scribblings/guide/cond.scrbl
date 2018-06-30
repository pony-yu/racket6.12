#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "conditionals"]{Conditionals}}
@title[#:tag "conditionals"]{条件}

@;{Most functions used for branching, such as @racket[<] and
@racket[string?], produce either @racket[#t] or @racket[#f]. Racket's
branching forms, however, treat any value other than @racket[#f] as
true. We say a @defterm{true value} to mean any value other than
@racket[#f].}
大多数函数都可用于分支，如@racket[<]和@racket[string?]，产生@racket[#t]或@racket[#f]。无论什么情况，Racket的分支表以任何非@racket[#f]值为真。我们说一个@defterm{真值（true value）}意味着@racket[#f]值之外的任何值。

@;{This convention for ``true value'' meshes well with protocols where
@racket[#f] can serve as failure or to indicate that an optional value
is not supplied. (Beware of overusing this trick, and remember that an
exception is usually a better mechanism to report failure.)}
这个对“真值（true value）”的约定在@racket[#f]能够代替故障或表明不提供一个可选的值的地方与协议完全吻合 。（谨防过度使用这一技巧，记住一个异常通常对报告故障是一个更好的机制。）

@;{For example, the @racket[member] function serves double duty; it can
be used to find the tail of a list that starts with a particular item,
or it can be used to simply check whether an item is present in a
list:}
例如，@racket[member]函数具有双重职责；它可以用来查找一个从一个特定条目开始的列表的尾部，或者它可以用来简单地检查一个项目是否存在于一个列表中：

@interaction[
(member "Groucho" '("Harpo" "Zeppo"))
(member "Groucho" '("Harpo" "Groucho" "Zeppo"))
(if (member "Groucho" '("Harpo" "Zeppo"))
    'yep
    'nope)
(if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
    'yep
    'nope)
]

@;------------------------------------------------------------------------
@;{@section{Simple Branching: @racket[if]}}
@section[#:tag "Simple-Branching-if"]{简单分支：@racket[if]}

@;{@refalso["if"]{@racket[if]}}
@margin-note{在《Racket参考》里的“（if）”部分有关于@racket[if]的文档。}

@;{In an @racket[if] form,}
在一个@racket[if]表里：

@specform[(if test-expr then-expr else-expr)]

@;{the @racket[_test-expr] is always evaluated. If it produces any value
other than @racket[#f], then @racket[_then-expr] is
evaluated. Otherwise, @racket[_else-expr] is evaluated.}
@racket[_test-expr]总是被求值。如果它产生任何非@racket[#f]值，那么@racket[_then-expr]被求值。否则，@racket[_else-expr]被求值。

@;{An @racket[if] form must have both a @racket[_then-expr] and an
@racket[_else-expr]; the latter is not optional. To perform (or skip)
side-effects based on a @racket[_test-expr], use @racket[when] or
@racket[unless], which we describe later in @secref["begin"].}
一个@racket[if]表必须既有一个@racket[_then-expr]也有一个@racket[_else-expr]；后者不是可选的。执行（或跳过）基于一个@racket[_test-expr]的副作用，使用@racket[when]或@racket[unless]，对此我们将在后边《@secref["begin"]》部分描述。

@;------------------------------------------------------------------------
@;{@section[#:tag "and+or"]{Combining Tests: @racket[and] and @racket[or]}}
@section[#:tag "and+or"]{组合测试：@racket[and]和@racket[or]}

@;{@refalso["if"]{@racket[and] and @racket[or]}}
@margin-note{在《Racket参考》的“（if）”部分有关于@racket[and]和@racket[or]的文档。}

@;{Racket's @racket[and] and @racket[or] are syntactic forms, rather than
functions. Unlike a function, the @racket[and] and @racket[or] forms
can skip evaluation of later expressions if an earlier one determines
the answer.}
Racket的@racket[and]和@racket[or]是语法表，而不是函数。不像一个函数，如果前边的一个求值确定了答案，@racket[and]和@racket[or]表会忽略后边表达式的求值。

@specform[(and expr ...)]

@;{An @racket[and] form produces @racket[#f] if any of its @racket[_expr]s
produces @racket[#f]. Otherwise, it produces the value of its last
@racket[_expr]. As a special case, @racket[(and)] produces
@racket[#t].}
如果其所有@racket[_expr]产生@racket[#f]，一个@racket[and]表产生@racket[#f]。否则，它从它最后的@racket[expr]产生值。作为一个特殊的情况，@racket[(and)]产生@racket[#t]。

@specform[(or expr ...)]

@;{The @racket[or] form produces @racket[#f] if all of its
@racket[_expr]s produce @racket[#f]. Otherwise, it produces the first
non-@racket[#f] value from its @racket[expr]s.  As a special case,
@racket[(or)] produces @racket[#f].}
如果其所有的@racket[_expr]产生@racket[#f]，@racket[and]表产生@racket[#f]。否则，它从它的@racket[expr]第一个非@racket[#f]值产生值。作为一个特殊的情况，@racket[(or)]产生@racket[#f]。

@examples[
(code:line
 (define (got-milk? lst)
   (and (not (null? lst))
        (or (eq? 'milk (car lst))
            (got-milk? (cdr lst))))) (code:comment @#,t{@;{recurs only if needed}仅在需要时再发生。}))
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))
]

If evaluation reaches the last @racket[_expr] of an @racket[and] or
@racket[or] form, then the @racket[_expr]'s value directly determines
the @racket[and] or @racket[or] result. Therefore, the last
@racket[_expr] is in tail position, which means that the above
@racket[got-milk?] function runs in constant space.
如果求值达到一个@racket[and]或@racket[or]}表的最后的@racket[_expr]，那么@racket[_expr]的值直接决定@racket[and]或@racket[or]}的结果。因此，最后的@racket[_expr]是在尾部的位置，这意味着上面的@racket[got-milk?]函数在固定空间中运行。

@;{@guideother{@secref["tail-recursion"] introduces tail calls and tail positions.}}
@guideother{《@secref["tail-recursion"]》介绍尾部调用和尾部位置。}

@;------------------------------------------------------------------------
@;{@section[#:tag "cond"]{Chaining Tests: @racket[cond]}}
@section[#:tag "cond"]{编链测试：cond}

@;{The @racket[cond] form chains a series of tests to select a result
expression. To a first approximation, the syntax of @racket[cond] is
as follows:}
@racket[cond]表编链了一系列的测试以选择一个结果表达式。对于一个初步近式，@racket[cond]语法如下：

@;{@refalso["if"]{@racket[cond]}}
@margin-note{在《Racket参考》里的“(if)”部分也有关于@racket[cond]的文档。}

@specform[(cond [test-expr body ...+]
                ...)]

@;{Each @racket[_test-expr] is evaluated in order. If it produces
@racket[#f], the corresponding @racket[_body]s are ignored, and
evaluation proceeds to the next @racket[_test-expr]. As soon as a
@racket[_test-expr] produces a true value, its @racket[_body]s
are evaluated to produce the result for the @racket[cond] form, and no
further @racket[_test-expr]s are evaluated.}
每个@racket[_test-expr]被按顺序求值。如果它产生@racket[#f]，相应的@racket[_body]被忽略，并且求值进行到下一个@racket[_test-expr]。一旦一个@racket[_test-expr]产生一个真值，它的@racket[_body]被求值以产生作为@racket[cond]表的结果。并不再进一步对@racket[_test-expr]求值。

@;{The last @racket[_test-expr] in a @racket[cond] can be replaced by
@racket[else]. In terms of evaluation, @racket[else] serves as a
synonym for @racket[#t], but it clarifies that the last clause is
meant to catch all remaining cases. If @racket[else] is not used, then
it is possible that no @racket[_test-expr]s produce a true value; in
that case, the result of the @racket[cond] expression is
@|void-const|.}
在一个@racket[cond]里最后的@racket[_test-expr]可用@racket[else]代替。就求值而言，@racket[else]作为一个@racket[#t]的同义词提供，但它阐明了最后的从句意味着捕获所有剩余的实例。如果@racket[else]没有被使用，那么可能没有@racket[_test-expr]产生一个真值；在这种情况下，该@racket[cond]表达式的结果是@|void-const|。

@examples[
(cond
 [(= 2 3) (error "wrong!")]
 [(= 2 2) 'ok])
(cond
 [(= 2 3) (error "wrong!")])
(cond
 [(= 2 3) (error "wrong!")]
 [else 'ok])
]

@def+int[
(define (got-milk? lst)
  (cond
    [(null? lst) #f]
    [(eq? 'milk (car lst)) #t]
    [else (got-milk? (cdr lst))]))
(got-milk? '(apple banana))
(got-milk? '(apple milk banana))
]

@;{The full syntax of @racket[cond] includes two more kinds of clauses:}
@racket[cond]的完整语法包括另外两种从句：

@specform/subs[#:literals (else =>)
               (cond cond-clause ...)
               ([cond-clause [test-expr then-body ...+]
                             [else then-body ...+]
                             [test-expr => proc-expr]
                             [test-expr]])]

@;{The @racket[=>] variant captures the true result of its
@racket[_test-expr] and passes it to the result of the
@racket[_proc-expr], which must be a function of one argument.}
@racket[=>]变体获取其@racket[_test-expr]的真值结果并且传递给@racket[_proc-expr]的结果，@racket[_proc-expr]必须是有一个参数的一个函数。

@examples[
(define (after-groucho lst)
  (cond
    [(member "Groucho" lst) => cdr]
    [else (error "not there")]))

(after-groucho '("Harpo" "Groucho" "Zeppo"))
(after-groucho '("Harpo" "Zeppo"))
]

@;{A clause that includes only a @racket[_test-expr] is rarely used. It
captures the true result of the @racket[_test-expr], and simply
returns the result for the whole @racket[cond] expression.}
一个从句只包括一个@racket[_test-expr]是很少使用的。它捕获@racket[_test-expr]的真值结果，并简单地返回这个结果给整个@racket[cond]表达式。
