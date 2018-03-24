#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/match))

@;{@title[#:tag "case"]{Simple Dispatch: @racket[case]}}
@title[#:tag "case"]{简单分派：@racket[case]}

@;{The @racket[case] form dispatches to a clause by matching the result
of an expression to the values for the clause:}
通过将表达式的结果与子句的值相匹配，@racket[case]表分派一个子句：

@specform[(case expr
            [(datum ...+) body ...+]
            ...)]

@;{Each @racket[_datum] will be compared to the result of @racket[_expr]
using @racket[equal?], and then the corresponding @racket[body]s are
evaluated. The @racket[case] form can dispatch to the correct clause
in @math{O(log N)} time for @math{N} @racket[datum]s.}
每个@racket[_datum]将使用@racket[equal?]对比@racket[_expr]的结果，然后相应的@racket[body]被求值。@racket[case]表可以为@math{N}个@racket[datum]在@math{O(log N)}时间内分派正确的从句。

@;{Multiple @racket[_datum]s can be supplied for each clause, and the
corresponding @racket[_body]s are evaluated if any of the
@racket[_datum]s match.}
可以给每个从句提供多个@racket[_datum]，而且如果任何一个@racket[_datum]匹配，那么相应的@racket[_body]被求值。

@examples[
(let ([v (random 6)])
  (printf "~a\n" v)
  (case v
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [(3 4 5) 'many]))
]

@;{The last clause of a @racket[case] form can use @racket[else], just
like @racket[cond]:}
一个@racket[case]表最后一个从句可以使用@racket[else]，就像@racket[cond]那样：

@examples[
(case (random 6)
  [(0) 'zero]
  [(1) 'one]
  [(2) 'two]
  [else 'many])
]

@;{For more general pattern matching (but without the dispatch-time
guarantee), use @racket[match], which is introduced in
@secref["match"].}
对于更一般的模式匹配（但没有分派时间保证），使用@racket[match]，这个会在《模式匹配》（@secref["match"]）中介绍。
