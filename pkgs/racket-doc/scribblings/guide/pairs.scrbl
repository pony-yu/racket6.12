#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "pairs"]{Pairs and Lists}}
@title[#:tag "pairs"]{点对（Pair）和列表（List）}

@;{A @deftech{pair} joins two arbitrary values. The @racket[cons]
procedure constructs pairs, and the @racket[car] and @racket[cdr]
procedures extract the first and second elements of the pair,
respectively. The @racket[pair?] predicate recognizes pairs.}
一个@deftech{点对（pair）}把两个任意值结合。@racket[cons]过程构建点对，@racket[car]和@racket[cdr]过程分别提取点对的第一和第二个点对元素。@racket[pair?]判断识别点对。

@;{Some pairs print by wrapping parentheses around the printed forms of
the two pair elements, putting a @litchar{'} at the beginning and a
@litchar{.} between the elements.}
一些点对通过圆括号包围两个点对元素的打印形式来打印，在开始位置放置一个@litchar{'}，并在元素之间放置一个@litchar{.}。

@examples[
(cons 1 2)
(cons (cons 1 2) 3)
(car (cons 1 2))
(cdr (cons 1 2))
(pair? (cons 1 2))
]

@;{A @deftech{list} is a combination of pairs that creates a linked
list. More precisely, a list is either the empty list @racket[null],
or it is a pair whose first element is a list element and whose second
element is a list. The @racket[list?] predicate recognizes lists. The
@racket[null?]  predicate recognizes the empty list.}
一个@deftech{列表（list）}是一个点对的组合，它创建一个链表。更确切地说，一个列表要么是空列表@racket[null]，要么是个点对，其第一个元素是一个列表元素，第二个元素是一个列表。@racket[list?]判断识别列表。@racket[null?]判断识别空列表。

@;{A list normally prints as a @litchar{'} followed by a pair of parentheses wrapped around the list elements.}
一个列表通常打印为一个@litchar{'}后跟一对括号包裹列表元素。

@examples[
null
(cons 0 (cons 1 (cons 2 null)))
(list? null)
(list? (cons 1 (cons 2 null)))
(list? (cons 1 2))
]

@;{A list or pair prints using @racketresult[list] or @racketresult[cons]
when one of its elements cannot be written as a @racket[quote]d
value. For example, a value constructed with @racket[srcloc] cannot be
written using @racket[quote], and it prints using @racketresult[srcloc]:}
当一个列表或点对的其中一个元素不能写成一个@racket[quote]（引用）值时，使用@racketresult[list]或@racketresult[cons]打印。例如，一个用@racket[srcloc]构建的值不能使用@racket[quote]来编写，应该使用@racketresult[srcloc]来编写：

@interaction[
(srcloc "file.rkt" 1 0 1 (+ 4 4))
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(cons 1 (srcloc "file.rkt" 1 0 1 8))
(cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))
]

@;{@margin-note{See also @racket[list*].}}
@margin-note{也参见@racket[list*]。}

@;{As shown in the last example, @racketresult[list*] is used to
abbreviate a series of @racketresult[cons]es that cannot be
abbreviated using @racketresult[list].}
如最后一个例子所示，@racketresult[list*]是用来缩略一系列不能使用@racketresult[list]缩写的@racketresult[cons]。

The @racket[write] and @racket[display] functions print a pair or list
without a leading @litchar{'}, @racketresult[cons],
@racketresult[list], or @racketresult[list*]. There is no difference
between @racket[write] and @racket[display] for a pair or list, except
as they apply to elements of the list:
@racket[write]和@racket[display]函数不带一个前导@litchar{'}、@racketresult[cons]、@racketresult[list]或@racketresult[list*]打印一个点对或一个列表。对于一个点对或列表来说@racket[write]和@racket[display]没有区别，除非它们运用于列表的元素：

@examples[
(write (cons 1 2))
(display (cons 1 2))
(write null)
(display null)
(write (list 1 2 "3"))
(display (list 1 2 "3"))
]

@;{Among the most important predefined procedures on lists are those that
iterate through the list's elements:}
对于列表来说最重要的预定义过程是遍历列表元素的那些过程：

@interaction[
(map (lambda (i) (/ 1 i))
     '(1 2 3))
(andmap (lambda (i) (i . < . 3))
       '(1 2 3))
(ormap (lambda (i) (i . < . 3))
       '(1 2 3))
(filter (lambda (i) (i . < . 3))
        '(1 2 3))
(foldl (lambda (v i) (+ v i))
       10
       '(1 2 3))
(for-each (lambda (i) (display i))
          '(1 2 3))
(member "Keys"
        '("Florida" "Keys" "U.S.A."))
(assoc 'where
       '((when "3:30") (where "Florida") (who "Mickey")))
]

@;{@refdetails["pairs"]{pairs and lists}}
@margin-note{在《Racket参考》中的“点对和列表（Pairs and Lists）”提供更多有关点对和列表的信息。}

@;{Pairs are immutable (contrary to Lisp tradition), and @racket[pair?]
and @racket[list?] recognize immutable pairs and lists, only. The
@racket[mcons] procedure creates a @deftech{mutable pair}, which works
with @racket[set-mcar!] and @racket[set-mcdr!], as well as
@racket[mcar] and @racket[mcdr]. A mutable pair prints using
@racketresult[mcons], while @racket[write] and @racket[display] print
mutable pairs with @litchar["{"] and @litchar["}"]:}
点对是不可变的（与Lisp传统相反），并且@racket[pair?]和@racket[list?]仅识别不可变的点对和列表。@racket[mcons]过程创建一个@deftech{可变点对（mutable pair）}，它配合@racket[set-mcar!]和@racket[set-mcdr!]，及@racket[mcar]和@racket[mcdr]进行操作。一个可变点对用@racketresult[mcons]打印，而@racket[write]和@racket[display]使用@litchar["{"]和@litchar["}"]打印：

@examples[
(define p (mcons 1 2))
p
(pair? p)
(mpair? p)
(set-mcar! p 0)
p
(write p)
]

@;{@refdetails["mpairs"]{mutable pairs}}
@margin-note{在《Racket参考》中的“可变点对和列表（Mutable Pairs and Lists）”中提供关于可变点对的更多信息。}
