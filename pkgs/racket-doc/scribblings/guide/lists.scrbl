#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf racket/list
          "guide-utils.rkt"
          (for-label racket/list))

@(define step @elem{=})

@(define list-eval (make-base-eval))
@(interaction-eval #:eval list-eval (require racket/list))

@;{@title{Lists, Iteration, and Recursion}}
@title[#:tag "Lists-Iteration-and-Recursion"]{列表、迭代和递归}

@;{Racket is a dialect of the language Lisp, whose name originally stood
for ``LISt Processor.'' The built-in list datatype remains a prominent
feature of the language.}
Racket语言是Lisp语言的一种方言，名字来自于“LISt Processor”。内置的列表数据类型保留了这种语言的一个显著特征。

@;{The @racket[list] function takes any number of values and returns
a list containing the values:}
@racket[list]函数接受任意数量的值并返回一个包含这些值的列表：

@interaction[(list "red" "green" "blue")
             (list 1 2 3 4 5)]

@;{@margin-note{A list usually prints with @litchar{'}, but the printed
             form of a list depends on its content. See
             @secref["pairs"] for more information.}}
@margin-note{一个列表通常用@litchar{'}打印，但是一个列表的打印形式取决于它的内容。更多信息请看《@secref["pairs"]》。}

@;{As you can see, a list result prints in the @tech{REPL} as a quote
@litchar{'} and then a pair of parentheses wrapped around the printed
form of the list elements. There's an opportunity for confusion here,
because parentheses are used for both expressions, such as
@racket[(list "red" "green" "blue")], and printed results, such as
@racketresult['("red" "green" "blue")]. In addition to the quote,
parentheses for results are printed in blue in the documentation and
in DrRacket, whereas parentheses for expressions are brown.}
就如你能够看到的那样，一个列表结果在@tech{REPL}中打印为一个引用@litchar{'}，并且采用一对圆括号包围这个列表元素的打印表。这里有一个容易混淆的地方，因为两个表达式都使用圆括号，比如@racket[(list "red" "green" "blue")]，那么打印结果为@racketresult['("red" "green" "blue")]。除了引用，结果的圆括号在文档中和在DrRacket中打印为蓝色，而表达式的圆括号是棕色的。

@;{Many predefined functions operate on lists. Here are a few examples:}
在列表方面有许多预定义的函数操作。下面是少许例子：

@interaction[
(code:line (length (list "hop" "skip" "jump"))        (code:comment @#,t{count the elements}))
(code:line (list-ref (list "hop" "skip" "jump") 0)    (code:comment @#,t{extract by position}))
(list-ref (list "hop" "skip" "jump") 1)
(code:line (append (list "hop" "skip") (list "jump")) (code:comment @#,t{combine lists}))
(code:line (reverse (list "hop" "skip" "jump"))       (code:comment @#,t{reverse order}))
(code:line (member "fall" (list "hop" "skip" "jump")) (code:comment @#,t{check for an element}))
]

@;------------------------------------------------------------------------
@;{@section{Predefined List Loops}}
@section[#:tag "Predefined-List-Loops"]{预定义列表循环}

@;{In addition to simple operations like @racket[append], Racket includes
functions that iterate over the elements of a list. These iteration
functions play a role similar to @racket[for] in Java, Racket, and other
languages. The body of a Racket iteration is packaged into a function
to be applied to each element, so the @racket[lambda] form becomes
particularly handy in combination with iteration functions.}
除了像@racket[append]这样的简单操作，Racket还包括遍历列表元素的函数。这些迭代函数扮演类似于java、Racket及其它语言里的@racket[for]一个角色。一个Racket迭代的主体被打包成一个应用于每个元素的函数，所以@racket[lambda]表在与迭代函数的组合中变得特别方便。

@;{Different list-iteration functions combine iteration results in
different ways. The @racket[map] function uses the per-element
results to create a new list:}
不同的列表迭代函数在不同的方式中组合迭代结果。@racket[map]函数使用每个元素结果创建一个新列表：

@interaction[
(map sqrt (list 1 4 9 16))
(map (lambda (i)
       (string-append i "!"))
     (list "peanuts" "popcorn" "crackerjack"))
]

@;{The @racket[andmap] and @racket[ormap] functions combine the results
by @racket[and]ing or @racket[or]ing:}
@racket[andmap]和@racket[ormap]函数通过@racket[and]或@racket[or]结合结果：

@interaction[
(andmap string? (list "a" "b" "c"))
(andmap string? (list "a" "b" 6))
(ormap number? (list "a" "b" 6))
]

@;{The @racket[map], @racket[andmap], and @racket[ormap]
functions can all handle multiple lists, instead of just a single
list. The lists must all have the same length, and the given function
must accept one argument for each list:}
@racket[map]、@racket[andmap]和@racket[ormap]函数都可以处理多个列表，而不只是一个单一的列表。这些列表都必须具有相同的长度，并且给定的函数必须对每个列表接受一个参数：

@interaction[
(map (lambda (s n) (substring s 0 n))
     (list "peanuts" "popcorn" "crackerjack")
     (list 6 3 7))
]

@;{The @racket[filter] function keeps elements for which the body result
is true, and discards elements for which it is @racket[#f]:}
@racket[filter]函数保持其主体结果是真的元素，并忽略其主体结果是@racket[#f]的元素：

@interaction[
(filter string? (list "a" "b" 6))
(filter positive? (list 1 -2 6 7 0))
]

@;{The @racket[foldl] function generalizes some iteration functions. It
uses the per-element function to both process an element and combine
it with the ``current'' value, so the per-element function takes an
extra first argument. Also, a starting ``current'' value must be
provided before the lists:}
@racket[foldl]函数包含某些迭代函数。它使用每个元素函数处理一个元素并将其与“当前”值相结合，因此每个元素函数接受额外的第一个参数。另外，在列表之前必须提供一个开始的“当前”值：

@interaction[
(foldl (lambda (elem v)
         (+ v (* elem elem)))
       0
       '(1 2 3))
]

@;{Despite its generality, @racket[foldl] is not as popular as the other
functions. One reason is that @racket[map], @racket[ormap],
@racket[andmap], and @racket[filter] cover the most common kinds of
list loops.}
尽管有其共性，@racket[foldl]不是像其它函数一样受欢迎。一个原因是@racket[map]、 @racket[ormap]、@racket[andmap]和@racket[filter]覆盖了最常见的列表迭代。

@;{Racket provides a general @defterm{list comprehension} form
@racket[for/list], which builds a list by iterating through
@defterm{sequences}. List comprehensions and related iteration forms
are described in @secref["for"].}
Racket提供了一个通用的@defterm{列表理解}表@racket[for/list]，它用迭代通过@defterm{序列（sequences）}来建立一个列表。列表理解表和相关迭代表将在《@secref["for"]》部分描述。

@;------------------------------------------------------------------------
@;{@section{List Iteration from Scratch}}
@section[#:tag "List-Iteration-from-Scratch"]{从头开始列表迭代}

@;{Although @racket[map] and other iteration functions are predefined, they
are not primitive in any interesting sense. You can write equivalent
iterations using a handful of list primitives.}
尽管@racket[map]和其它迭代函数是预定义的，但它们在任何令人感兴趣的意义上都不是原始的。使用少量列表原语即能编写等效迭代。

@;{Since a Racket list is a linked list, the two core operations on a
non-empty list are}
由于一个Racket列表是一个链表，对非空列表的两个核心操作是:

@itemize[

 @item{
  @;{@racket[first]: get the first thing in the list; and}
    @racket[first]：取得列表上的第一个元素；
 }

 @item{
  @;{@racket[rest]: get the rest of the list.}
    @racket[rest]：获取列表的其余部分。
 }

]

@examples[
#:eval list-eval
(first (list 1 2 3))
(rest (list 1 2 3))
]

@;{To create a new node for a linked list---that is, to add to the front
of the list---use the @racket[cons] function, which is short for
``construct.'' To get an empty list to start with, use the
@racket[empty] constant:}
为一个链表添加一个新的节点——确切地说，添加到这个列表的前面——使用@racket[cons]函数，它是“construct”（构造）的缩写。要得到一个空列表用于开始，用@racket[empty]来构造：

@interaction[
#:eval list-eval
empty
(cons "head" empty)
(cons "dead" (cons "head" empty))
]

@;{To process a list, you need to be able to distinguish empty lists from
non-empty lists, because @racket[first] and @racket[rest] work only on
non-empty lists. The @racket[empty?] function detects empty lists,
and @racket[cons?] detects non-empty lists:}
要处理一个列表，你需要能够区分空列表和非空列表，因为@racket[first]和@racket[rest]只在非空列表上工作。@racket[empty?]函数检测空列表，@racket[cons?]检测非空列表：

@interaction[
#:eval list-eval
(empty? empty)
(empty? (cons "head" empty))
(cons? empty)
(cons? (cons "head" empty))
]

@;{With these pieces, you can write your own versions of the
@racket[length] function, @racket[map] function, and more.}
通过这些片段，你能够编写你自己的@racket[length]函数、@racket[map]函数以及更多的函数的版本。

@defexamples[
#:eval list-eval
(define (my-length lst)
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))
(my-length empty)
(my-length (list "a" "b" "c"))
]
@def+int[
#:eval list-eval
(define (my-map f lst)
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
               (my-map f (rest lst)))]))
(my-map string-upcase (list "ready" "set" "go"))
]

@;{If the derivation of the above definitions is mysterious to you,
consider reading @|HtDP|. If you are merely suspicious of the use
of recursive calls instead of a looping construct, then read on.}
如果上述定义的派生对你来说难以理解，建议去读《@|HtDP|（如何设计程序）》。如果你只对使用递归调用表示不信任，而不是循环结构，那就继续往后读。

@;------------------------------------------------------------------------
@;{@section[#:tag "tail-recursion"]{Tail Recursion}}
@section[#:tag "tail-recursion"]{尾递归}

@;{Both the @racket[my-length] and @racket[my-map] functions run in
@math{O(n)} space for a list of length @math{n}. This is easy to see by
imagining how @racket[(my-length (list "a" "b" "c"))] must evaluate:}
@racket[my-length]函数和@racket[my-map]函数都为一个 @math{n}长度的列表在@math{O(n)}空间内运行。通过想象就很容易明白必须怎样@racket[(my-length (list "a" "b" "c"))]求值：

@racketblock[
#||# (my-length (list "a" "b" "c"))
#,step (+ 1 (my-length (list "b" "c")))
#,step (+ 1 (+ 1 (my-length (list "c"))))
#,step (+ 1 (+ 1 (+ 1 (my-length (list)))))
#,step (+ 1 (+ 1 (+ 1 0)))
#,step (+ 1 (+ 1 1))
#,step (+ 1 2)
#,step 3
]

@;{For a list with @math{n} elements, evaluation will stack up @math{n}
@racket[(+ 1 ...)] additions, and then finally add them up when the
list is exhausted.}
对于一个带有@math{n}个元素的列表，求值将堆栈叠加@math{n}@racket[(+ 1 ...)]，并且直到列表用完时最后才把它们加起来。

@;{You can avoid piling up additions by adding along the way. To
accumulate a length this way, we need a function that takes both a
list and the length of the list seen so far; the code below uses a
local function @racket[iter] that accumulates the length in an
argument @racket[len]:}
你可以通过一路求和避免堆积添加。要以这种方式累积一个长度，我们需要一个既可以操作列表也可以操作当前列表长度的函数；下面的代码使用一个局部函数@racket[iter]，它在一个参数@racket[len]中累积长度：

@racketblock[
(define (my-length lst)
  (code:comment @#,t{@;{local function @racket[iter]:}局部函数@racket[iter]:})
  (define (iter lst len)
    (cond
     [(empty? lst) len]
     [else (iter (rest lst) (+ len 1))]))
  (code:comment @#,t{@;{body of @racket[my-length] calls @racket[iter]:}@racket[my-length]的主体调用@racket[iter]:})
  (iter lst 0))
]

@;{Now evaluation looks like this:}
现在求值过程看起来像这样：

@racketblock[
#||# (my-length (list "a" "b" "c"))
#,step (iter (list "a" "b" "c") 0)
#,step (iter (list "b" "c") 1)
#,step (iter (list "c") 2)
#,step (iter (list) 3)
3
]

@;{The revised @racket[my-length] runs in constant space, just as the
evaluation steps above suggest. That is, when the result of a
function call, like @racket[(iter (list "b" "c") 1)], is exactly the
result of some other function call, like @racket[(iter (list "c")
2)], then the first one doesn't have to wait around for the second
one, because that takes up space for no good reason.}
修正后的@racket[my-length]在恒定的空间中运行，正如上面所建议的求值步骤那样。也就是说，当一个函数调用的结果，像@racket[(iter (list "b" "c") 1)]，恰恰是其它函数调用的结果，像@racket[(iter (list "c"))]，那么第一个函数不需要等待第二个函数回绕，因为那样会为了不恰当的原因占用空间。

@;{This evaluation behavior is sometimes called @idefterm{tail-call
optimization}, but it's not merely an ``optimization'' in Racket; it's
a guarantee about the way the code will run. More precisely, an
expression in @deftech{tail position} with respect to another
expression does not take extra computation space over the other
expression.}
这种求值行为有时称为@idefterm{”尾部调用优化(tail-call
optimization)“}，但它在Racket里不仅仅是一种“优化”，它是一种代码将要运行的方式的保证。更确切地说，相对于另一表达式的一个@deftech{尾部(tail position)}位置的表达式在另一表达式上不占用额外的计算空间。

@;{In the case of @racket[my-map], @math{O(n)} space complexity is
reasonable, since it has to generate a result of size
@math{O(n)}. Nevertheless, you can reduce the constant factor by
accumulating the result list. The only catch is that the accumulated
list will be backwards, so you'll have to reverse it at the very end:}
在@racket[my-map]例子中，@math{O(n)}空间复杂度是合理的，因为它必须生成一个@math{O(n)}大小的结果。不过，你可以通过累积结果列表来减少常量因子。唯一的问题是累积的列表将是向后的，所以你将不得不在每个结尾处反转它：

@;{@margin-note{Attempting to reduce a constant factor like this is
usually not worthwhile, as discussed below.}}
@margin-note{如下面所述，试图减少像这样的一个常量因子通常是不值得的。}

@racketblock[
(define (my-map f lst)
  (define (iter lst backward-result)
    (cond
     [(empty? lst) (reverse backward-result)]
     [else (iter (rest lst)
                 (cons (f (first lst))
                       backward-result))]))
  (iter lst empty))
]

@;{It turns out that if you write}
事实证明，如果你这样写：

@racketblock[
(define (my-map f lst)
  (for/list ([i lst])
    (f i)))
]

@;{then the @racket[for/list] form in the function is expanded to
essentially the same code as the @racket[iter] local definition and
use. The difference is merely syntactic convenience.}
那么函数中的@racket[for/list]表扩展到和@racket[iter]函数局部定义和使用在本质上相同的代码。区别仅仅是句法上的便利。

@;------------------------------------------------------------------------
@;{@section{Recursion versus Iteration}}
@section[#:tag "Recursion-versus-Iteration"]{递归和迭代}

@;{The @racket[my-length] and @racket[my-map] examples demonstrate that
iteration is just a special case of recursion. In many languages, it's
important to try to fit as many computations as possible into
iteration form. Otherwise, performance will be bad, and moderately
large inputs can lead to stack overflow.  Similarly, in Racket, it is
sometimes important to make sure that tail recursion is used to avoid
@math{O(n)} space consumption when the computation is easily performed
in constant space.}
@racket[my-length]和@racket[my-map]示例表明迭代只是递归的一个特例。在许多语言中，尽可能地将尽可能多的计算合并成迭代形式是很重要的。否则，性能会变差，不太大的输入都会导致堆栈溢出。类似地，在Racket中，有时很重要的一点是要确保在易于计算的常数空间中使用尾递归避免@math{O(n)}空间消耗。

@;{At the same time, recursion does not lead to particularly bad
performance in Racket, and there is no such thing as stack overflow;
you can run out of memory if a computation involves too much context,
but exhausting memory typically requires orders of magnitude deeper
recursion than would trigger a stack overflow in other
languages. These considerations, combined with the fact that
tail-recursive programs automatically run the same as a loop, lead
Racket programmers to embrace recursive forms rather than avoid them.}
然而，在Racket里递归不会导致特别差的性能，而且没有堆栈溢出那样的事情；如果一个计算涉及到太多的上下文，你可能耗尽内存，但耗尽内存通常需要比可能触发其它语言中的堆栈溢出更多数量级以上的更深层次的递归。基于这些考虑因素，加上尾递归程序会自动和一个循环一样运行的事实相结合，引导Racket程序员接受递归形式而不是避免它们。

@;{Suppose, for example, that you want to remove consecutive duplicates
from a list. While such a function can be written as a loop that
remembers the previous element for each iteration, a Racket programmer
would more likely just write the following:}
例如，假设你想从一个列表中去除连续的重复项。虽然这样的一个函数可以写成一个循环，为每次迭代记住前面的元素，但一个Racket程序员更可能只写以下内容：

@def+int[
#:eval list-eval
(define (remove-dups l)
  (cond
   [(empty? l) empty]
   [(empty? (rest l)) l]
   [else
    (let ([i (first l)])
      (if (equal? i (first (rest l)))
          (remove-dups (rest l))
          (cons i (remove-dups (rest l)))))]))
(remove-dups (list "a" "b" "b" "b" "c" "c"))
]

@;{In general, this function consumes @math{O(n)} space for an input
list of length @math{n}, but that's fine, since it produces an
@math{O(n)} result. If the input list happens to be mostly consecutive
duplicates, then the resulting list can be much smaller than
@math{O(n)}---and @racket[remove-dups] will also use much less than
@math{O(n)} space! The reason is that when the function discards
duplicates, it returns the result of a @racket[remove-dups] call
directly, so the tail-call ``optimization'' kicks in:}
一般来说，这个函数为一个长度为@math{n}的输入列表消耗@math{O(n)}的空间，但这很好，因为它产生一个@math{O(n)}结果。如果输入列表恰巧是连续重复的，那么得到的列表可以比@math{O(n)}小得多——而且@racket[remove-dups]也将使用比@math{O(n)}更少的空间！原因是当函数放弃重复，它返回一个@racket[remove-dups]的直接调用结果，所以尾部调用“优化”加入：

@racketblock[
#||# (remove-dups (list "a" "b" "b" "b" "b" "b"))
#,step (cons "a" (remove-dups (list "b" "b" "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b" "b")))
#,step (cons "a" (remove-dups (list "b" "b")))
#,step (cons "a" (remove-dups (list "b")))
#,step (cons "a" (list "b"))
#,step (list "a" "b")
]

@; ----------------------------------------------------------------------

@close-eval[list-eval]
