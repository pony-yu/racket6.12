#lang scribble/doc
@(require scribble/manual scribble/eval "../guide-utils.rkt" "utils.rkt"
          (for-label racket/contract))

@;{@title[#:tag "contracts-struct"]{Contracts on Structures}}
@title[#:tag "contracts-struct"]{结构的合约}

@;{Modules deal with structures in two ways. First they export
@racket[struct] definitions, i.e., the ability to create
structs of a certain kind, to access their fields, to modify
them, and to distinguish structs of this kind against every
other kind of value in the world. Second, on occasion a
module exports a specific struct and wishes to promise that
its fields contain values of a certain kind. This section
explains how to protect structs with contracts for both
uses.}
模块以两种方式处理结构。首先它们导出@racket[struct]的定义，即创造一种明确方法的结构的能力，存取它们的字段，修改它们，并使这类结构和领域内的每一种值有区别。其次，有时模块导出特定的结构，并希望它的字段包含某种类型的值。本节说明如何使用合约保护结构。

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "single-struct"]{Guarantees for a Specific Value}}
@ctc-section[#:tag "single-struct"]{对特定值的确保}

@;{If your module defines a variable to be a structure, then you can
specify the structure's shape using @racket[struct/c]:}
如果你的模块定义了一个变量为一个结构，那么你可以使用@racket[struct/c]指定结构的形态：

@racketmod[
racket
(require lang/posn)
  
(define origin (make-posn 0 0))

(provide (contract-out
          [origin (struct/c posn zero? zero?)]))
]

@;{In this example, the module imports a library for representing positions, which
exports a @racket[posn] structure. One of the @racket[posn]s it creates
and exports stands for the origin, i.e., @tt{(0,0)}, of the grid.}
在这个例子中，该模块导入一个代表位置的库，它导出了一个@racket[posn]结构。其中的@racket[posn]创建并导出所代表的网格起点，即@tt{(0,0)}。

@;{@margin-note{See also @racket[vector/c] and similar contract
combinators for (flat) compound data.}}
@margin-note{又见@racket[vector/c]和类似的对（扁平）复合数据的合约组合。}

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "define-struct"]{Guarantees for All Values}}
@ctc-section[#:tag "define-struct"]{对所有值的确保}

@;{The book @|HtDP| teaches that @racket[posn]s should contain only
numbers in their two fields. With contracts we would enforce this
informal data definition as follows:}
这@|HtDP|本书教授了@racket[posn]应该只包含在它们两个字段里的数值。有了合约，我们将执行以下非正式数据定义：

@racketmod[
racket
(struct posn (x y))
  
(provide (contract-out
          [struct posn ((x number?) (y number?))]
          [p-okay posn?]
          [p-sick posn?]))

(define p-okay (posn 10 20))
(define p-sick (posn 'a 'b))
]

@;{This module exports the entire structure definition: @racket[posn],
@racket[posn?], @racket[posn-x], @racket[posn-y],
@racket[set-posn-x!], and @racket[set-posn-y!]. Each function enforces
or promises that the two fields of a @racket[posn] structure are
numbers --- when the values flow across the module boundary.  Thus, if
a client calls @racket[posn] on @racket[10] and @racket['a], the
contract system signals a contract violation.}
这个模块导出整个结构的定义：@racket[posn]、@racket[posn?]、@racket[posn-x]、@racket[posn-y]、@racket[set-posn-x!]和@racket[set-posn-y!]。 每个函数强制执行或承诺@racket[posn]结构的这两个字段是数值——当这些值在模块范围内传递时。因此，如果一个客户机对@racket[10]和@racket['a]调用@racket[posn]，合约系统就发出违反合约的信号。

@;{The creation of @racket[p-sick] inside of the @racket[posn] module,
however, does not violate the contracts. The function @racket[posn] is
used internally, so @racket['a] and @racket['b] don't cross the module
boundary. Similarly, when @racket[p-sick] crosses the boundary of
@racket[posn], the contract promises a @racket[posn?] and nothing
else. In particular, this check does @italic{not} require that the
fields of @racket[p-sick] are numbers.}
然而，@racket[posn]模块内的@racket[p-sick]的创建，并不违反合约。@racket[posn]函数是在内部使用，所以@racket['a]和@racket['b]不跨约模块范围。同样，当@racket[p-sick]跨越@racket[posn]的范围时，合约承诺了@racket[posn?]，别的什么也没有。特别是，@racket[p-sick]的字段数是数值这个检查完全不需要。

@;{The association of contract checking with module boundaries implies that
@racket[p-okay] and @racket[p-sick] look alike from a client's
perspective until the client extracts the pieces:}
对模块范围的合约检查意味着@racket[p-okay]和@racket[p-sick]从客户机的角度看起来相似，直到客户机引用以下片断：

@racketmod[
racket
(require lang/posn)
  
... (posn-x p-sick) ...
]

@;{Using @racket[posn-x] is the only way the client can find out what
a @racket[posn] contains in the @racket[x] field. The application of
@racket[posn-x] sends @racket[p-sick] back into the
@racket[posn] module and the result value -- @racket['a] here -- back to
the client, again across the module boundary. At this very point, the contract
system discovers that a promise is broken. Specifically, @racket[posn-x]
doesn't return a number but a symbol and is therefore blamed.}
使用@racket[posn-x]是客户机可以找到一个@racket[posn]包含@racket[x]字段的唯一途径。对@racket[posn-x]应用程序发送@racket[p-sick]回传给@racket[posn]模块并且结果值——@racket['a]这里——回传给客户机，再跨越模块范围。在这一点上，合约系统发现承诺被打破了。具体来说，@racket[posn-x]没有返回一个数值但却返回了一个符号，因此应归咎于它。

@;{This specific example shows that the explanation for a contract violation
doesn't always pinpoint the source of the error. The good news is that the
error is located in the @racket[posn] module. The bad news is that the
explanation is misleading. Although it is true that @racket[posn-x]
produced a symbol instead of a number, it is the fault of the programmer who
created a @racket[posn] from symbols, i.e., the programmer who added}
这个具体的例子表明，对违背合约的解释并不总是指明错误的来源。好消息是，错误位于@racket[posn]模块。坏消息是这种解释是误导性的。虽然这是真的，@racket[posn-x]产生一个符号而不是一个数值，它是程序员的责任，他从符号创建了@racket[posn]，即程序员添加了以下内容

@racketblock[
(define p-sick (posn 'a 'b))
]

 @;{to the module. So, when you are looking for bugs based on contract
 violations, keep this example in mind.}
到模块中。所以，当你在寻找基于违反合同的bug时，记住这个例子。

@;{If we want to fix the contract for @racket[p-sick] so that the error
is caught when @racket[sick] is exported, a single change suffices:}
如果我们想修复@racket[p-sick]的合约这样的错误，它是当@racket[sick]被导出时被引发的，一个单独的改变就足够了：

@racketblock[
(provide
 (contract-out
  ...
  [p-sick (struct/c posn number? number?)]))
]

@;{That is, instead of exporting @racket[p-sick] as a plain
@racket[posn?], we use a @racket[struct/c] contract to enforce
constraints on its components.}
更确切地说，代替作为一个普通的@racket[posn?]导出@racket[p-sick]的方式，我们使用@racket[struct/c]合约对组件进行强制约束。

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "lazy-contracts"]{Checking Properties of Data Structures}}
@ctc-section[#:tag "lazy-contracts"]{检查数据结构的特性}

@;{Contracts written using @racket[struct/c] immediately
check the fields of the data structure, but sometimes this
can have disastrous effects on the performance of a program
that does not, itself, inspect the entire data structure.}
用@racket[struct/c]编写的合约会立即检查数据结构的字段，但是有时这会对程序本身的性能造成灾难性的影响，而这个程序本身并不检查整个数据结构。

@;{As an example, consider the binary search tree
search algorithm. A binary search tree is like a binary
tree, except that the numbers are organized in the tree to
make searching the tree fast. In particular, for each
interior node in the tree, all of the numbers in the left
subtree are smaller than the number in the node, and all of
the numbers in the right subtree are larger than the number
in the node.}
作为一个例子，考虑二叉搜索树搜索算法。一个二叉搜索树就像一个二叉树，除了这些数值被组织在树中，以便快速搜索树。特别是，对于树中的每个内部节点，左边子树中的所有数值都小于节点中的数值，而右子树中的所有数值都大于节点中的数值。

@;{We can implement a search function @racket[in?] that takes
advantage of the structure of the binary search tree.}
我们可以实现搜索函数@racket[in?]，它利用了二叉搜索树结构的优越性。

@racketmod[
racket

(struct node (val left right))
  
(code:comment @;{"determines if `n' is in the binary search tree `b',"}"确定二进制搜索树中是否存在“n”，")
(code:comment @;{"exploiting the binary search tree invariant"}"利用二进制搜索树不变。")
(define (in? n b)
  (cond
    [(null? b) #f]
    [else (cond
            [(= n (node-val b))
             #t]
            [(< n (node-val b))
             (in? n (node-left b))]
            [(> n (node-val b))
             (in? n (node-right b))])]))

(code:comment @;{"a predicate that identifies binary search trees"}"一个识别二进制搜索树的判断。")
(define (bst-between? b low high)
  (or (null? b)
      (and (<= low (node-val b) high)
           (bst-between? (node-left b) low (node-val b))
           (bst-between? (node-right b) (node-val b) high))))

(define (bst? b) (bst-between? b -inf.0 +inf.0))
  
(provide (struct-out node))
(provide (contract-out
          [bst? (any/c . -> . boolean?)]
          [in? (number? bst? . -> . boolean?)]))
]

@;{In a full binary search tree, this means that
the @racket[in?] function only has to explore a
logarithmic number of nodes.}
在一个完整的二叉搜索树中，这意味着@racket[in?]函数只需探索一个对数节点。

@;{The contract on @racket[in?] guarantees that its input
is a binary search tree. But a little careful thought
reveals that this contract defeats the purpose of the binary
search tree algorithm. In particular, consider the
inner @racket[cond] in the @racket[in?]
function. This is where the @racket[in?] function gets
its speed: it avoids searching an entire subtree at each
recursive call. Now compare that to the @racket[bst-between?]
function. In the case that it returns @racket[#t], it
traverses the entire tree, meaning that the speedup
of @racket[in?] is lost.}
对@racket[in?]的合约保证其输入是二叉搜索树。但经过仔细的思考发现，该合约违背了二叉搜索树算法的目的。特别是，考虑到@racket[in?]函数内部的@racket[cond]。这是@racket[in?]函数得到它的速度的地方：它避免在每次递归调用时搜索整个子树。现在把它与@racket[bst-between?]函数相比。在这种情况下，它返回@racket[#t]，它遍历整个树，意味@racket[in?]的加速没有实现。

@;{In order to fix that, we can employ a new strategy for
checking the binary search tree contract. In particular, if
we only checked the contract on the nodes
that @racket[in?] looks at, we can still guarantee that
the tree is at least partially well-formed, but without
changing the complexity.}
为了解决这个问题，我们可以采用一种新的策略来检查二叉搜索树合约。特别是，如果我们只检查了@racket[in?]接受的节点上的合约，我们仍然可以保证树至少部分地成形了，但是没有改变复杂性。

@;{To do that, we need to use @racket[struct/dc] to define
@racket[bst-between?]. Like @racket[struct/c], @racket[struct/dc] defines a
contract for a structure. Unlike
@racket[struct/c], it allows fields to be marked as lazy, so that
the contracts are only checked when the matching selector is called.
Also, it does not allow mutable fields to be marked as lazy.}
要做到这一点，我们需要使用@racket[struct/dc]定义@racket[bst-between?]。像@racket[struct/c]一样，@racket[struct/dc]定义了一个结构的合约。与@racket[struct/c]不同，它允许字段被标记为惰性，这样当匹配选择器被调用时，才检查这些合约。此外，它不允许将可变字段标记为惰性。

@;{The @racket[struct/dc] form accepts a contract for each
field of the struct and returns a contract on the
struct. More interestingly, @racket[struct/dc] allows us to write dependent
contracts, i.e., contracts where some of the contracts on
the fields depend on the values of other fields. We can use
this to define the binary search tree contract:}
@racket[struct/dc]表接受结构的每个字段的一个合约，并返回一个结构的合约。更有趣的是，@racket[struct/dc]允许我们编写依赖的合约，也就是说，某些字段上的合约依赖于其它字段的值。我们可以用这个去定义二叉搜索树合约：

@racketmod[
racket

(struct node (val left right))

(code:comment @;{"determines if `n' is in the binary search tree `b'"}"确定“n”是否在二进制搜索树“b”中")
(define (in? n b) ... as before ...)

(code:comment "bst-between : number number -> contract")
(code:comment "builds a contract for binary search trees")
(code:comment "whose values are between low and high")
(define (bst-between/c low high)
  (or/c null?
        (struct/dc node [val (between/c low high)]
                        [left (val) #:lazy (bst-between/c low val)]
                        [right (val) #:lazy (bst-between/c val high)])))

(define bst/c (bst-between/c -inf.0 +inf.0))

(provide (struct-out node))
(provide (contract-out
          [bst/c contract?]
          [in? (number? bst/c . -> . boolean?)]))
]

@;{In general, each use of @racket[struct/dc] must name the
fields and then specify contracts for each field. In the
above, the @racket[val] field is a contract that accepts
values between @racket[low] and @racket[high].
The @racket[left] and @racket[right] fields are
dependent on the value of the @racket[val] field,
indicated by their second sub-expressions. They are
also marked with the @racket[#:lazy] keyword to indicate
that they should be checked only when the appropriate
accessor is called on the struct instance. Their contracts
are built by recursive calls to
the @racket[bst-between/c] function. Taken together,
this contract ensures the same thing that
the @racket[bst-between?] function checked in the
original example, but here the checking only happens
as @racket[in?] explores the tree.}
一般来说，每个@racket[struct/dc]的使用都必须命名字段，然后为每个字段指定合约。在上面的@racket[val]字段是一个接受@racket[low]与@racket[high]之间的值的合约。@racket[left]和@racket[right]的字段依赖于它们的第二个子表达式所表示的@racket[val]值。他们也用@racket[#:lazy]关键字标记，以表明他们只有当合适的存取被结构实例调用应该被检查。它们的合约是通过递归调用@racket[bst-between/c]函数来构建的。综合起来，这个合约保证了@racket[bst-between?]函数在原始示例中检查同样的事情，但这里的检查只发生在@racket[in?]探索这个树时。

@;{Although this contract improves the performance
of @racket[in?], restoring it to the logarithmic
behavior that the contract-less version had, it is still
imposes a fairly large constant overhead. So, the contract
library also provides @racket[define-opt/c] that brings
down that constant factor by optimizing its body. Its shape
is just like the @racket[define] above. It expects its
body to be a contract and then optimizes that contract.}
虽然这个合约提高了@racket[in?]的性能，把它恢复到合约较少版本的对数行为上，但它仍然强加了相当大的恒定开销。因此，合约库还提供了@racket[define-opt/c]，它通过优化它的主体来降低常数因子。它的形态和上面的@racket[define]一样。它希望它的主体是一个合约，然后优化该合约。

@racketblock[
(define-opt/c (bst-between/c low high)
  (or/c null?
        (struct/dc node [val (between/c low high)]
                        [left (val) #:lazy (bst-between/c low val)]
                        [right (val) #:lazy (bst-between/c val high)])))
]

