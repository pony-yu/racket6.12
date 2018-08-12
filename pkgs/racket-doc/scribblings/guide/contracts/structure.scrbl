#lang scribble/doc
@(require scribble/manual scribble/eval "../guide-utils.rkt" "utils.rkt"
          (for-label racket/contract))

@;{@title[#:tag "contracts-struct"]{Contracts on Structures}}
@title[#:tag "contracts-struct"]{结构上的合约}

@;{Modules deal with structures in two ways. First they export
@racket[struct] definitions, i.e., the ability to create
structs of a certain kind, to access their fields, to modify
them, and to distinguish structs of this kind against every
other kind of value in the world. Second, on occasion a
module exports a specific struct and wishes to promise that
its fields contain values of a certain kind. This section
explains how to protect structs with contracts for both
uses.}
模块以两种方式处理结构。首先它们输出@racket[struct]的定义，即某种创造结构的资格、存取它们的字段的资格、修改它们的资格以及区别这种结构于世界上所有其它值的资格。其次，有时一个模块输出一个特定的结构并希望它的字段包含某种值。本节讲解如何使用合约保护结构的两种使用。

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "single-struct"]{Guarantees for a Specific Value}}
@ctc-section[#:tag "single-struct"]{确保一个特定值}

@;{If your module defines a variable to be a structure, then you can
specify the structure's shape using @racket[struct/c]:}
如果你的模块定义了一个变量做为一个结构，那么你可以使用@racket[struct/c]指定结构的形态：

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
在这个例子中，该模块输入一个代表位置的库，它输出一个@racket[posn]结构。@racket[posn]中的一个创建并输出所代表的网格原点，即@tt{(0,0)}。

@;{@margin-note{See also @racket[vector/c] and similar contract
combinators for (flat) compound data.}}
@margin-note{又见@racket[vector/c]及类似的对（扁平）复合数据的合约组合器。}

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "define-struct"]{Guarantees for All Values}}
@ctc-section[#:tag "define-struct"]{确保所有值}

@;{The book @|HtDP| teaches that @racket[posn]s should contain only
numbers in their two fields. With contracts we would enforce this
informal data definition as follows:}
《@|HtDP|》这本书教授了@racket[posn]应该只包含在它们两个字段里的数值。用合约我们可以执行以下这种非正式数据定义：

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
这个模块输出整个结构定义：@racket[posn]、@racket[posn?]、@racket[posn-x]、@racket[posn-y]、@racket[set-posn-x!]和@racket[set-posn-y!]。每个函数执行或承诺一个@racket[posn]结构的这两个字段是数值——当这些值穿过模块边界传递时。因此，如果一个客户端在@racket[10]和@racket['a]上调用@racket[posn]，这个合约系统就发出一个合约违反信号。

@;{The creation of @racket[p-sick] inside of the @racket[posn] module,
however, does not violate the contracts. The function @racket[posn] is
used internally, so @racket['a] and @racket['b] don't cross the module
boundary. Similarly, when @racket[p-sick] crosses the boundary of
@racket[posn], the contract promises a @racket[posn?] and nothing
else. In particular, this check does @italic{not} require that the
fields of @racket[p-sick] are numbers.}
然而，@racket[posn]模块内的@racket[p-sick]的创建，并没有违反该合约。这个函数@racket[posn]在内部使用，所以@racket['a]和@racket['b]不穿过模块边界。同样，当@racket[p-sick]穿过@racket[posn]的边界时，该合约承诺一个@racket[posn?]并且别的什么也没有。特别是，这个检查并@italic{没有}需要@racket[p-sick]的字段是数值。

@;{The association of contract checking with module boundaries implies that
@racket[p-okay] and @racket[p-sick] look alike from a client's
perspective until the client extracts the pieces:}
用模块边界的合约检查的联系意味着@racket[p-okay]和@racket[p-sick]从一个客户端的角度看起来相似，直到客户端选取了以下片断：

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
使用@racket[posn-x]是这个客户端能够找到一个@racket[posn]在@racket[x]字段里包含的内容的唯一途径。@racket[posn-x]的应用程序发送回@racket[p-sick]进入@racket[posn]模块以及发送回这个结果值——这里的@racket['a]——给客户端，再跨越模块边界。在这一点上，这个合约系统发现一个承诺被违反了。具体来说，@racket[posn-x]没有返回一个数值但却返回了一个符号，因此应该被归咎。

@;{This specific example shows that the explanation for a contract violation
doesn't always pinpoint the source of the error. The good news is that the
error is located in the @racket[posn] module. The bad news is that the
explanation is misleading. Although it is true that @racket[posn-x]
produced a symbol instead of a number, it is the fault of the programmer who
created a @racket[posn] from symbols, i.e., the programmer who added}
这个具体的例子表明，对一个违反合约的解释并不总是指明错误的来源。好消息是，这个错误被定位在@racket[posn]模块内。坏消息是，这种解释是误导性的。虽然@racket[posn-x]产生了一个符号而不是一个数值是真的，它是从符号创建了@racket[posn]的这个程序员的责任，亦即这个程序员添加了

@racketblock[
(define p-sick (posn 'a 'b))
]

 @;{to the module. So, when you are looking for bugs based on contract
 violations, keep this example in mind.}
到这个模块中。所以，当你在寻找基于违反合约的bug时，把这个例子记在心里。

@;{If we want to fix the contract for @racket[p-sick] so that the error
is caught when @racket[sick] is exported, a single change suffices:}
如果我们想修复@racket[p-sick]的合约以便在@racket[sick]被输出时这个错误被捕获，一个单一的改变就足够了：

@racketblock[
(provide
 (contract-out
  ...
  [p-sick (struct/c posn number? number?)]))
]

@;{That is, instead of exporting @racket[p-sick] as a plain
@racket[posn?], we use a @racket[struct/c] contract to enforce
constraints on its components.}
更确切地说，代替作为一个直白的@racket[posn?]的输出@racket[p-sick]，我们使用一个@racket[struct/c]合约来执行对其组件的约束。

@; ----------------------------------------------------------------------
@;{@ctc-section[#:tag "lazy-contracts"]{Checking Properties of Data Structures}}
@ctc-section[#:tag "lazy-contracts"]{检查数据结构的特性}

@;{Contracts written using @racket[struct/c] immediately
check the fields of the data structure, but sometimes this
can have disastrous effects on the performance of a program
that does not, itself, inspect the entire data structure.}
用@racket[struct/c]编写的合约立即检查数据结构的字段，但是有时这能够对一个程序的性能具有灾难性的影响，这个程序本身并不检查整个数据结构。

@;{As an example, consider the binary search tree
search algorithm. A binary search tree is like a binary
tree, except that the numbers are organized in the tree to
make searching the tree fast. In particular, for each
interior node in the tree, all of the numbers in the left
subtree are smaller than the number in the node, and all of
the numbers in the right subtree are larger than the number
in the node.}
作为一个例子，考虑二叉搜索树搜索算法。一个二叉搜索树就像一个二叉树，除了这些数值被组织在树中以便快速搜索这个树。特别是，对于树中的每个内部节点，左边子树中的所有数值都小于节点中的数值，同时右子树中的所有数值都大于节点中的数值。

@;{We can implement a search function @racket[in?] that takes
advantage of the structure of the binary search tree.}
我们可以实现一个搜索函数@racket[in?]，它利用二叉搜索树结构的优势。

@racketmod[
racket

(struct node (val left right))
  
@;{(code:comment "determines if `n' is in the binary search tree `b',")
(code:comment "exploiting the binary search tree invariant")}
 (code:comment "利用二叉搜索树不变量，")
 (code:comment "确定二叉搜索树“b”中是否存在“n”。")

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

(code:comment @;{"a predicate that identifies binary search trees"}"一个识别二叉搜索树的判断。")
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
对@racket[in?]的合约保证其输入是一个二叉搜索树。但仔细的思考表明，该合约违背了二叉搜索树算法的目的。特别是，考虑到@racket[in?]函数里内部的@racket[cond]。这是@racket[in?]函数获取其速度的地方：它避免在每次递归调用时搜索整个子树。现在把它与@racket[bst-between?]函数比较。在这种情况下它返回@racket[#t]，它遍历整个树，意味@racket[in?]的加速没有实现。

@;{In order to fix that, we can employ a new strategy for
checking the binary search tree contract. In particular, if
we only checked the contract on the nodes
that @racket[in?] looks at, we can still guarantee that
the tree is at least partially well-formed, but without
changing the complexity.}
为了解决这个问题，我们可以利用一种新的策略来检查这个二叉搜索树合约。特别是，如果我们只检查了@racket[in?]看着的节点上的合约，我们仍然可以保证这个树至少部分形成良好，但是没有改变复杂性。

@;{To do that, we need to use @racket[struct/dc] to define
@racket[bst-between?]. Like @racket[struct/c], @racket[struct/dc] defines a
contract for a structure. Unlike
@racket[struct/c], it allows fields to be marked as lazy, so that
the contracts are only checked when the matching selector is called.
Also, it does not allow mutable fields to be marked as lazy.}
要做到这一点，我们需要使用@racket[struct/dc]来定义@racket[bst-between?]。像@racket[struct/c]一样，@racket[struct/dc]为一个结构定义一个合约。与@racket[struct/c]不同，它允许字段被标记为惰性，这样当匹配选择器被调用时，这些合约才被检查。同样，它不允许将可变字段被标记为惰性。

@;{The @racket[struct/dc] form accepts a contract for each
field of the struct and returns a contract on the
struct. More interestingly, @racket[struct/dc] allows us to write dependent
contracts, i.e., contracts where some of the contracts on
the fields depend on the values of other fields. We can use
this to define the binary search tree contract:}
@racket[struct/dc]表接受这个结构的每个字段的一个合约并返回结构上的一个合约。更有趣的是，@racket[struct/dc]允许我们编写依赖合约，也就是说，合约中的某些合约取决于其它字段。我们可以用这个去定义二叉搜索树合约：

@racketmod[
racket

(struct node (val left right))

(code:comment @;{"determines if `n' is in the binary search tree `b'"}"确定“n”是否在二进制搜索树“b”中")
(define (in? n b) ... as before ...)

(code:comment "bst-between : number number -> contract")
;(code:comment "builds a contract for binary search trees")
(code:comment "构建了一个二叉搜索树合约")
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
一般来说，@racket[struct/dc]的每个使用都必须命名字段并且接着为每个字段指定合约。在上面，@racket[val]字段是一个接受@racket[low]与@racket[high]之间的值的合约。@racket[left]和@racket[right]字段依赖于@racket[val]的值，被它们的第二个子表达式所表示。他们也用@racket[#:lazy]关键字标记以表明它们只有当合适的存取器被结构实例被调用时应该被检查。它们的合约是通过递归调用@racket[bst-between/c]函数来构建的。综合起来，这个合约确保了在原始示例中被检查的@racket[bst-between?]函数的同样的事情，但这里这个检查只发生在@racket[in?]探索这个树时。

@;{Although this contract improves the performance
of @racket[in?], restoring it to the logarithmic
behavior that the contract-less version had, it is still
imposes a fairly large constant overhead. So, the contract
library also provides @racket[define-opt/c] that brings
down that constant factor by optimizing its body. Its shape
is just like the @racket[define] above. It expects its
body to be a contract and then optimizes that contract.}
虽然这个合约提高了@racket[in?]的性能，把它恢复到无合约版本的对数行为上，但它仍然施加相当大的恒定开销。因此，这个合约库也提供@racket[define-opt/c]，它通过优化其主体来降低常数因子。它的形态和上面的@racket[define]一样。它希望它的主体是一个合约并且接着优化该合约。

@racketblock[
(define-opt/c (bst-between/c low high)
  (or/c null?
        (struct/dc node [val (between/c low high)]
                        [left (val) #:lazy (bst-between/c low val)]
                        [right (val) #:lazy (bst-between/c val high)])))
]

