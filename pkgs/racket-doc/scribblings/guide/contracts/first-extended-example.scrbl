#lang scribble/doc

@(require scribble/manual scribble/core scribble/eval
          "utils.rkt"
          (only-in racket/list argmax)
          (for-label racket/contract))

@;(require "shared.rkt" (only-in racket/list argmax))

@;{@title[#:tag "contracts-first"]{Contracts: A Thorough Example}}
@title[#:tag "contracts-first"]{合约：一个完整的例子}

@;{This section develops several different flavors of contracts for one and
 the same example: Racket's @racket[argmax] function. According to
 its Racket documentation, the function  consumes a procedure @racket[proc] and
 a non-empty list of values, @racket[lst]. It 
@nested[#:style 'inset]{
 returns the @emph{first} element in the list @racket[lst] that maximizes
 the result of @racket[proc].}
 The emphasis on @emph{first} is ours.}
本节开发对于同一个例子的合约的几种不同特点：Racket的@racket[argmax]函数。根据它的Racket文档，这个函数接受一个过程@racket[proc]和一个非空的值列表，@racket[lst]。它@nested[#:style 'inset]{返回在最大化@racket[proc]的结果的列表@racket[lst]中的@emph{first}元素。对@emph{first}的强调是我们的。}

@;{Examples: }
例子：
@interaction[#:eval ((make-eval-factory (list 'racket)))
(argmax add1 (list 1 2 3)) 
(argmax sqrt (list .4 .9 .16))
(argmax second '((a 2) (b 3) (c 4) (d 1) (e 4)))
]

@;{Here is the simplest possible contract for this function:}
这里是这个函数的可能最简单的合约：

@racketmod[#:file @tt{version 1}
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax (-> (-> any/c real?) (and/c pair? list?) any/c)]))
]

 @;{This contract captures two essential conditions of the informal
 description of @racket[argmax]: }
这个合约捕捉@racket[argmax]的非正式描述的两个必备条件：

@itemlist[

@item{
  @;{the given function must produce numbers that are comparable according
to @racket[<]. In particular, the contract @racket[(-> any/c number?)]
would not do, because @racket[number?] also recognizes complex numbers in
Racket.}
    这个给定的函数必须产生按@racket[<]进行比较的数值。特别是，这个合约@racket[(-> any/c number?)]不可行，因为@racket[number?]也承认Racket中的复数有效。
    }

@item{
  @;{the given list must contain at least one item.}
    给定列表必须至少包含一项。
    }
]

 @;{When combined with the name, the contract explains the behavior of 
 @racket[argmax] at the same level as an ML function type in a
 module signature (except for the non-empty list aspect). }
当组合名称时，合约解释在同级的@racket[argmax]的行为作为在一个模块签名（除空表方面外）中的一个ML（机器语言）函数类型。

@;{Contracts may communicate significantly more than a type signature,
 however. Take a look at this second contract for @racket[argmax]:}
然而，合约可能比一个类型签名更值得关注。看一看@racket[argmax]的第二个合约：

@racketmod[#:file @tt{version 2}
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) () 
         (r (f lov)
            (lambda (r)
              (define f@r (f r))
              (for/and ([v lov]) (>= f@r (f v))))))]))
]

 @;{It is a @emph{dependent} contract that names the two arguments and uses
 the names to impose a predicate on the result. This predicate computes
 @racket[(f r)] -- where @racket[r] is the result of @racket[argmax] -- and
 then validates that this value is greater than or equal to all values
 of @racket[f] on the items of @racket[lov].}
它是一个@emph{依赖}合约，它命名两个参数并使用这个名称在结果上添加一个判断。这个判断计算 @racket[(f r)]——这里@racket[r]是@racket[argmax]的结果——并接着验证这个值大于或等于在@racket[lov]的项目上的所有@racket[f]值。

@;{Is it possible that @racket[argmax] could cheat by returning a random value
 that accidentally maximizes @racket[f] over all elements of @racket[lov]? 
 With a contract, it is possible to rule out this possibility: }
这是可能的吗？——@racket[argmax]会通过返回一个随机值作弊，这个随机值意外地最大化@racket[f]超过@racket[lov]的所有元素。用一个合约，就有可能排除这种可能性：

@racketmod[#:file @tt{version 2 rev. a}
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) () 
         (r (f lov)
            (lambda (r)
              (define f@r (f r))
              (and (memq r lov)
                   (for/and ([v lov]) (>= f@r (f v)))))))]))
]

 @;{The @racket[memq] function ensures that @racket[r] is @emph{intensionally equal}
 @margin-note*{That is, "pointer equality" for those who prefer to think at
 the hardware level.} to one of the members of @racket[lov]. Of course, a
 moment's worth of reflection shows that it is impossible to make up such a
 value. Functions are opaque values in Racket and without applying a
 function, it is impossible to determine whether some random input value
 produces an output value or triggers some exception. So we ignore this
 possibility from here on.}
@racket[memq]函数确保@racket[r]是@emph{相等（intensionally equal）}@margin-note*{也就是说，那些喜欢在硬件层面思考的人的“指针相等（pointer equality）”。}于@racket[lov]的其中一个成员。当然，片刻的反思显露出要构成这样一个值是不可能的。函数是Racket中的不透明值，并且没有应用一个函数，无法确定某个随机输入值是否产生一个输出值或触发某些异常。因此我们从这里开始忽略这种可能性。

@;{Version 2 formulates the overall sentiment of @racket[argmax]'s
 documentation, but it fails to bring across that the result is the
 @emph{first} element of the given list that maximizes the given function
 @racket[f]. Here is a version that communicates this second aspect of
 the informal documentation: }
版本2确切地阐述了@racket[argmax]文档的整体观点，但它没能传达出这个结果是这个给定的最大化给定的函数@racket[f]的列表的@emph{第一个}元素。这是一个传达这个非正式文档的第二个方面的版本：

@racketmod[#:file @tt{version 3} 
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
         (r (f lov)
            (lambda (r)
              (define f@r (f r))
              (and (for/and ([v lov]) (>= f@r (f v)))
                   (eq? (first (memf (lambda (v) (= (f v) f@r)) lov)) 
                        r)))))]))
]

 @;{That is, the @racket[memf] function determines the first element of
 @racket[lov] whose value under @racket[f] is equal to @racket[r]'s value
 under @racket[f]. If this element is intensionally equal to @racket[r],
 the result of @racket[argmax] is correct.}
那就是，@racket[memf]函数确定@racket[lov]的第一个元素，@racket[f]下的@racket[lov]的值等于@racket[f]下的@racket[r]的值。如果此元素是有意等于@racket[r]，@racket[argmax]的结果就是正确的。

@;{This second refinement step introduces two problems. First, both conditions
 recompute the values of @racket[f] for all elements of @racket[lov]. Second,
 the contract is now quite difficult to read. Contracts should have a concise
 formulation that a client can comprehend with a simple scan. Let us
 eliminate the readability problem with two auxiliary functions that have
 reasonably meaningful names: }
第二个细化步骤介绍了两个问题。首先，条件都重新计算@racket[lov]的所有元素的@racket[f]的值。第二，这个合约现在很难阅读。合约应该有一个简洁的表达方式，它可以让一个客户端可以用一个简单的扫描进行理解。让我们用具有合理意义的名称的两个辅助函来数消除可读性问题：

@(define dominates1
  @multiarg-element['tt]{@list{
   @;{@racket[f@r] is greater or equal to all @racket[(f v)] for @racket[v] in @racket[lov]}
   @racket[f@r]大于或等于在@racket[lov]中@racket[v]的所有@racket[(f v)]}})

@(define first?1
  @multiarg-element['tt]{@list{
   @;{@racket[r] is @racket[eq?] to the first element @racket[v] of @racket[lov] 
         for which @racket[(pred? v)]}
@racket[r]是@racket[eq?]于@racket[lov]的第一个元素@racket[v]，因为它的@racket[(pred? v)]}})

@; ---------------------------------------------------------------------------------------------------
@racketmod[#:file @tt{version 3 rev. a} 
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
         (r (f lov)
            (lambda (r)
              (define f@r (f r))
              (and (is-first-max? r f@r f lov)
                   (dominates-all f@r f lov)))))]))

@code:comment{@;{where}这里}

@code:comment{@#,dominates1}
(define (dominates-all f@r f lov)
  (for/and ([v lov]) (>= f@r (f v))))

@code:comment{@#,first?1}
(define (is-first-max? r f@r f lov)
  (eq? (first (memf (lambda (v) (= (f v) f@r)) lov)) r))
]

 @;{The names of the two predicates express their functionality and, in
 principle, render it unnecessary to read their definitions. }
原则上，这两个判断的名称表示它们的功能和表达不需要读取它们的定义。

@;{This step leaves us with the problem of the newly introduced inefficiency.
 To avoid the recomputation of @racket[(f v)] for all @racket[v] on
 @racket[lov], we change the contract so that it computes these values and
 reuses them as needed:}
这一步给我们带来了新引进的低效率问题。为了避免因@racket[lov]上的所有 @racket[v]引起的@racket[(f v)]的重复计算，我们改变合约以致其计算这些值和重用它们是必要的：

@(define dominates2
  @multiarg-element['tt]{@list{
   @;{@racket[f@r] is greater or equal to all @racket[f@v] in @racket[flov]}
  @racket[f@r]大于或等于@racket[flov]中所有的@racket[f@v]}})

@(define first?2
  @multiarg-element['tt]{@list{
   @;{@racket[r] is @racket[(first x)] for the first
         @racket[x] in @racket[lov+flov] s.t. @racket[(= (second x) f@r)]}
 @racket[r]是@racket[lov+flov]里第一个@racket[x]的@racket[(first x)]，整理为@racket[(= (second x) f@r)]}})

@racketmod[#:file @tt{version 3 rev. b} 
racket

(define (argmax f lov) ...)

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
         (r (f lov)
            (lambda (r)
              (define f@r (f r))
              (define flov (map f lov))
              (and (is-first-max? r f@r (map list lov flov))
                   (dominates-all f@r flov)))))]))

@code:comment{@;{where}这里}

@code:comment{@#,dominates2}
(define (dominates-all f@r flov)
  (for/and ([f@v flov]) (>= f@r f@v)))

@code:comment{@#,first?2}
(define (is-first-max? r f@r lov+flov)
  (define fst (first lov+flov))
  (if (= (second fst) f@r)
      (eq? (first fst) r)
      (is-first-max? r f@r (rest lov+flov))))
]

 @;{Now the predicate on the result once again computes all values of @racket[f]
 for elements of @racket[lov] once. }
现在对结果的判断为@racket[lov]的元素再次计算了@racket[f]的所有值一次。

@;{@margin-note{The word "eager" comes from the literature on the linguistics
 of contracts.}}
@margin-note{单词“eager（热切，急切）”来自于合约语言学文献。}

@;{Version 3 may still be too eager when it comes to calling @racket[f]. While
 Racket's @racket[argmax] always calls @racket[f] no matter how many items
 @racket[lov] contains, let us imagine for illustrative purposes that our
 own implementation first checks whether the list is a singleton.  If so,
 the first element would be the only element of @racket[lov] and in that
 case there would be no need to compute @racket[(f r)].
@margin-note*{The @racket[argmax] of Racket implicitly argues that it not
 only promises the first value that maximizes @racket[f] over @racket[lov]
 but also that @racket[f] produces/produced a value for the result.}
 As a matter of fact, since @racket[f] may diverge or raise an exception
 for some inputs, @racket[argmax] should avoid calling @racket[f] when
 possible.}
当版本3去调用@racket[f]时也许还太急切。然而无论@racket[lov]包含有多少成员，Racket的@racket[argmax]总是调用@racket[f]，让我们想象一下，为了说明目的，我们自己的实现首先检查列表是否是单体。如果是这样，第一个元素将是@racket[lov]的唯一元素，在这种情况下就不需要计算@racket[(f r)]。@margin-note*{Racket的@racket[argmax]隐含论证它不仅承诺第一个值，它最大化@racket[f]超过@racket[lov]但同样@racket[f]产生一个结果的值。}
事实上，由于@racket[f]可能发散或增加一些例外输入，argmax应该尽可能避免调用@racket[f]。

@;{The following contract demonstrates how a higher-order dependent contract
 needs to be adjusted so as to avoid being over-eager: }
下面的合约演示了如何调整高阶依赖合约，以避免过度依赖：

@racketmod[#:file @tt{version 4} 
racket

(define (argmax f lov) 
  (if (empty? (rest lov))
      (first lov)
      ...))

(provide
 (contract-out
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
         (r (f lov)
            (lambda (r)
              (cond
                [(empty? (rest lov)) (eq? (first lov) r)]
                [else
                 (define f@r (f r))
                 (define flov (map f lov))
                 (and (is-first-max? r f@r (map list lov flov))
                      (dominates-all f@r flov))]))))]))

@code:comment{where}

@code:comment{@#,dominates2}
(define (dominates-all f@r lov) ...)

@code:comment{@#,first?2}
(define (is-first-max? r f@r lov+flov) ...)
]

 @;{Note that such considerations don't apply to the world of first-order
 contracts. Only a higher-order (or lazy) language forces the programmer to
 express contracts with such precision.}
注意，这种考虑不适用于一阶合同的世界。只有一个高阶（或惰性）语言迫使程序员去以如此精确度去表达合约。
 
@;{The problem of diverging or exception-raising functions should alert the
 reader to the even more general problem of functions with side-effects. If
 the given function @racket[f] has visible effects -- say it logs its calls
 to a file -- then the clients of @racket[argmax] will be able to observe
 two sets of logs for each call to @racket[argmax]. To be precise, if the
 list of values contains more than one element, the log will contain two
 calls of @racket[f] per value on @racket[lov]. If @racket[f] is expensive
 to compute, doubling the calls imposes a high cost.}
发散或异常提升函数的问题应该让读者对带副作用的函数的一般性问题保持警惕。如果这个给定的函数@racket[f]有明显的影响——表明它把它的调用记录到了一个文件——那么@racket[argmax]的客户端将能够观察每次调用@racket[argmax]的两套日志。确切地讲，如果值列表包含多个元素，这个日志将包含@racket[lov]上的每一个值的两个@racket[f]调用。如果@racket[f]对于计算来说太昂贵，则加倍调用承受一个高成本。

@;{To avoid this cost and to signal problems with overly eager contracts, a
 contract system could record the i/o of contracted function arguments and
 use these hashtables in the dependency specification. This is a topic of
 on-going research in PLT. Stay tuned. }
用过度热切的合约来避免这种成本以及来标志问题，一个合约系统可以记录已约定的函数参数的i/o并使用这些散列表的相关规范。这是PLT研究中的一个课题。敬请关注。

@;{one could randomly check some element here, instead of all of them and
thus ensure 'correctness' at 1/(length a) probability}
