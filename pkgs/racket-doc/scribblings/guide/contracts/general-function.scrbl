#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label framework/framework racket/contract racket/gui))

@;{@title[#:tag "contracts-general-functions"]{Contracts on Functions in General}}
@title[#:tag "contracts-general-functions"]{一般功能合约}

@;{The @racket[->] contract constructor works for functions that take a
fixed number of arguments and where the result contract is independent
of the input arguments. To support other kinds of functions, Racket
supplies additional contract constructors, notably @racket[->*] and 
@racket[->i].}
@racket[->]合约构造器为带有一个固定数量参数的函数工作，并且这里这个结果合约不依赖于这个输入参数。为了支持其它类型的函数，Racket提供额外的合约构造器，尤其是 @racket[->*]和@racket[->i]。

@;{@ctc-section[#:tag "optional"]{Optional Arguments}}
@ctc-section[#:tag "optional"]{可选参数}

@;{Take a look at this excerpt from a string-processing module, inspired by the
@link["http://schemecookbook.org"]{Scheme cookbook}: }
请看一个字符串处理模块的摘录，该灵感来自于《@link["http://schemecookbook.org"]{Scheme cookbook}》：

@racketmod[
racket

(provide
 (contract-out
  (code:comment @#,t{@;{pad the given str left and right with}用（可选的）char填充给定的左右两个str以使其左右居中})
  (code:comment @#,t{@;{the (optional) char so that it is centered}})
  [string-pad-center (->* (string? natural-number/c)
                          (char?) 
                          string?)]))

(define (string-pad-center str width [pad #\space])
  (define field-width (min width (string-length str)))
  (define rmargin (ceiling (/ (- width field-width) 2)))
  (define lmargin (floor (/ (- width field-width) 2)))
  (string-append (build-string lmargin (λ (x) pad))
                 str
                 (build-string rmargin (λ (x) pad))))
]

 @;{The module exports @racket[string-pad-center], a function
 that creates a string of a given @racket[width] with the
 given string in the center. The default fill character is
 @racket[#\space]; if the client module wishes to use a
 different character, it may call @racket[string-pad-center]
 with a third argument, a @racket[char], overwriting the
 default.}
这个模块输出@racket[string-pad-center]，一个函数，它在中心用给定字符串创建一个给定的@racket[width]的一个字符串。这个默认的填充字符是@racket[#\space]；如果这个客户端模块希望使用一个不同的字符，它可以用第三个参数——一个重写默认值的@racket[char]——调用@racket[string-pad-center]。

@;???????????????????????????????????????????????????????????
@;{The function definition uses optional arguments, which is
appropriate for this kind of functionality. The interesting
point here is the formulation of the contract for the
@racket[string-pad-center].}
这个函数定义使用可选参数，这对于这种功能是合适的。这里有趣的一点是@racket[string-pad-center]的合约。

@;{The contract combinator @racket[->*], demands several groups of contracts: }
合约组合@racket[->*]，要求几组合约：

@itemize[
@item{
  @;{The first one is a parenthesized group of contracts for all required
arguments. In this example, we see two: @racket[string?] and
@racket[natural-number/c]. }
第一个是一个对所有必需参数合约的括号组。在这个例子中，我们看到两个：@racket[string?]和@racket[natural-number/c]。
    }

@item{
  @;{The second one is a parenthesized group of contracts for all optional
arguments: @racket[char?]. }
    第二个是对所有可选参数合约的括号组：@racket[char?]。
    }

@item{
  @;{The last one is a single contract: the result of the function.}
    最后一个是一个单一的合约：函数的结果。
    }
]

 @;{Note that if a default value does not satisfy a contract, you won't get a
 contract error for this interface. If you can't trust yourself to get
 the initial value right, you need to communicate the initial value
 across a boundary.}
请注意，如果默认值不满足合约，则不会获得此接口的合约错误。如果不能信任你自己去正确获得初始值，则需要在边界上传递初始值。

@;{@ctc-section[#:tag "rest-args"]{Rest Arguments}}
@ctc-section[#:tag "rest-args"]{剩余参数}

@;{The @racket[max] operator consumes at least one real number, but it
 accepts any number of additional arguments. You can write other such
 functions using a @tech{rest argument}, such as in @racket[max-abs]:}
@racket[max]操作符至少接受一个实数，但它接受任意数量的附加参数。您可以使用@tech{剩余参数（rest argument）}编写其它此类函数，例如在@racket[max-abs]中：

@;{@margin-note{See @secref["rest-args"] for an introduction to rest
arguments.}}
@margin-note{参见@secref["rest-args"]以获取剩余参数的介绍。}

@racketblock[
(define (max-abs n . rst)
  (foldr (lambda (n m) (max (abs n) m)) (abs n) rst))
]

@;{Describing this function through a contract requires a further
extension of @racket[->*]: a @racket[#:rest] keyword specifies a
contract on a list of arguments after the required and optional
arguments:}
通过一个合同描述此函数需要进一步扩展@racket[->*]：一个@racket[#:rest]关键字在必需参数和可选参数之后指定一个参数列表合约：

@racketblock[
(provide
 (contract-out
  [max-abs (->* (real?) () #:rest (listof real?) real?)]))
]

@;{As always for @racket[->*], the contracts for the required arguments
are enclosed in the first pair of parentheses, which in this case is a
single real number. The empty pair of parenthesis indicates that there
are no optional arguments (not counting the rest arguments). The
contract for the rest argument follows @racket[#:rest]; since all
additional arguments must be real numbers, the list of rest arguments
must satisfy the contract @racket[(listof real?)].}
正如对@racket[->*]的通常情况，必需参数合约被封闭在第一对括号中，在这种情况下是一个实数。空括号表示没有可选参数（不包括剩余参数）。剩余参数合约如下@racket[#:rest]；因为所有的额外的参数必须是实数，剩余参数列表必须满足合约@racket[(listof real?)]。

@;{@ctc-section[#:tag "keywords"]{Keyword Arguments}}
@ctc-section[#:tag "keywords"]{关键字参数}

@;{It turns out that the @racket[->] contract constructor also contains
support for keyword arguments. For example, consider this function,
which creates a simple GUI and asks the user a yes-or-no question:}
原来@racket[->]合约构造函数也包含对关键字参数的支持。例如，考虑这个函数，它创建一个简单的GUI并向用户询问一个“yes-or-no”的问题：

@;{@margin-note{See @secref["lambda-keywords"] for an introduction to
keyword arguments.}}
@margin-note{参见@secref["lambda-keywords"]以获取关键字参数的介绍。}

@racketmod[
racket/gui

(define (ask-yes-or-no-question question 
                                #:default answer
                                #:title title
                                #:width w
                                #:height h)
  (define d (new dialog% [label title] [width w] [height h]))
  (define msg (new message% [label question] [parent d]))
  (define (yes) (set! answer #t) (send d show #f))
  (define (no) (set! answer #f) (send d show #f))
  (define yes-b (new button% 
                     [label "Yes"] [parent d] 
                     [callback (λ (x y) (yes))]
                     [style (if answer '(border) '())]))
  (define no-b (new button% 
                    [label "No"] [parent d] 
                    [callback (λ (x y) (no))]
                    [style (if answer '() '(border))]))
  (send d show #t)
  answer)

(provide (contract-out
          [ask-yes-or-no-question
           (-> string?
               #:default boolean?
               #:title string?
               #:width exact-integer?
               #:height exact-integer?
               boolean?)]))
]

@;{@margin-note{If you really want to ask a yes-or-no question
via a GUI, you should use @racket[message-box/custom]. For that
matter, it's usually better to provide buttons with more specific
answers than ``yes'' and ``no.''}}
@margin-note{如果你真的想通过一个GUI问一个“yes”或“no”的问题，你应该使用@racket[message-box/custom]。在这个问题上，通常会提供比“yes”和“no”更具体的答案的按钮。}

@;{The contract for @racket[ask-yes-or-no-question] uses @racket[->], and
in the same way that @racket[lambda] (or @racket[define]-based
functions) allows a keyword to precede a functions formal argument,
@racket[->] allows a keyword to precede a function contract's argument
contract. In this case,
the contract says that @racket[ask-yes-or-no-question] must receive four keyword
arguments, one for each of the keywords
@racket[#:default],
@racket[#:title],
@racket[#:width], and
@racket[#:height]. 
As in a function definition, the order of the keywords in @racket[->]
relative to each other does not matter for clients of the function;
only the relative order of argument contracts without keywords
matters.}
@racket[ask-yes-or-no-question]的合同使用@racket[->]，同样的方式，@racket[lambda]（或基于@racket[define]的函数）允许关键字在函数正式参数之前，@racket[->]允许关键字先于函数合约的参数合约。在这种情况下，合约表明@racket[ask-yes-or-no-question]必须得到四个关键字参数，各个关键字为：@racket[#:default]、@racket[#:title]、@racket[#:width]和@racket[#:height]。在函数定义中，函数中的关键字之间的@racket[->]相对顺序对函数的客户机并不重要；只有没有关键字的参数合约的相对顺序。

@;{@ctc-section[#:tag "optional-keywords"]{Optional Keyword Arguments}}
@ctc-section[#:tag "optional-keywords"]{可选关键字参数}

@;{Of course, many of the parameters in
@racket[ask-yes-or-no-question] (from the previous question)
have reasonable defaults and should be made optional:}
当然，@racket[ask-yes-or-no-question]（从上一个问题中引来）中有许多参数有合理的默认值，应该是可选的：

@racketblock[
(define (ask-yes-or-no-question question 
                                #:default answer
                                #:title [title "Yes or No?"]
                                #:width [w 400]
                                #:height [h 200])
  ...)
]

@;{To specify this function's contract, we need to use
@racket[->*] again. It supports keywords just as you might
expect in both the optional and mandatory argument
sections. In this case, we have the mandatory keyword
@racket[#:default] and optional keywords
@racket[#:title],
@racket[#:width], and
@racket[#:height]. So, we write the contract like this:}
要指定这个函数的合约，我们需要再次使用@racket[->*]。它支持关键字，正如你在可选参数和强制参数部分中所期望的一样。在这种情况下，我们有强制关键字@racket[#:default]和可选关键字@racket[#:title]、@racket[#:width]和@racket[#:height]。所以，我们像这样写合约：

@racketblock[
(provide (contract-out
          [ask-yes-or-no-question
           (->* (string?
                 #:default boolean?)
                (#:title string?
                 #:width exact-integer?
                 #:height exact-integer?)

                boolean?)]))
]

@;{That is, we put the mandatory keywords in the first section, and we
put the optional ones in the second section.}
也就是说，我们在第一节中使用了强制关键字，并在第二部分中选择了可选关键字。

@;{@ctc-section[#:tag "case-lambda"]{Contracts for @racket[case-lambda]}}
@ctc-section[#:tag "case-lambda"]{对@racket[case-lambda]的合约}

@;{A function defined with @racket[case-lambda] might impose different
constraints on its arguments depending on how many are provided. For
example, a @racket[report-cost] function might convert either a pair
of numbers or a string into a new string:}
用@racket[case-lambda]定义的函数可能对其参数施加不同的约束，这取决于其提供了多少。例如，@racket[report-cost]函数可以将一对数字或字符串转换为一个新字符串：

@;{@margin-note{See @secref["case-lambda"] for an introduction to
@racket[case-lambda].}}
@margin-note{参见@secref["case-lambda"]以获得@racket[case-lambda]的介绍。}

@def+int[
(define report-cost
  (case-lambda
    [(lo hi) (format "between $~a and $~a" lo hi)]
    [(desc) (format "~a of dollars" desc)]))
(report-cost 5 8)
(report-cost "millions")
]

@;{The contract for such a function is formed with the @racket[case->]
 combinator, which combines as many functional contracts as needed: }
合约对这样的函数用@racket[case->]构成组合，这种结合对多个函数合约是必要的：

@racketblock[
(provide (contract-out
          [report-cost
           (case->
            (integer? integer? . -> . string?)
            (string? . -> . string?))]))
]
 @;{As you can see, the contract for @racket[report-cost] combines two
 function contracts, which is just as many clauses as the explanation
 of its functionality required.}
如你所见，@racket[report-cost]合约合并了两个函数合约，这与解释其函数所需的子句一样多。

@;{
This isn't supported anymore (yet...?). -robby

In the case of @racket[substring1], we also know that the indices
  that it consumes ought to be natural numbers less than the length of the
  given string. Since @racket[case->] just combines arrow contracts,
  adding such constraints is just a matter of strengthening the individual
  contracts: 
<racket>
(provide
 (contract-out
  [substring1 (case->
               (string? . -> . string?)
               (->r ([s string?]
                     [_ (and/c natural-number/c (</c (string-length s)))])
                 string?)
               (->r ([s string?]
                     [a (and/c natural-number/c (</c (string-length s)))]
                     [o (and/c natural-number/c
                               (>=/c a)
                               (</c (string-length s)))])
                  string?))]))
</racket>
  Here we used @racket[->r] to name the parameters and express the
  numeric constraints on them. 
}

@;{@ctc-section[#:tag "arrow-d"]{Argument and Result Dependencies}}
@ctc-section[#:tag "arrow-d"]{参数和结果的依赖}

@;{The following is an excerpt from an imaginary numerics module:}
以下是来自一个虚构的数值模块的摘录：

@racketblock[
(provide
 (contract-out
  [real-sqrt (->i ([argument (>=/c 1)])
                  [result (argument) (<=/c argument)])]))
]
 
@margin-note{
 @;{The word ``indy'' is meant to suggest that blame may be
 assigned to the contract itself, because the contract must be considered an
 independent component. The name was chosen in
 response to two existing labels---``lax'' and ``picky''---for different
 semantics of function contracts in the research literature.}
这个词“indy”意味着责任可能被分配到合约本身，因为合约必须被认为是一个独立的组件。这个名字是根据两个现有的标签而来的——“lax”和“picky”——具有与研究文献中函数契约不同语义。
}

@;{The contract for the exported function @racket[real-sqrt] uses the
@racket[->i] rather than @racket[->*] function contract. The ``i''
stands for an @italic{indy dependent} contract, meaning the contract for the
function range depends on the value of the argument. The appearance
of @racket[argument] in the line for @racket[result]'s contract means
that the result depends on the argument. In this
particular case, the argument of @racket[real-sqrt] is greater or
equal to 1, so a very basic correctness check is that the result is
smaller than the argument.}
为导出函数@racket[real-sqrt]的合约，使用@racket[->i]函数合约比@racket[->*]更好。“i”代表是一个@italic{印依赖合约（indy dependent contract）}（意味着责任可能被分配到合同本身，因为合同必须被认为是一个独立的组件），意味着作用范围的合约取决于该参数的值。为了@racket[result]的合约的实现在程序行中出现@racket[argument]意味着结果取决于参数。在特别情况下，@racket[real-sqrt]的参数大于或等于1，那么一个很基本的正确性检查是，结果小于参数。

@;{In general, a dependent function contract looks just like
the more general @racket[->*] contract, but with names added
that can be used elsewhere in the contract.}
一般来说，一个从属函数合约看起来更像一般的@racket[->*]合约，但添加的名称可以在合约的其它地方使用。

@;{
Yes, there are many other contract combinators such as @racket[<=/c]
and @racket[>=/c], and it pays off to look them up in the contract
section of the reference manual. They simplify contracts tremendously
and make them more accessible to potential clients. 
}

@;{Going back to the bank-account example, suppose that we generalize the
module to support multiple accounts and that we also include a
withdrawal operation. The improved bank-account module includes an
@racket[account] structure type and the following functions:}
回到银行帐户示例，假设我们将模块泛化为支持多个帐户，并且我们还包括一个取款操作。改进后的银行帐户模块包括@racket[account]结构类型和以下函数：

@racketblock[
(provide (contract-out
          [balance (-> account? amount/c)]
          [withdraw (-> account? amount/c account?)]
          [deposit (-> account? amount/c account?)]))
]

@;{Besides requiring that a client provide a valid amount for a
withdrawal, however, the amount should be less than or equal to the specified
account's balance, and the resulting account will have less money than
it started with. Similarly, the module might promise that a deposit
produces an account with money added to the account. The following
implementation enforces those constraints and guarantees through
contracts:}
但是，除了要求客户为取款提供有效金额外，金额应小于或等于指定账户的余额，由此产生的账户将比开始时的钱少。同样，该模块可能承诺存款产生一个帐户，加上账户中的钱。以下实现通过契约强制执行这些约束和保证：

@racketmod[
racket

(code:comment @;{"section 1: the contract definitions"}"第1节：合约定义")
(struct account (balance))
(define amount/c natural-number/c)

(code:comment @;{"section 2: the exports"}"第2节：导出")
(provide
 (contract-out
  [create   (amount/c . -> . account?)]
  [balance  (account? . -> . amount/c)]
  [withdraw (->i ([acc account?]
                  [amt (acc) (and/c amount/c (<=/c (balance acc)))])
                 [result (acc amt)
                         (and/c account? 
                                (lambda (res)
                                  (>= (balance res) 
                                      (- (balance acc) amt))))])]
  [deposit  (->i ([acc account?]
                  [amt amount/c])
                 [result (acc amt)
                         (and/c account? 
                                (lambda (res)
                                  (>= (balance res) 
                                      (+ (balance acc) amt))))])]))

(code:comment @;{"section 3: the function definitions"}"第3节：函数定义")
(define balance account-balance)

(define (create amt) (account amt))

(define (withdraw a amt)
  (account (- (account-balance a) amt)))

(define (deposit a amt)
  (account (+ (account-balance a) amt)))
]

@;{The contracts in section 2 provide typical type-like guarantees for
@racket[create] and @racket[balance]. For @racket[withdraw] and
@racket[deposit], however, the contracts check and guarantee the more
complicated constraints on @racket[balance] and @racket[deposit].  The
contract on the second argument to @racket[withdraw] uses
@racket[(balance acc)] to check whether the supplied withdrawal amount
is small enough, where @racket[acc] is the name given within
@racket[->i] to the function's first argument. The contract on the
result of @racket[withdraw] uses both @racket[acc] and @racket[amt] to
guarantee that no more than that requested amount was withdrawn. The
contract on @racket[deposit] similarly uses @racket[acc] and
@racket[amount] in the result contract to guarantee that at least as
much money as provided was deposited into the account.}
第2节中的合约提供了典型的@racket[create]和@racket[balance]的类型保证。然而，对于@racket[withdraw]和@racket[deposit]，合约检查并保证对@racket[balance]和@racket[deposit]的更为复杂的约束。第二个参数@racket[withdraw]的合约使用@racket[(balance acc)]检查所提供的取款金额是否足够小，其中@racket[acc]是在@racket[->i]之中给定的函数第一个参数的名称。@racket[withdraw]结果的合约使用@racket[acc]和@racket[amt]保证不超过所要求的金额被取回。@racket[deposit]的合约同样使用结果合约中的@racket[acc]和@racket[amount]来保证提供的至少钱存入账户。

@;{As written above, when a contract check fails, the error message is
not great. The following revision uses @racket[flat-named-contract]
within a helper function @racket[mk-account-contract] to provide
better error messages.}
正如上面所描述的，当合约检查失败时，错误消息不是很显著。下面的修订在辅助函数@racket[mk-account-contract]中使用了@racket[flat-named-contract]（扁平命名合约），以提供更好的错误消息。

@racketmod[
racket

(code:comment @;{"section 1: the contract definitions"}"第1节：合约定义")
(struct account (balance))
(define amount/c natural-number/c)

(define msg> "account a with balance larger than ~a expected")
(define msg< "account a with balance less than ~a expected")

(define (mk-account-contract acc amt op msg)
  (define balance0 (balance acc))
  (define (ctr a)
    (and (account? a) (op balance0 (balance a))))
  (flat-named-contract (format msg balance0) ctr))

(code:comment @;{"section 2: the exports"}"第2节：导出")
(provide
 (contract-out
  [create   (amount/c . -> . account?)]
  [balance  (account? . -> . amount/c)]
  [withdraw (->i ([acc account?]
                  [amt (acc) (and/c amount/c (<=/c (balance acc)))])
                 [result (acc amt) (mk-account-contract acc amt >= msg>)])]
  [deposit  (->i ([acc account?]
                  [amt amount/c])
                 [result (acc amt) 
                         (mk-account-contract acc amt <= msg<)])]))

(code:comment @;{"section 3: the function definitions"}"第3节：函数定义")
(define balance account-balance)

(define (create amt) (account amt))

(define (withdraw a amt)
  (account (- (account-balance a) amt)))

(define (deposit a amt)
  (account (+ (account-balance a) amt)))
]

@;{@ctc-section[#:tag "arrow-d-eval-order"]{Checking State Changes}}
@ctc-section[#:tag "arrow-d-eval-order"]{检查状态变化}

@;{The @racket[->i] contract combinator can also ensure that a
function only modifies state according to certain
constraints. For example, consider this contract
(it is a slightly simplified version from the function
@racket[preferences:add-panel] in the framework):}
@racket[->i]合约的组合也可以确保函数按照一定的约束只修改状态。例如，考虑这个合约（它是从框架中函数@racket[preferences:add-panel]首选项中添加的略微简化的版本）：

@racketblock[
(->i ([parent (is-a?/c area-container-window<%>)])
      [_ (parent)
       (let ([old-children (send parent get-children)])
         (λ (child)
           (andmap eq?
                   (append old-children (list child))
                   (send parent get-children))))])
]

@;{It says that the function accepts a single argument, named
@racket[parent], and that @racket[parent] must be
an object matching the interface @racket[area-container-window<%>].}
它表示函数接受单个参数，命名为@racket[parent]并且@racket[parent]必须是匹配接口@racket[area-container-window<%>]。

@;{The range contract ensures that the function only modifies
the children of @racket[parent] by adding a new child to the
front of the list. It accomplishes this by using the
@racket[_] instead of a normal identifier, which tells the
contract library that the range contract does not depend on
the values of any of the results, and thus the contract
library evaluates the expression following the @racket[_]
when the function is called, instead of when it
returns. Therefore the call to the @racket[get-children] method
happens before the function under the contract is called.
When the function under contract returns, its result is
passed in as @racket[child], and the contract ensures that
the children after the function return are the same as the
children before the function called, but with one more
child, at the front of the list.}
范围合约确保函数只通过在列表前面添加一个新的子代来修改@racket[parent]的子类。这是通过使用@racket[_]代替正常的标识符，它告诉合约库的范围合约并不依赖于任何结果的值，因此合约计算表达式后，@racket[_]库在函数被调用时，而不是返回时。因此，调用@racket[get-children]方法之前发生在合约下的函数调用。当合约下的函数返回时，它的结果作为@racket[child]传递，并且合约确保函数返回后的child与函数调用之前的child相同，但是在列表前面还有一个child。

@;{To see the difference in a toy example that focuses
on this point, consider this program}
要看一个集中在这一点上的无实用价值的示例的差异，请考虑这个程序：

@racketmod[
racket
(define x '())
(define (get-x) x)
(define (f) (set! x (cons 'f x)))
(provide
 (contract-out
  [f (->i () [_ (begin (set! x (cons 'ctc x)) any/c)])]
  [get-x (-> (listof symbol?))]))
]

@;{If you were to require this module, call @racket[f], then
the result of @racket[get-x] would be @racket['(f ctc)]. In
contrast, if the contract for @racket[f] were}
如果你需要这个模块，调用@racket[f]，然后@racket[get-x]结果会@racket['(f ctc)]。相反，如果 @racket[f]的合约是

@racketblock[(->i () [res (begin (set! x (cons 'ctc x)) any/c)])]

@;{(only changing the underscore to @racket[res]), then
the result of @racket[get-x] would be @racket['(ctc f)].}
（只改变下划线@racket[res]），然后 @racket[get-x]结果会是@racket['(ctc f)]。

@;{@ctc-section[#:tag "multiple"]{Multiple Result Values}}
@ctc-section[#:tag "multiple"]{多个结果值}

@;{The function @racket[split] consumes a list of @racket[char]s
  and delivers the string that occurs before the first occurrence of
  @racket[#\newline] (if any) and the rest of the list: }
函数 @racket[split]接受@racket[char]列表和传递所发生的 @racket[#\newline] 的第一次出现在字符串（如果有）和其余的列表：

@racketblock[
(define (split l)
  (define (split l w)
    (cond
      [(null? l) (values (list->string (reverse w)) '())]
      [(char=? #\newline (car l))
       (values (list->string (reverse w)) (cdr l))]
      [else (split (cdr l) (cons (car l) w))]))
  (split l '()))
]

  @;{It is a typical multiple-value function, returning two values by
  traversing a single list.}
它是一个典型的多值函数，通过遍历单个列表返回两个值。

@;{The contract for such a function can use the ordinary
function arrow @racket[->], since @racket[->]
treats @racket[values] specially when it appears as the
last result:}
这种函数的合约可以使用普通函数箭头@racket[->]，那么当它作为最后结果出现时，@racket[->]特别地处理@racket[values]：

@racketblock[
(provide (contract-out
          [split (-> (listof char?)
                     (values string? (listof char?)))]))
]

@;{The contract for such a function can also be written
using @racket[->*]:}
这种函数的合约也可以使用@racket[->*]：

@racketblock[
(provide (contract-out
          [split (->* ((listof char?))
                      ()
                      (values string? (listof char?)))]))
]

 @;{As before, the contract for the argument with @racket[->*] is wrapped in an
 extra pair of parentheses (and must always be wrapped like
 that) and the empty pair of parentheses indicates that
 there are no optional arguments. The contracts for the
 results are inside @racket[values]: a string and a list of
 characters.}
和以前一样，与@racket[->*]参数的合约被封装在一对额外的圆括号中（并且必须总是这样包装），而空的括号表示没有可选参数。结果的合约是内部的@racket[values]：字符串和字符列表。

@;{Now, suppose that we also want to ensure that the first result of
 @racket[split] is a prefix of the given word in list format. In that
 case, we need to use the @racket[->i] contract combinator:}
现在，假设我们还希望确保第一个结果@racket[split]是给定列表格式中给定单词的前缀。在这种情况下，我们需要使用@racket[->i]合约的组合： 

@racketblock[
(define (substring-of? s)
  (flat-named-contract
    (format "substring of ~s" s)
    (lambda (s2)
      (and (string? s2)
           (<= (string-length s2) (string-length s))
           (equal? (substring s 0 (string-length s2)) s2)))))

(provide
 (contract-out
  [split (->i ([fl (listof char?)])
              (values [s (fl) (substring-of? (list->string fl))]
                      [c (listof char?)]))]))
]

 @;{Like @racket[->*], the @racket[->i] combinator uses a function over the
 argument to create the range contracts. Yes, it doesn't just return one
 contract but as many as the function produces values: one contract per
 value.  In this case, the second contract is the same as before, ensuring
 that the second result is a list of @racket[char]s. In contrast, the
 first contract strengthens the old one so that the result is a prefix of
 the given word. }
像@racket[->*]、@racket[->i]组合使用函数中的参数来创建范围的合约。是的，它不只是返回一个合约，而是函数产生值的数量：每个值的一个合约。在这种情况下，第二个合约和以前一样，确保第二个结果是@racket[char]列表。与此相反，第一个合约增强旧的，因此结果是给定单词的前缀。

@;{This contract is expensive to check, of course. Here is a slightly
  cheaper version: }
当然，这个合约检查是很值得的。这里有一个稍微廉价一点的版本：

@racketblock[
(provide
 (contract-out
  [split (->i ([fl (listof char?)])
              (values [s (fl) (string-len/c (length fl))]
                      [c (listof char?)]))]))
]


@;{@ctc-section[#:tag "no-domain"]{Fixed but Statically Unknown Arities}}
@ctc-section[#:tag "no-domain"]{固定但静态未知数量的参数}

@;{Imagine yourself writing a contract for a function that accepts some other
function and a list of numbers that eventually applies the former to the
latter. Unless the arity of the given function matches the length of the
given list, your procedure is in trouble. }
想象一下你自己为一个函数写了一个合约，这个函数接受了另一个函数和一个字列表，最终数值前者应用于后者。如果给定的函数的数量匹配给定列表的长度，你的程序就有困难。

@;{Consider this @racket[n-step] function:}
考虑这个@racket[n-step]函数：

@racketblock[
(code:comment "(number ... -> (union #f number?)) (listof number) -> void")
(define (n-step proc inits)
  (let ([inc (apply proc inits)])
    (when inc
      (n-step proc (map (λ (x) (+ x inc)) inits)))))
]

@;{The argument of @racket[n-step] is @racket[proc], a function
@racket[proc] whose results are either numbers or false, and a list. It
then applies @racket[proc] to the list @racket[inits]. As long as
@racket[proc] returns a number, @racket[n-step] treats that number
as an increment for each of the numbers in @racket[inits] and
recurs. When @racket[proc] returns @racket[false], the loop stops.}
@racket[n-step]参数是@racket[proc]，一个@racket[proc]函数的结果要么是数字要么是假，或者一个列表。然后应用@racket[proc]到@racket[inits]列表中。只要@racket[proc]返回一个数值，@racket[n-step]对待数值为每个在其数字@racket[inits]和递归的增量值。当@racket[proc]返回@racket[false]时，循环停止。
  
@;{Here are two uses:}
这里有两个应用：

@racketblock[
(code:comment "nat -> nat") 
(define (f x)
  (printf "~s\n" x)
  (if (= x 0) #f -1))
(n-step f '(2))

(code:comment "nat nat -> nat") 
(define (g x y)
  (define z (+ x y))
  (printf "~s\n" (list x y z))
  (if (= z 0) #f -1))
  
(n-step g '(1 1))
]

@;{A contract for @racket[n-step] must specify two aspects of
@racket[proc]'s behavior: its arity must include the number of elements
in @racket[inits], and it must return either a number or
@racket[#f]. The latter is easy, the former is difficult. At first
glance, this appears to suggest a contract that assigns a
@italic{variable-arity} to @racket[proc]: }
一个@racket[n-step]的合约必须指定@racket[proc]的两方面行为：其数量必须在@racket[inits]里包括元素的数量，它必须返回一个数值或@racket[#f]。后者是容易的，前者是困难的。乍一看，这似乎表明合约分配@italic{variable-arity}给了@racket[proc]：

@racketblock[
(->* () 
     #:rest (listof any/c)
     (or/c number? false/c))
]

@;{This contract, however, says that the function must accept @emph{any}
number of arguments, not a @emph{specific} but
@emph{undetermined} number. Thus, applying @racket[n-step] to
@racket[(lambda (x) x)] and @racket[(list 1)] breaks the contract
because the given function accepts only one argument. }
然而，这个合约说函数必须接受@emph{任意（any）}数量的参数，而不是@emph{特定（specific）}的但@emph{不确定（undetermined）}的数值。因此，应用@racket[n-step]到@racket[(lambda (x) x)] and @racket[(list 1)]违约，因为给定的函数只接受一个参数。

 @;{The correct contract uses the @racket[unconstrained-domain->]
 combinator, which specifies only the range of a function, not its
 domain. It is then possible to combine this contract with an arity test to
 specify the correct contract for @racket[n-step]:}
正确的合约采用@racket[unconstrained-domain->]组合，其仅指定函数的范围，而不是它的域。可以将本合约的数量测试指定正确的合约结合@racket[n-step]：

@racketblock[
(provide
 (contract-out
  [n-step
   (->i ([proc (inits)
          (and/c (unconstrained-domain-> 
                  (or/c false/c number?))
                 (λ (f) (procedure-arity-includes? 
                         f 
                         (length inits))))]
         [inits (listof number?)])
        ()
        any)]))
]

