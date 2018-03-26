#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/unit racket/class))

@(define toy-eval (make-base-eval))

@(interaction-eval #:eval toy-eval (require racket/unit))

@(define-syntax-rule (racketmod/eval [pre ...] form more ...)
   (begin
     (racketmod pre ... form more ...)
     (interaction-eval #:eval toy-eval form)))

@;{@title[#:tag "units" #:style 'toc]{Units@aux-elem{ (Components)}}}
@title[#:tag "units" #:style 'toc]{单元@aux-elem{（组件）}}

@;{@deftech{Units} organize a program into separately compilable and
reusable @deftech{components}. A unit resembles a procedure in that
both are first-class values that are used for abstraction. While
procedures abstract over values in expressions, units abstract over
names in collections of definitions. Just as a procedure is called to
evaluate its expressions given actual arguments for its formal
parameters, a unit is @deftech{invoked} to evaluate its definitions
given actual references for its imported variables. Unlike a
procedure, however, a unit's imported variables can be partially
linked with the exported variables of another unit @italic{prior to
invocation}. Linking merges multiple units together into a single
compound unit. The compound unit itself imports variables that will be
propagated to unresolved imported variables in the linked units, and
re-exports some variables from the linked units for further linking.}
@deftech{单元（unit）}组织程序分成独立的编译和可重用的@deftech{组件（component）}。一个单元类似于过程，因为这两个都是用于抽象的一级值。虽然程序对表达式中的值进行抽象，但在集合定义中对名称进行抽象。正如一个过程被调用来对它的表达式求值，表达式把实际的参数作为给它的正式参数，一个单元被@deftech{调用（invoked）}来对它的定义求值，这个定义给出其导入变量的实际引用。但是，与过程不同的是，在调用之前，一个单元的导入变量可以部分地与另一个@italic{之前调用（prior to
invocation）}单元的导出变量链接。链接将多个单元合并成单个复合单元。复合单元本身导入将传播到链接单元中未解决的导入变量的变量，并从链接单元中重新导出一些变量以进一步链接。

@local-table-of-contents[]

@; ----------------------------------------

@;{@section{Signatures and Units}}
@section[#:tag "Signatures-and-Units"]{签名和单元}

@;{The interface of a unit is described in terms of
@deftech{signatures}. Each signature is defined (normally within a
@racket[module]) using @racket[define-signature].  For example, the
following signature, placed in a @filepath{toy-factory-sig.rkt} file,
describes the exports of a component that implements a toy factory:}
单元的接口用@deftech{签名（signature）}来描述。每个签名都使用@racket[define-signature]来定义（通常在@racket[module]（模块）中）。例如，下面的签名，放在一个@filepath{toy-factory-sig.rkt}的文件中，描述了一个组件的导出（export），它实现了一个玩具厂（toy factory）：

@;{@margin-note{By convention, signature names end with @litchar{^}.}}
@margin-note{按照惯例，签名名称用@litchar{^}结束。}

@racketmod/eval[[#:file
"toy-factory-sig.rkt"
racket]

(define-signature toy-factory^
  (build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
   repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
   toy?        (code:comment #, @tt{(any/c -> boolean?)})
   toy-color)) (code:comment #, @tt{(toy? -> symbol?)})

(provide toy-factory^)
]

@;{An implementation of the @racket[toy-factory^] signature is written
using @racket[define-unit] with an @racket[export] clause that names
@racket[toy-factory^]:}
一个@racket[toy-factory^]签名的实现是用@racket[define-unit]来写的，它定义了一个名为@racket[toy-factory^]的@racket[export]（导出）从句：

@;{@margin-note{By convention, unit names end with @litchar["@"].}}
@margin-note{按照惯例，单位名称用@litchar["@"]结束。}

@racketmod/eval[[#:file
"simple-factory-unit.rkt"
racket

(require "toy-factory-sig.rkt")]

(define-unit simple-factory@
  (import)
  (export toy-factory^)

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide simple-factory@)
]

@;{The @racket[toy-factory^] signature also could be referenced by a unit
that needs a toy factory to implement something else. In that case,
@racket[toy-factory^] would be named in an @racket[import] clause.
For example, a toy store would get toys from a toy factory. (Suppose,
for the sake of an example with interesting features, that the store
is willing to sell only toys in a particular color.)}
@racket[toy-factory^]签名也可以被一个单元引用，它需要一个玩具工厂来实施其它某些东西。在这种情况下，@racket[toy-factory^]将以一个@racket[import]（导入）从句命名。例如，玩具店可以从玩具厂买到玩具。（假设为了一个有趣的例子，商店只愿意出售特定颜色的玩具）。

@racketmod/eval[[#:file
"toy-store-sig.rkt"
racket]

(define-signature toy-store^
  (store-color     (code:comment #, @tt{(-> symbol?)})
   stock!          (code:comment #, @tt{(integer? -> void?)})
   get-inventory)) (code:comment #, @tt{(-> (listof toy?))})

(provide toy-store^)
]

@racketmod/eval[[#:file
"toy-store-unit.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define-unit toy-store@
  (import toy-factory^)
  (export toy-store^)

  (define inventory null)

  (define (store-color) 'green)

  (define (maybe-repaint t)
    (if (eq? (toy-color t) (store-color))
        t
        (repaint t (store-color))))

  (define (stock! n)
    (set! inventory 
          (append inventory
                  (map maybe-repaint
                       (build-toys n)))))

  (define (get-inventory) inventory))

(provide toy-store@)
]

@;{Note that @filepath{toy-store-unit.rkt} imports
@filepath{toy-factory-sig.rkt}, but not
@filepath{simple-factory-unit.rkt}.  Consequently, the
@racket[toy-store@] unit relies only on the specification of a toy
factory, not on a specific implementation.}
请注意，@filepath{toy-store-unit.rkt}导入@filepath{toy-factory-sig.rkt}，而不是@filepath{simple-factory-unit.rkt}。因此，@racket[toy-store@]单元只依赖于玩具工厂的规格，而不是具体的实施。


@; ----------------------------------------

@;{@section{Invoking Units}}
@section[#:tag "Invoking-Units"]{调用单元}

@;{The @racket[simple-factory@] unit has no imports, so it can be
@tech{invoked} directly using @racket[invoke-unit]:}
@racket[simple-factory@]单元没有导入，因此可以使用@racket[invoke-unit]直接调用它：

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.rkt") (void))
(invoke-unit simple-factory@)
]

@;{The @racket[invoke-unit] form does not make the body definitions
available, however, so we cannot build any toys with this factory. The
@racket[define-values/invoke-unit] form binds the identifiers of a
signature to the values supplied by a unit (to be @tech{invoked}) that
implements the signature:}
但是，@racket[invoke-unit]表并不能使主体定义可用，因此我们不能在这家工厂制造任何玩具。@racket[define-values/invoke-unit]表将签名的标识符绑定到实现签名的一个单元（要@tech{调用}的）提供的值：

@interaction[
#:eval toy-eval
(define-values/invoke-unit/infer simple-factory@)
(build-toys 3)
]

@;{Since @racket[simple-factory@] exports the @racket[toy-factory^]
signature, each identifier in @racket[toy-factory^] is defined by the
@racket[define-values/invoke-unit/infer] form. The
@racketidfont{/infer} part of the form name indicates that the
identifiers bound by the declaration are inferred from
@racket[simple-factory@].}
由于@racket[simple-factory@]导出@racket[toy-factory^]签名，@racket[toy-factory^]的每个标识符都是由@racket[define-values/invoke-unit/infer]表定义的。表名称的@racketidfont{/infer}部分表明，由声明约束的标识符是从@racket[simple-factory@]推断出来的。

@;{Now that the identifiers in @racket[toy-factory^] are defined, we can
also invoke @racket[toy-store@], which imports @racket[toy-factory^]
to produce @racket[toy-store^]:}
在定义@racket[toy-factory^]的标识后，我们还可以调用@racket[toy-store@]，它导入@racket[toy-factory^]以产生@racket[toy-store^]：

@interaction[
#:eval toy-eval
(eval:alts (require "toy-store-unit.rkt") (void))
(define-values/invoke-unit/infer toy-store@)
(get-inventory)
(stock! 2)
(get-inventory)
]

@;{Again, the @racketidfont{/infer} part
@racket[define-values/invoke-unit/infer] determines that
@racket[toy-store@] imports @racket[toy-factory^], and so it supplies
the top-level bindings that match the names in @racket[toy-factory^]
as imports to @racket[toy-store@].}
同样，@racketidfont{/infer}部分@racket[define-values/invoke-unit/infer]确定@racket[toy-store@]导入@racket[toy-factory^]，因此它提供与@racket[toy-factory^]中的名称匹配的顶级绑定，如导入@racket[toy-store@]。

@; ----------------------------------------

@;{@section{Linking Units}}
@section[#:tag "Linking-Units"]{链接单元}

@;{We can make our toy economy more efficient by having toy factories
that cooperate with stores, creating toys that do not have to be
repainted. Instead, the toys are always created using the store's
color, which the factory gets by importing @racket[toy-store^]:}
我们可以借助玩具工厂的合作使我们的玩具店玩具经济性更有效，不需要重新创建。相反，玩具总是使用商店的颜色来制造，而工厂的颜色是通过导入@racket[toy-store^]来获得的：

@racketmod/eval[[#:file
"store-specific-factory-unit.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define-unit store-specific-factory@
  (import toy-store^)
  (export toy-factory^)

  (define-struct toy () #:transparent)

  (define (toy-color t) (store-color))

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy)))

  (define (repaint t col)
    (error "cannot repaint")))

(provide store-specific-factory@)
]

@;{To invoke @racket[store-specific-factory@], we need
@racket[toy-store^] bindings to supply to the unit. But to get
@racket[toy-store^] bindings by invoking @racket[toy-store@], we will
need a toy factory! The unit implementations are mutually dependent,
and we cannot invoke either before the other.}
要调用@racket[store-specific-factory@]，我们需要@racket[toy-store^]绑定供及给单元。但是为了通过调用@racket[toy-store^]来获得@racket[toy-store^]的绑定，我们需要一个玩具工厂！单元实现是相互依赖的，我们不能在另一个之前调用那一个。

@;{The solution is to @deftech{link} the units together, and then we can
invoke the combined units. The @racket[define-compound-unit/infer] form
links any number of units to form a combined unit. It can propagate
imports and exports from the linked units, and it can satisfy each
unit's imports using the exports of other linked units.}
解决方案是将这些单元@deftech{链接（link）}在一起，然后调用组合单元。@racket[define-compound-unit/infer]表将任意数量的单元链接成一个组合单元。它可以从相连的单元中进行导入和导出，并利用其它链接单元的导出来满足各单元的导入。

@interaction[
#:eval toy-eval
(eval:alts (require "toy-factory-sig.rkt") (void))
(eval:alts (require "toy-store-sig.rkt") (void))
(eval:alts (require "store-specific-factory-unit.rkt") (void))
(define-compound-unit/infer toy-store+factory@
  (import)
  (export toy-factory^ toy-store^)
  (link store-specific-factory@
        toy-store@))
]

@;{The overall result above is a unit @racket[toy-store+factory@] that
exports both @racket[toy-factory^] and @racket[toy-store^]. The
connection between @racket[store-specific-factory@] and
@racket[toy-store@] is inferred from the signatures that each imports
and exports.}
上边总的结果是一个单元@racket[toy-store+factory@]，其导出既是@racket[toy-factory^]也是@racket[toy-store^]。从每个导入和导出的签名中推断出@racket[store-specific-factory@]和@racket[toy-store@]之间的联系。

@;{This unit has no imports, so we can always invoke it:}
这个单元没有导入，所以我们可以随时调用它：

@interaction[
#:eval toy-eval
(define-values/invoke-unit/infer toy-store+factory@)
(stock! 2)
(get-inventory)
(map toy-color (get-inventory))
]

@; ----------------------------------------

@;{@section[#:tag "firstclassunits"]{First-Class Units}}
@section[#:tag "firstclassunits"]{一级单元}

@;{The @racket[define-unit] form combines @racket[define] with a
@racket[unit] form, similar to the way that @racket[(define (f x)
....)]  combines @racket[define] followed by an identifier with an
implicit @racket[lambda].}
@racket[define-unit]表将@racket[define]与@racket[unit]表相结合，类似于@racket[(define (f x)
....)]结合@racket[define]，后跟带一个隐式的@racket[lambda]的标识符。

@;{Expanding the shorthand, the definition of @racket[toy-store@] could
almost be written as}
扩大简写，@racket[toy-store@]的定义几乎可以写成

@racketblock[
(define toy-store@
  (unit
   (import toy-factory^)
   (export toy-store^)

   (define inventory null)

   (define (store-color) 'green)
   ....))
]

@;{A difference between this expansion and @racket[define-unit] is that
the imports and exports of @racket[toy-store@] cannot be
inferred. That is, besides combining @racket[define] and
@racket[unit], @racket[define-unit] attaches static information to the
defined identifier so that its signature information is available
statically to @racket[define-values/invoke-unit/infer] and other
forms.}
这个扩展和@racket[define-unit]的区别在于，@racket[toy-store@]的导入和导出不能被推断出来。也就是说，除了将@racket[define]和@racket[unit]结合在一起，@racket[define-unit]还将静态信息附加到定义的标识符，以便静态地提供它的签名信息来@racket[define-values/invoke-unit/infer]和其它表。

@;{Despite the drawback of losing static signature information,
@racket[unit] can be useful in combination with other forms that work
with first-class values. For example, we could wrap a @racket[unit]
that creates a toy store in a @racket[lambda] to supply the store's
color:}
虽有丢失静态签名信息的缺点，@racket[unit]可以与使用第一类值的其它表结合使用。例如，我们可以封装一个@racket[unit]，它在一个 @racket[lambda]中创建一个玩具商店来提供商店的颜色：

@racketmod/eval[[#:file
"toy-store-maker.rkt"
racket

(require "toy-store-sig.rkt"
         "toy-factory-sig.rkt")]

(define toy-store@-maker
  (lambda (the-color)
    (unit
     (import toy-factory^)
     (export toy-store^)

     (define inventory null)

     (define (store-color) the-color)

     (code:comment @#,t{@;{the rest is the same as before}其余的和前面一样})

     (define (maybe-repaint t)
       (if (eq? (toy-color t) (store-color))
           t
           (repaint t (store-color))))

     (define (stock! n)
       (set! inventory
             (append inventory
                     (map maybe-repaint
                          (build-toys n)))))

     (define (get-inventory) inventory))))

(provide toy-store@-maker)
]

@;{To invoke a unit created by @racket[toy-store@-maker], we must use
@racket[define-values/invoke-unit], instead of the
@racketidfont{/infer} variant:}
要调用由@racket[toy-store@-maker]创建的单元，我们必须使用@racket[define-values/invoke-unit]，而不是@racketidfont{/infer}变量：

@interaction[
#:eval toy-eval
(eval:alts (require "simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer simple-factory@)
(eval:alts (require "toy-store-maker.rkt") (void))
(define-values/invoke-unit (toy-store@-maker 'purple)
  (import toy-factory^)
  (export toy-store^))
(stock! 2)
(get-inventory)
]

@;{In the @racket[define-values/invoke-unit] form, the @racket[(import
toy-factory^)] line takes bindings from the current context that match
the names in @racket[toy-factory^] (the ones that we created by
invoking @racket[simple-factory@]), and it supplies them as imports to
@racket[toy-store@]. The @racket[(export toy-store^)] clause indicates
that the unit produced by @racket[toy-store@-maker] will export
@racket[toy-store^], and the names from that signature are defined
after invoking the unit.}
在@racket[define-values/invoke-unit]表中，@racket[(import
toy-factory^)]行从当前的上下文中获取与@racket[toy-factory^]中的名称匹配的绑定（我们通过调用@racket[simple-factory@])创建的名称），并将它们提供于导入@racket[toy-store@]。@racket[(export toy-store^)]从句表明@racket[toy-store@-maker]产生的单元将导出@racket[toy-store^]，并在调用该单元后定义该签名的名称。

@;{To link a unit from @racket[toy-store@-maker], we can use the
@racket[compound-unit] form:}
为了把一个单元与@racket[toy-store@-maker]链接起来，我们可以使用@racket[compound-unit]表：

@interaction[
#:eval toy-eval
(eval:alts (require "store-specific-factory-unit.rkt") (void))
(define toy-store+factory@
  (compound-unit
   (import)
   (export TF TS)
   (link [((TF : toy-factory^)) store-specific-factory@ TS]
         [((TS : toy-store^)) toy-store@ TF])))
]

@;{This @racket[compound-unit] form packs a lot of information into one
place. The left-hand-side @racket[TF] and @racket[TS] in the
@racket[link] clause are binding identifiers. The identifier
@racket[TF] is essentially bound to the elements of
@racket[toy-factory^] as implemented by
@racket[store-specific-factory@].  The identifier @racket[TS] is
similarly bound to the elements of @racket[toy-store^] as implemented
by @racket[toy-store@]. Meanwhile, the elements bound to @racket[TS]
are supplied as imports for @racket[store-specific-factory@], since
@racket[TS] follows @racket[store-specific-factory@]. The elements
bound to @racket[TF] are similarly supplied to
@racket[toy-store@]. Finally, @racket[(export TF TS)] indicates that
the elements bound to @racket[TF] and @racket[TS] are exported from
the compound unit.}
这个@racket[compound-unit]表将许多信息聚集到一个地方。@racket[link]从句中的左侧@racket[TF]和@racket[TS]是绑定标识符。标识符@racket[TF]基本上绑定到@racket[toy-factory^]的元素作为由@racket[store-specific-factory@]的实现。标识符@racket[TS]类似地绑定到@racket[toy-store^]的元素作为由@racket[toy-store@]的实现。同时，绑定到@racket[TS]的元素作为提供给@racket[store-specific-factory@]的导入，因为@racket[TS]是随着@racket[store-specific-factory@]的。绑定到@racket[TF]的元素也同样提供给@racket[toy-store^]。最后，@racket[(export TF TS)]表明绑定到@racket[TF]和@racket[TS]的元素从复合单元导出。

@;{The above @racket[compound-unit] form uses
@racket[store-specific-factory@] as a first-class unit, even though
its information could be inferred. Every unit can be used as a
first-class unit, in addition to its use in inference contexts. Also,
various forms let a programmer bridge the gap between inferred and
first-class worlds. For example, @racket[define-unit-binding] binds a
new identifier to the unit produced by an arbitrary expression; it
statically associates signature information to the identifier, and it
dynamically checks the signatures against the first-class unit
produced by the expression.}
上面的@racket[compound-unit]表使用@racket[store-specific-factory@]作为一个一级单元，尽管它的信息可以推断。除了在推理上下文中的使用外，每个单元都可以用作一个一级单元。此外，各种表让程序员弥合了推断的和一级的世界之间的间隔。例如，@racket[define-unit-binding]将一个新的标识符绑定到由任意表达式生成的单元；它静态地将签名信息与标识符相关联，并动态地对表达式产生的一级单元进行签名检查。

@; ----------------------------------------

@;{@section{Whole-@racket[module] Signatures and Units}}
@section[#:tag "Whole-module-Signatures-and-Units"]{完整的@racket[module]签名和单元}

@;{In programs that use units, modules like @filepath{toy-factory-sig.rkt}
and @filepath{simple-factory-unit.rkt} are common. The
@racket[racket/signature] and @racket[racket/unit] module names can be
used as languages to avoid much of the boilerplate module, signature,
and unit declaration text.}
在程序中使用的单元，模块如@filepath{toy-factory-sig.rkt}和@filepath{simple-factory-unit.rkt}是常见的。@racket[racket/signature]和@racket[racket/unit]模块的名称可以作为语言来避免大量的样板模块、签名和单元申明文本。

@;{For example, @filepath{toy-factory-sig.rkt} can be written as}
例如，@filepath{toy-factory-sig.rkt}可以写为

@racketmod[
racket/signature

build-toys  (code:comment #, @tt{(integer? -> (listof toy?))})
repaint     (code:comment #, @tt{(toy? symbol? -> toy?)})
toy?        (code:comment #, @tt{(any/c -> boolean?)})
toy-color   (code:comment #, @tt{(toy? -> symbol?)})
]

@;{The signature @racket[toy-factory^] is automatically provided from the
module, inferred from the filename @filepath{toy-factory-sig.rkt} by
replacing the @filepath{-sig.rkt} suffix with @racketidfont{^}.}
签名@racket[toy-factory^]是自动从模块中提供的，它通过用@racketidfont{^}从文件名@filepath{toy-factory-sig.rkt}置换@filepath{-sig.rkt}后缀来推断。

@;{Similarly, @filepath{simple-factory-unit.rkt} module can be written}
同样，@filepath{simple-factory-unit.rkt}模块可以写为

@racketmod[
racket/unit

(require "toy-factory-sig.rkt")

(import)
(export toy-factory^)

(printf "Factory started.\n")

(define-struct toy (color) #:transparent)

(define (build-toys n)
  (for/list ([i (in-range n)])
    (make-toy 'blue)))

(define (repaint t col)
  (make-toy col))
]

@;{The unit @racket[simple-factory@] is automatically provided from the
module, inferred from the filename @filepath{simple-factory-unit.rkt} by
replacing the @filepath{-unit.rkt} suffix with @racketidfont["@"].}
单元@racket[simple-factory@]是自动从模块中提供，它通过用@racketidfont["@"]从文件名@filepath{simple-factory-unit.rkt}置换@filepath{-unit.rkt}后缀来推断。

@; ----------------------------------------

@(interaction-eval #:eval toy-eval (require racket/contract))

@;{@section{Contracts for Units}}
@section[#:tag "Contracts-for-Units"]{单元合约}

@;{There are a couple of ways of protecting units with contracts.  One way
is useful when writing new signatures, and the other handles the case
when a unit must conform to an already existing signature.}
有两种用合约保护单元的方法。一种方法在编写新的签名时是有用的，另一种方法当一个单元必须符合已经存在的签名时就可以处理这种情况。

@;{@subsection{Adding Contracts to Signatures}}
@subsection[#:tag "Adding-Contracts-to-Signatures"]{给签名添加合约}

@;{When contracts are added to a signature, then all units which implement
that signature are protected by those contracts.  The following version
of the @racket[toy-factory^] signature adds the contracts previously
written in comments:}
当合约添加到签名时，实现该签名的所有单元都受到这些合约的保护。@racket[toy-factory^]签名的以下版本添加了前面说明中写过的合约：

@racketmod/eval[[#:file
"contracted-toy-factory-sig.rkt"
racket]

(define-signature contracted-toy-factory^
  ((contracted
    [build-toys (-> integer? (listof toy?))]
    [repaint    (-> toy? symbol? toy?)]
    [toy?       (-> any/c boolean?)]
    [toy-color  (-> toy? symbol?)])))

(provide contracted-toy-factory^)]

Now we take the previous implementation of @racket[simple-factory@] and
implement this version of @racket[toy-factory^] instead:
现在我们采用以前实现的@racket[simple-factory@]，并实现@racket[toy-factory^]的这个版本来代替：

@racketmod/eval[[#:file
"contracted-simple-factory-unit.rkt"
racket

(require "contracted-toy-factory-sig.rkt")]

(define-unit contracted-simple-factory@
  (import)
  (export contracted-toy-factory^)

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide contracted-simple-factory@)
]

@;{As before, we can invoke our new unit and bind the exports so
that we can use them.  This time, however, misusing the exports
causes the appropriate contract errors.}
和以前一样，我们可以调用我们的新单元并绑定导出，这样我们就可以使用它们。然而这次，滥用导出引起相应的合约错误。

@interaction[
#:eval toy-eval
(eval:alts (require "contracted-simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer contracted-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]

@;{@subsection{Adding Contracts to Units}}
@subsection[#:tag "Adding-Contracts-to-Units"]{给单元添加合约}

@;{However, sometimes we may have a unit that must conform to an
already existing signature that is not contracted.  In this case,
we can create a unit contract with @racket[unit/c] or use
the @racket[define-unit/contract] form, which defines a unit which
has been wrapped with a unit contract.}
然而，有时我们可能有一个单元，它必须符合一个已经存在的签名而不是符合合约。在这种情况下，我们可以创建一个带@racket[unit/c]或使用@racket[define-unit/contract]表的单元合约，它定义了一个已被单元合约包装的单元。

@;{For example, here's a version of @racket[toy-factory@] which still
implements the regular @racket[toy-factory^], but whose exports
have been protected with an appropriate unit contract.}
例如，这里有一个@racket[toy-factory@]的版本，它仍然实现了规则@racket[toy-factory^]，但它的输出得到了适当的合约的保护。

@racketmod/eval[[#:file
"wrapped-simple-factory-unit.rkt"
racket

(require "toy-factory-sig.rkt")]

(define-unit/contract wrapped-simple-factory@
  (import)
  (export (toy-factory^
           [build-toys (-> integer? (listof toy?))]
           [repaint    (-> toy? symbol? toy?)]
           [toy?       (-> any/c boolean?)]
           [toy-color  (-> toy? symbol?)]))

  (printf "Factory started.\n")

  (define-struct toy (color) #:transparent)

  (define (build-toys n)
    (for/list ([i (in-range n)])
      (make-toy 'blue)))

  (define (repaint t col)
    (make-toy col)))

(provide wrapped-simple-factory@)
]

@interaction[
#:eval toy-eval
(eval:alts (require "wrapped-simple-factory-unit.rkt") (void))
(define-values/invoke-unit/infer wrapped-simple-factory@)
(build-toys 3)
(build-toys #f)
(repaint 3 'blue)
]


@; ----------------------------------------

@;{@section{@racket[unit] versus @racket[module]}}
@section[#:tag "unit-versus-module"]{@racket[unit]与@racket[module]的比较}

@;{As a form for modularity, @racket[unit] complements @racket[module]:}
作为模块的一个表，@racket[unit]（单元）是对@racket[module]（模块）的补充：

@itemize[

 @item{
  @;{The @racket[module] form is primarily for managing a universal
       namespace. For example, it allows a code fragment to refer
       specifically to the @racket[car] operation from
       @racketmodname[racket/base]---the one that extracts the first
       element of an instance of the built-in pair datatype---as
       opposed to any number of other functions with the name
       @racket[car]. In other words, the @racket[module] construct lets
       you refer to @emph{the} binding that you want.}
@racket[module]表主要用于管理通用命名空间。例如，它允许一个代码片段是专指来自@racketmodname[racket/base]的@racket[car]运算——其中一个提取内置配对数据类型的一个实例的第一个元素——而不是任何其它带@racket[car]名字的函数。换句话说，@racket[module]构造允许你引用你想要的@emph{这个}绑定。
    }

 @item{
  @;{The @racket[unit] form is for parameterizing a code fragment
       with respect to most any kind of run-time value. For example,
       it allows a code fragment to work with a @racket[car]
       function that accepts a single argument, where the specific
       function is determined later by linking the fragment to
       another. In other words, the @racket[unit] construct lets you
       refer to @emph{a} binding that meets some specification.}
@racket[unit]表是参数化的带相对于大多数运行时的值的任意种类的代码片段。例如，它允许一个代码片段与一个接受单个参数的@racket[car]函数一起工作，其中特定函数在稍后通过将片段连接到另一个参数被确定。换句话说，@racket[unit]结构允许你引用满足某些规范的一个绑定。
    }

]

@;{The @racket[lambda] and @racket[class] forms, among others, also allow
parameterization of code with respect to values that are chosen
later. In principle, any of those could be implemented in terms of any
of the others. In practice, each form offers certain
conveniences---such as allowing overriding of methods or especially
simple application to values---that make them suitable for different
purposes.}
除其他外，@racket[lambda]和@racket[class]表还允许对稍后选择的值进行代码参数化。原则上，其中任何一项都可以以其他任何方式执行。在实践中，每个表都提供了某些便利——例如允许重写方法或者特别是对值的特别简单的应用——使它们适合不同的目的。

@;{The @racket[module] form is more fundamental than the others, in a
sense. After all, a program fragment cannot reliably refer to a
@racket[lambda], @racket[class], or @racket[unit] form without the
namespace management provided by @racket[module]. At the same time,
because namespace management is closely related to separate expansion
and compilation, @racket[module] boundaries end up as
separate-compilation boundaries in a way that prohibits mutual
dependencies among fragments. For similar reasons, @racket[module]
does not separate interface from implementation.}
从某种意义上说，@racket[module]表比其它表更为基础。毕竟，没有@racket[module]提供的命名空间管理，程序片段不能可靠地引用@racket[lambda]、@racket[class]或@racket[unit]表。同时，由于名称空间管理与单独的扩展和编译密切相关，@racket[module]边界以独立的编译边界结束，在某种程度上阻止了片段之间的相互依赖关系。出于类似的原因，@racket[module]不将接口与实现分开。

@;{Use @racket[unit] when @racket[module] by itself almost works, but
when separately compiled pieces must refer to each other, or when you
want a stronger separation between @defterm{interface} (i.e., the
parts that need to be known at expansion and compilation time) and
@defterm{implementation} (i.e., the run-time parts). More generally,
use @racket[unit] when you need to parameterize code over functions,
datatypes, and classes, and when the parameterized code itself
provides definitions to be linked with other parameterized code.}
使用@racket[unit]的情况为，在@racket[module]本身几乎可以运行时，但当独立编译的部分必须相互引用时，或当你想要在@defterm{接口（interface）}（即，需要在扩展和编译时间被知道的部分）和@defterm{实现（implementation）}（即，运行时部分）之间有一个更强健的隔离时。更普遍使用@racket[unit]的情况是，当你需要在函数、数据类型和类上参数化代码时，以及当参数代码本身提供定义以和其它参数代码链接时。

@; ----------------------------------------------------------------------

@close-eval[toy-eval]
