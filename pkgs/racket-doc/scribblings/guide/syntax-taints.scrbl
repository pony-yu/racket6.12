#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "stx-certs" #:style 'quiet]{Syntax Taints}}
@title[#:tag "stx-certs" #:style 'quiet]{语法污染}

@;{A use of a macro can expand into a use of an identifier that is not
exported from the module that binds the macro. In general, such an
identifier must not be extracted from the expanded expression and used
in a different context, because using the identifier in a different
context may break invariants of the macro's module.}
一个宏的一个使用可以扩展到一个标识符的使用，该标识符不会从绑定宏的模块中导出。一般来说，这样的标识符不必从扩展表达式中提取出来，并在不同的上下文中使用，因为使用不同上下文中的标识符可能会中断宏模块的不变量。

@;{For example, the following module exports a macro @racket[go] that
expands to a use of @racket[unchecked-go]:}
例如，下面的模块导出一个宏@racket[go]，它扩展到@racket[unchecked-go]的使用：

@racketmod[
#:file "m.rkt"
racket
(provide go)

(define (unchecked-go n x) 
  (code:comment @#,t{@;{to avoid disaster, @racket[n] must be a number}为了避免灾难，@racket[n]必须是个数字})
  (+ n 17))

(define-syntax (go stx)
  (syntax-case stx ()
   [(_ x)
    #'(unchecked-go 8 x)]))
]

@;{If the reference to @racket[unchecked-go] is extracted from the
expansion of @racket[(go 'a)], then it might be inserted into a new
expression, @racket[(unchecked-go #f 'a)], leading to disaster. The
@racket[datum->syntax] procedure can be used similarly to construct
references to an unexported identifier, even when no macro expansion
includes a reference to the identifier.}
如果对@racket[unchecked-go]的引用从@racket[(go 'a)]扩展解析，那么它可能会被插入一个新的表达，@racket[(unchecked-go #f 'a)]，导致灾难。@racket[datum->syntax]程序同样可用于构建一个导出标识符引用，即使没有宏扩展包括一个对标识符的引用。

@;{To prevent such abuses of unexported identifiers, the @racket[go]
macro must explicitly protect its expansion by using
@racket[syntax-protect]:}
为了防止这种滥用的导出标识符，这个宏@racket[go]必须用@racket[syntax-protect]明确保护其扩展：

@racketblock[
(define-syntax (go stx)
  (syntax-case stx ()
   [(_ x)
    (syntax-protect #'(unchecked-go 8 x))]))
]

@;{The @racket[syntax-protect] function causes any syntax object that is
extracted from the result of @racket[go] to be @deftech{tainted}.  The
macro expander rejects tainted identifiers, so attempting to extract
@racket[unchecked-go] from the expansion of @racket[(go 'a)] produces
an identifier that cannot be used to construct a new expression (or, at
least, not one that the macro expander will accept). The
@racket[syntax-rules], @racket[syntax-id-rule], and
@racket[define-syntax-rule] forms automatically protect their
expansion results.}
@racket[syntax-protect]函数会导致从@racket[go]被@deftech{污染（tainted）}的结果中提取的任何语法对象。宏扩展程序拒绝受污染的标识符，因此试图从@racket[(go 'a)]的扩展中提取@racket[unchecked-go]产生一个标识符，该标识符不能用于构造一个新表达式（或者，至少，不是宏扩展程序将接受的表达式）。@racket[syntax-rules]、@racket[syntax-id-rule]和@racket[define-syntax-rule]表自动保护其扩展结果。

@;{More precisely, @racket[syntax-protect] @deftech{arms} a syntax object
with a @deftech{dye pack}. When a syntax object is armed, then
@racket[syntax-e] taints any syntax object in its result. Similarly,
@racket[datum->syntax] taints its result when its first argument is
armed. Finally, if any part of a quoted syntax object is armed, then
the corresponding part is tainted in the resulting syntax constant.}
更确切地说，@racket[syntax-protect] @deftech{配备}了一个带一个@deftech{染料包（dye pack）}的语法对象。当一个语法对象被配备时，那么@racket[syntax-e]在它的结果污染任何语法对象。同样，它的当第一个参数被配备时，@racket[datum->syntax]污染其结果。最后，如果引用的语法对象的任何部分被配备，则相应的部分将受到所生成的语法常数的影响。

@;{Of course, the macro expander itself must be able to @deftech{disarm}
a taint on a syntax object, so that it can further expand an
expression or its sub-expressions. When a syntax object is armed with
a dye pack, the dye pack has an associated inspector that can be used
to disarm the dye pack. A @racket[(syntax-protect _stx)] function call
is actually a shorthand for @racket[(syntax-arm _stx #f #t)], which
arms @racket[_stx] using a suitable inspector. The expander uses
@racket[syntax-disarm] and with its inspector on every expression
before trying to expand or compile it.}
当然，宏扩展本身必须能够@deftech{解除（disarm）}语法对象上的污染，以便它可以进一步扩展表达式或其子表达式。当一个语法对象配备有一个染料包时，染料包装有一个相关的检查者，可以用来解除染料包装。一个@racket[(syntax-protect _stx)]函数调用实际上是一个对@racket[(syntax-arm _stx #f #t)]的简写，这配备@racket[_stx]使用合适的检查程序。在试图扩展或编译它之前，扩展程序使用@racket[syntax-disarm]并在每个表达式上使用它的检查程序。

@;{In much the same way that the macro expander copies properties from a
syntax transformer's input to its output (see @refsecref["stxprops"]),
the expander copies dye packs from a transformer's input to its
output. Building on the previous example,}
与宏扩展程序从语法转换器的输入到其输出的属性（参见《@refsecref["stxprops"]》（Syntax Object Properties））相同，扩展程序将从转换器的输入复制染料包到输出。以前面的例子为基础，

@racketmod[
#:file "n.rkt"
racket
(require "m.rkt")

(provide go-more)

(define y 'hello)

(define-syntax (go-more stx)
  (syntax-protect #'(go y)))
]

@;{the expansion of @racket[(go-more)] introduces a reference to the
unexported @racket[y] in @racket[(go y)], and the expansion result is
armed so that @racket[y] cannot be extracted from the expansion.  Even
if @racket[go] did not use @racket[syntax-protect] for its result
(perhaps because it does not need to protect @racket[unchecked-go]
after all), the dye pack on @racket[(go y)] is propagated to the final
expansion @racket[(unchecked-go 8 y)]. The macro expander uses
@racket[syntax-rearm] to propagate dye packs from a transformer's
input to its output.}
@racket[(go-more)]的扩展介绍了一个对在@racket[(go y)]中的非导出@racket[y]的引用，以及扩展结果被装备，这样@racket[y]不能从扩展中提取。即使@racket[go]没有为其结果使用@racket[syntax-protect]（可能归根到底是因为它不需要保护@racket[unchecked-go]），@racket[(go y)]上的染色包被传播给了最终扩展@racket[(unchecked-go 8 y)]。宏扩展器使用@racket[syntax-rearm]从转换程序的输入和输出增殖（propagate）染料包。

@;------------------------------------------------------------------------
@;{@section{Tainting Modes}}
@section[#:tag "Tainting-Modes"]{污染模式}

@;{In some cases, a macro implementor intends to allow limited
destructuring of a macro result without tainting the result.
For example, given the following @racket[define-like-y]
macro,}
在某些情况下，一个宏执行者有意允许有限的解构的宏结果没有污染结果。例如，给出@racket[define-like-y]宏，

@racketmod[
#:file "q.rkt"
racket

(provide define-like-y)

(define y 'hello)

(define-syntax (define-like-y stx)
  (syntax-case stx ()
    [(_ id) (syntax-protect #'(define-values (id) y))]))
]

@;{someone may use the macro in an internal definition:}
也有人可以在内部定义中使用宏：

@racketblock[
(let ()
  (define-like-y x)
  x)
]

@;{The implementor of the @filepath{q.rkt} module most likely intended to allow
such uses of @racket[define-like-y]. To convert an internal definition
into a @racket[letrec] binding, however, the @racket[define] form
produced by @racket[define-like-y] must be deconstructed, which would
normally taint both the binding @racket[x] and the reference to
@racket[y].}
@filepath{q.rkt}模块的执行器最有可能是希望允许@racket[define-like-y]这样的使用。以转换一个内部定义为@racket[letrec]绑定，但是通过@racket[define-like-y]产生的@racket[define]表必须解构，这通常会污染@racket[x]的绑定和对@racket[y]的引用。

@;{Instead, the internal use of @racket[define-like-y] is allowed,
because @racket[syntax-protect] treats specially a syntax list that
begins with @racket[define-values]. In that case, instead of arming
the overall expression, each individual element of the syntax list is
armed, pushing dye packs further into the second element of the list so
that they are attached to the defined identifiers. Thus,
@racket[define-values], @racket[x], and @racket[y] in the expansion
result @racket[(define-values (x) y)] are individually armed, and the
definition can be deconstructed for conversion to @racket[letrec].}
相反，对@racket[define-like-y]的内部使用是允许的，因为@racket[syntax-protect]特别对待一个以@racket[define-values]开始的语法列表。在这种情况下，代替装备整个表达式的是，语法列表中的每个元素都被装备，进一步将染料包推到列表的第二个元素中，以便它们被附加到定义的标识符上。因此，在扩展结果@racket[(define-values (x) y)]中的@racket[define-values]、@racket[x]和@racket[y]分别被装备，同时定义可以被解构以转化为@racket[letrec]。

@;{Just like @racket[syntax-protect], the expander rearms a transformer
result that starts with @racket[define-values], by pushing dye packs
into the list elements. As a result, @racket[define-like-y] could have
been implemented to produce @racket[(define id y)], which uses
@racket[define] instead of @racket[define-values]. In that case, the
entire @racket[define] form is at first armed with a dye pack, but as the
@racket[define] form is expanded to @racket[define-values], the dye
pack is moved to the parts.}
就像@racket[syntax-protect]，通过将染料包推入这个列表元素，这个扩展程序重新装备一个以@racket[define-values]开始的转换程序结果。作为一个结果， @racket[define-like-y]已经实施产生@racket[(define id y)]，它使用@racket[define]代替@racket[define-values]。在这种情况下，整个@racket[define]表首先装备一个染料包，但是一旦@racket[define]表扩展到@racket[define-values]，染料包被移动到各个部分。

@;{The macro expander treats syntax-list results starting with
@racket[define-syntaxes] in the same way that it treats results
starting with @racket[define-values]. Syntax-list results starting
with @racket[begin] are treated similarly, except that the second
element of the syntax list is treated like all the other elements
(i.e., the immediate element is armed, instead of its
content). Furthermore, the macro expander applies this special
handling recursively, in case a macro produces a @racket[begin] form
that contains nested @racket[define-values] forms.}
宏扩展程序以它处理以@racket[define-values]开始的结果相同的方式处理以@racket[define-syntaxes]开始的语法列表结果。从@racket[begin]开始的语法列表结果同样被处理，除了语法列表的第二个元素被当作其它元素一样处理（即，直接元素被装备，而不是它的上下文）。此外，宏扩展程序递归地应用此特殊处理，以防宏生成包含嵌套@racket[define-values]表的一个@racket[begin]表。

@;{The default application of dye packs can be overridden by attaching
a @racket['taint-mode] property (see @refsecref["stxprops"]) to the
resulting syntax object of a macro transformer. If the property value is
@racket['opaque], then the syntax object is armed and not its
parts. If the property value is @racket['transparent], then the
syntax object's parts are armed. If the property value is
@racket['transparent-binding], then the syntax object's parts and
the sub-parts of the second part (as for @racket[define-values] and
@racket[define-syntaxes]) are armed. The @racket['transparent] and
@racket['transparent-binding] modes trigger recursive property
checking at the parts, so that armings can be pushed arbitrarily deeply
into a transformer's result.}
通过将一个@racket['taint-mode]属性（见《@refsecref["stxprops"]》（Syntax Object Properties））附加到宏转换程序的结果语法对象中，可以覆盖染料包的默认应用程序。如果属性值是@racket['opaque]，那么语法对象被装备而且不是它的部件。如果属性值是@racket['transparent]，则语法对象的部件被装备。如果属性值是@racket['transparent-binding]，那么语法对象的部件和第二个部件的子部件（如@racket[define-values]和@racket[define-syntaxes]）被装备。@racket['transparent]和@racket['transparent-binding]模式触发递归属性在部件的检测，这样就可以把装备任意深入地推入到转换程序的结果中。

@;------------------------------------------------------------------------
@;{@section[#:tag "taints+code-inspectors"]{Taints and Code Inspectors}}
@section[#:tag "taints+code-inspectors"]{污染和代码检查}

@;{Tools that are intended to be privileged (such as a debugging
transformer) must disarm dye packs in expanded programs.  Privilege is
granted through @deftech{code inspectors}. Each dye pack records an
inspector, and a syntax object can be disarmed using a sufficiently
powerful inspector.}
想要获得特权的工具（例如调试转换器）必须在扩展程序中解除染料包的作用。权限是通过 @deftech{代码检查器（code inspector）}授予的。每个染料包的记录一个检查器，同时语法对象可以使用使用一个足够强大的检查器解除。

@;{When a module is declared, the declaration captures the current value
of the @racket[current-code-inspector] parameter.  The captured
inspector is used when @racket[syntax-protect] is applied by a macro
transformer that is defined within the module. A tool can disarm the
resulting syntax object by supplying @racket[syntax-disarm] with
an inspector that is the same or a super-inspector of the module's
inspector. Untrusted code is ultimately run after setting
@racket[current-code-inspector] to a less powerful inspector (after
trusted code, such as debugging tools, have been loaded).}
当声明一个模块时，该声明将捕获@racket[current-code-inspector]参数的当前值。当模块中定义的宏转换器应用@racket[syntax-protect]时，将使用捕获的检查器。一个工具可以通过提供与一个相同的检查器或模块检查器的超级检查器提供@racket[syntax-disarm]对结果语法对象予以解除。在将@racket[current-code-inspector]设置为不太强大的检查器（在加载了受信任的代码，如调试工具，之后），最终会运行不信任代码。

@;{With this arrangement, macro-generating macros require some care,
since the generating macro may embed syntax objects in the generated
macro that need to have the generating module's protection level,
rather than the protection level of the module that contains the
generated macro. To avoid this problem, use the module's
declaration-time inspector, which is accessible as
@racket[(variable-reference->module-declaration-inspector
(#%variable-reference))], and use it to define a variant of
@racket[syntax-protect].}
有了这种安排，宏生成宏需要小心些，因为正在生成的宏可以在已经生成的宏中嵌入语法对象，这些已经生成的宏需要正在生成的模块的保护等级，而不是包含已经生成的宏的模块的保护等级。为了避免这个问题，使用模块的声明时间检查器，它是可以作为@racket[(variable-reference->module-declaration-inspector
(#%variable-reference))]访问的，并使用它来定义一个@racket[syntax-protect]的变体。

@;{For example, suppose that the @racket[go] macro is implemented through
a macro:}
例如，假设@racket[go]宏是通过宏实现的：

@racketmod[
racket
(provide def-go)

(define (unchecked-go n x) 
  (+ n 17))

(define-syntax (def-go stx)
  (syntax-case stx ()
    [(_ go)
     (protect-syntax
      #'(define-syntax (go stx)
          (syntax-case stx ()
            [(_ x)
             (protect-syntax #'(unchecked-go 8 x))])))]))
]

@;{When @racket[def-go] is used inside another module to define
@racket[go], and when the @racket[go]-defining module is at a
different protection level than the @racket[def-go]-defining module, the
generated macro's use of @racket[protect-syntax] is not right.  The
use of @racket[unchecked-go] should be protected at the level of the
@racket[def-go]-defining module, not the @racket[go]-defining module.}
当@racket[def-go]被用于另一个模块定义@racket[go]时，并且当@racket[go]定义模块处于与@racket[def-go]定义模块不同的保护等级时，生成的@racket[protect-syntax]的宏使用是不正确的。在@racket[unchecked-go]在@racket[def-go]定义模块等级应该被保护，而不是@racket[go]定义模块。

@;{The solution is to define and use @racket[go-syntax-protect], instead:}
解决方案是定义和使用@racket[go-syntax-protect]，而不是：

@racketmod[
racket
(provide def-go)

(define (unchecked-go n x) 
  (+ n 17))

(define-for-syntax go-syntax-protect
  (let ([insp (variable-reference->module-declaration-inspector
               (#%variable-reference))])
    (lambda (stx) (syntax-arm stx insp))))

(define-syntax (def-go stx)
  (syntax-case stx ()
    [(_ go)
     (protect-syntax
      #'(define-syntax (go stx)
          (syntax-case stx ()
           [(_ x)
            (go-syntax-protect #'(unchecked-go 8 x))])))]))
]

@;------------------------------------------------------------------------
@;{@section[#:tag "code-inspectors+protect"]{Protected Exports}}
@section[#:tag "code-inspectors+protect"]{受保护的导出}

@;{Sometimes, a module needs to export bindings to some modules---other
modules that are at the same trust level as the exporting module---but
prevent access from untrusted modules. Such exports should use the
@racket[protect-out] form in @racket[provide]. For example,
@racket[ffi/unsafe] exports all of its unsafe bindings as
@deftech{protected} in this sense.}
有时，一个模块需要将绑定导出到一些模块——其它与导出模块在同一信任级别上的模块——但阻止不受信任模块的访问。此类导出应使用@racket[provide]中的@racket[protect-out]表。例如,@racket[ffi/unsafe]导出所有的非安全绑定作为从这个意义上讲的@deftech{受保护的（protected）}。

@;{Code inspectors, again, provide the mechanism for determining which
modules are trusted and which are untrusted. When a module is
declared, the value of @racket[current-code-inspector] is associated
to the module declaration. When a module is instantiated (i.e., when the
body of the declaration is actually executed), a sub-inspector is
created to guard the module's exports. Access to the module's
@tech{protected} exports requires a code inspector higher in the
inspector hierarchy than the module's instantiation inspector; note
that a module's declaration inspector is always higher than its
instantiation inspector, so modules are declared with the same code
inspector can access each other's exports.}
代码检查器再次提供了这个机制，它确定哪一个模块是可信的以及哪一个模块是不可信的。当一个模块被声明时，@racket[current-code-inspector]的值被关联到模块声明。当一个模块被实例化时（即当声明的主体实际上被执行了），一个子检查器被创建来保护模块的导出。对模块的@tech{受保护的（protected）}导出的访问需要一个在检查器层次结构上比这个模块的实例化检查器更高级别的代码检查器；注意一个模块的声明检查器总是高于它的实例化检查器，因此模块以相同的代码声明检查器可以访问其它每一个的导出。

@;{Syntax-object constants within a module, such as literal identifiers
in a template, retain the inspector of their source module. In this
way, a macro from a trusted module can be used within an untrusted
module, and @tech{protected} identifiers in the macro expansion still
work, even through they ultimately appear in an untrusted
module. Naturally, such identifiers should be @tech{arm}ed, so that
they cannot be extracted from the macro expansion and abused by
untrusted code.}
在一个模块中的语法对象常量，如在一个模板中的文字标识符，保留它们的源模块的检查器。以这方式，来自于一个受信任的模块的一个宏可以在不可信的模块内使用，同时宏扩展中的@tech{受保护的（protected）}标识符一直在工作，即使通过它们最终出现在不可信的模块中。当然，这样的标识符应该被@tech{装备（arm）}，所以它们不能从宏扩展中提取并被非信任代码滥用。

@;{Compiled code from a @filepath{.zo} file is inherently untrustworthy,
unfortunately, since it can be synthesized by means other than
@racket[compile]. When compiled code is written to a @filepath{.zo}
file, syntax-object constants within the compiled code lose their
inspectors. All syntax-object constants within compiled code acquire
the enclosing module's declaration-time inspector when the code is
loaded.}
不幸的是，因为它可以用除了@racket[编译（compile）]之外的其它方法合成，因此来自于一个@filepath{.zo}文件的被编译代码本质上是不可信的。当编译后的代码写入到一个@filepath{.zo}文件中，编译代码中的语法对象常量就失去了检查器。当代码加载时，已编译代码中的所有语法对象常量获得封闭模块的声明时间检查器。