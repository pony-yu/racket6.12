#lang scribble/doc
@(require scribble/manual scribble/eval racket/class "guide-utils.rkt")

@;{@title[#:tag "reflection" #:style 'toc]{Reflection and Dynamic Evaluation}}
@title[#:tag "reflection" #:style 'toc]{反射和动态求值}

@;{Racket is a @italic{dynamic} language. It offers numerous facilities
for loading, compiling, and even constructing new code at run
time.}
Racket是一个动态的语言。它提供了许多用于加载、编译、甚至在运行时构造新代码的工具。

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@;{@section[#:tag "eval"]{@racket[eval]}}
@section[#:tag "eval"]{@racket[eval]}

@margin-note{
 @;{This example will not work within a module or in DrRacket's definitions window,
             but it will work in the interactions window, for reasons that are
             explained by the end of @secref["namespaces"].}
这个例子将不工作在一个模块之内或在DrRacket的定义窗口中，但它会工作在交互窗口中，原因在@secref["namespaces"]的末尾讲解。
   }

@;{The @racket[eval] function takes a representation of an expression or definition
(as a ``quoted'' form or @tech{syntax object}) and evaluates it:}
@racket[eval]函数构成一个表达或定义的表达（如“引用（quoted）”表或@tech{句法对象（syntax object）}）并且对它进行求值：

@interaction[
(eval '(+ 1 2))
]

@;{The power of @racket[eval] is that an expression can be
constructed dynamically:}
@racket[eval]函数的强大在于表达式可以动态构造：

@interaction[
(define (eval-formula formula)
  (eval `(let ([x 2]
               [y 3])
           ,formula)))
(eval-formula '(+ x y))
(eval-formula '(+ (* x y) y))
]

@;{Of course, if we just wanted to evaluate expressions with given values
for @racket[x] and @racket[y], we do not need @racket[eval]. A more
direct approach is to use first-class functions:}
当然，如果我们只是想计算表达式给出@racket[x]和@racket[y]的值，我们不需要@racket[eval]。更直接的方法是使用一级函数：

@interaction[
(define (apply-formula formula-proc)
  (formula-proc 2 3))
(apply-formula (lambda (x y) (+ x y)))
(apply-formula (lambda (x y) (+ (* x y) y)))
]

@;{However, if expressions like @racket[(+ x y)] and @racket[(+ (* x y)
y)] are read from a file supplied by a user, for example, then
@racket[eval] might be appropriate. Similarly, the @tech{REPL} reads
expressions that are typed by a user and uses @racket[eval] to
evaluate them.}
然而，譬如，如果表达式样@racket[(+ x y)]和@racket[(+ (* x y)
y)]是从用户提供的文件中读取，然后@racket[eval]可能是适当的。同样地，@tech{REPL}读取表达式，由用户输入，使用@racket[eval]求值。

@;{Also, @racket[eval] is often used directly or indirectly on whole
modules. For example, a program might load a module on demand using
@racket[dynamic-require], which is essentially a wrapper around
@racket[eval] to dynamically load the module code.}
一样地，在整个模块中@racket[eval]往往直接或间接地使用。例如，程序可以在定义域中用@racket[dynamic-require]读取一个模块，这基本上是一个封装在@racket[eval]中的动态加载模块的代码。

@; ----------------------------------------

@;{@subsection{Local Scopes}}
@subsection[#:tag "Local-Scopes"]{本地域}

@;{The @racket[eval] function cannot see local bindings in the context
where it is called. For example, calling @racket[eval] inside an
unquoted @racket[let] form to evaluate a formula does not make values
visible for @racket[x] and @racket[y]:}
@racket[eval]函数不能看到上下文中被调用的局部绑定。例如，调用在一个非引用的@racket[let]表中的@racket[eval]以对一个公式求值不会使得值@racket[x]和@racket[y]可见：

@interaction[
(define (broken-eval-formula formula)
  (let ([x 2]
        [y 3])
    (eval formula)))
(broken-eval-formula '(+ x y))
]

@;{The @racket[eval] function cannot see the @racket[x] and @racket[y]
bindings precisely because it is a function, and Racket is a lexically
scoped language. Imagine if @racket[eval] were implemented as}
@racket[eval]函数不能看到@racket[x]和@racket[y]的绑定，正是因为它是一个函数，并且Racket是词法作用域的语言。想象一下如果@racket[eval]被实现为

@racketblock[
(define (eval x)
  (eval-expanded (macro-expand x)))
]

@;{then at the point when @racket[eval-expanded] is called, the most
recent binding of @racket[x] is to the expression to evaluate, not the
@racket[let] binding in @racket[broken-eval-formula]. Lexical scope
prevents such confusing and fragile behavior, and consequently
prevents @racket[eval] from seeing local bindings in the context where
it is called.}
那么在@racket[eval-expanded]被调用的这个点上，@racket[x]最近的绑定是表达式求值，不是@racket[broken-eval-formula]中的@racket[let]绑定。词法范围防止这样的困惑和脆弱的行为，从而防止@racket[eval]表看到上下文中被调用的局部绑定。

@;{You might imagine that even though @racket[eval] cannot see the local
bindings in @racket[broken-eval-formula], there must actually be a
data structure mapping @racket[x] to @racket[2] and @racket[y] to
@racket[3], and you would like a way to get that data structure. In
fact, no such data structure exists; the compiler is free to replace
every use of @racket[x] with @racket[2] at compile time, so that the
local binding of @racket[x] does not exist in any concrete sense at
run-time. Even when variables cannot be eliminated by
constant-folding, normally the names of the variables can be
eliminated, and the data structures that hold local values do not
resemble a mapping from names to values.}
你可以想象，即使通过@racket[eval]不能看到@racket[broken-eval-formula]中的局部绑定，这里实际上必须是一个@racket[x]到@racket[2]和@racket[y]到@racket[3]的数据结构映射，以及你想办法得到那些数据结构。事实上，没有这样的数据结构存在；编译器可以自由地在编译时替换带有@racket[2]的@racket[x]的每一个使用，因此在运行时的任何具体意义上都不存在@racket[x]的局部绑定。即使变量不能通过常量折叠消除，通常也可以消除变量的名称，而保存局部值的数据结构与从名称到值的映射不一样。

@; ----------------------------------------

@;{@subsection[#:tag "namespaces"]{Namespaces}}
@subsection[#:tag "namespaces"]{命名空间}

@;{Since @racket[eval] cannot see the bindings from the context where it
is called, another mechanism is needed to determine dynamically
available bindings. A @deftech{namespace} is a first-class value that
encapsulates the bindings available for dynamic evaluation.}
由于@racket[eval]不能从它调用的上下文中看到绑定，另一种机制是需要确定动态可获得的绑定。一个@deftech{命名空间（namespace）}是一个一级的值，它封装了用于动态求值的可获得绑定。

@margin-note{
 @;{Informally, the term @defterm{namespace} is sometimes
 used interchangeably with @defterm{environment} or
 @defterm{scope}. In Racket, the term @defterm{namespace} has the
 more specific, dynamic meaning given above, and it should not be
 confused with static lexical concepts.}
通俗地说，术语@defterm{命名空间（namespace）}有时交替使用@defterm{环境（environment）}或@defterm{范围（scope）}。在Racket里，术语@defterm{命名空间（namespace）}有更具体的、动态的意义，并且它不应该和静态的词汇概念混淆。
   }

@;{Some functions, such as @racket[eval], accept an optional namespace
argument. More often, the namespace used by a dynamic operation is the
@deftech{current namespace} as determined by the
@racket[current-namespace] @tech{parameter}.}
某些函数，如@racket[eval]，接受一个可选的命名空间参数。通常，动态操作所使用的命名空间是@racket[current-namespace]@tech{参数}所确定的@deftech{当前命名空间（current namespace）}。

@;{When @racket[eval] is used in a @tech{REPL}, the current namespace is the one
that the @tech{REPL} uses for evaluating expressions. That's why the
following interaction successfully accesses @racket[x] via
@racket[eval]:}
当@racket[eval]在@tech{REPL}中使用时，当前命名空间是@tech{REPL}使用于求值表达式中的一个。这就是为什么下面的互动设计成功通过@racket[eval]访问@racket[x]的原因：

@interaction[
(define x 3)
(eval 'x)
]

@;{In contrast, try the following simple module and running it directly
in DrRacket or supplying the file as a command-line argument to
@exec{racket}:}
相反，尝试以下简单的模块并直接在DrRacket里或提供文件作为命令行参数给@exec{racket}运行它：

@racketmod[
racket

(eval '(cons 1 2))
]

@;{This fails because the initial current namespace is empty. When you
run @exec{racket} in interactive mode (see
@secref["start-interactive-mode"]), the initial namespace is
initialized with the exports of the @racket[racket] module, but when
you run a module directly, the initial namespace starts empty.}
这失败是因为初始当前命名空间是空的。当你在交互模式下运行@exec{racket}（见@secref["start-interactive-mode"]）时，初始的命名空间是用@racket[racket]模块的导出初始化的，但是当你直接运行一个模块时，初始的命名空间开始为空。

In general, it's a bad idea to use @racket[eval] with whatever
namespace happens to be installed. Instead, create a namespace
explicitly and install it for the call to eval:
在一般情况下，用任何命名空间安装结果来使用@racket[eval]一个坏主意。相反，明确地创建一个命名空间并安装它以调用去求值：

@racketmod[
racket

(define ns (make-base-namespace))
(eval '(cons 1 2) ns) (code:comment @#,t{works})
]

@;{The @racket[make-base-namespace] function creates a namespace that is
initialized with the exports of @racket[racket/base]. The later
section @secref["mk-namespace"] provides more information on creating
and configuring namespaces.}
@racket[make-base-namespace]函数创建一个命名空间，该命名空间是用@racket[racket/base]导出初始化的。后一部分@secref["mk-namespace"]提供了关于创建和配置名称空间的更多信息。

@; ----------------------------------------

@;{@subsection{Namespaces and Modules}}
@subsection[#:tag "Namespaces-and-Modules"]{命名空间和模块}

@;{As with @racket[let] bindings, lexical scope means that @racket[eval]
cannot automatically see the definitions of a @racket[module] in which
it is called. Unlike @racket[let] bindings, however, Racket provides a
way to reflect a module into a @tech{namespace}.}
为@racket[let]绑定，词法范围意味着@racket[eval]不能自动看到一个调用它的@racket[module]（模块）的定义。然而，和@racket[let]绑定不同的是，Racket提供了一种将模块反射到一个@tech{namespace（命名空间）}的方法。

@;{The @racket[module->namespace] function takes a quoted @tech{module
path} and produces a namespace for evaluating expressions and
definitions as if they appeared in the @racket[module] body:}
@racket[module->namespace]函数接受一个引用的@tech{模块路径（module path）}，并生成一个命名空间，用于对表达式和定义求值，就像它们出现在@racket[module]主体中一样：

@interaction[
(module m racket/base
  (define x 11))
(require 'm)
(define ns (module->namespace ''m))
(eval 'x ns)
]

@margin-note{
 @;{The double quoting in @racket[''m] is because @racket['m]
is a module path that refers to an interactively declared module, and
so @racket[''m] is the quoted form of the path.}
@racket[''m]中的双引号是因为@racket['m]是引用一个交互声明模块的模块路径，所以@racket[''m]是路径的引用表。
   }

@;{The @racket[module->namespace] function is mostly useful from outside
a module, where the module's full name is known. Inside a
@racket[module] form, however, the full name of a module may not be
known, because it may depend on where the module source is located
when it is eventually loaded.}
@racket[module->namespace]函数对来自于模块之外的模块是最有用的，在这里模块的全名是已知的。然而，在@racket[module]表内，模块的全名可能不知道，因为它可能取决于在最终加载时模块源位于何处。

From within a @racket[module], use @racket[define-namespace-anchor] to
declare a reflection hook on the module, and use
@racket[namespace-anchor->namespace] to reel in the module's
namespace:
在@racket[module]内，使用@racket[define-namespace-anchor]声明模块上的反射钩子，并使用@racket[namespace-anchor->namespace]在模块的命名空间中滚动：

@racketmod[
racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define x 1)
(define y 2)

(eval '(cons x y) ns) (code:comment @#,t{produces @racketresult[(1 . 2)]})
]


@; ----------------------------------------------------------------------

@;{@section[#:tag "mk-namespace"]{Manipulating Namespaces}}
@section[#:tag "mk-namespace"]{操纵的命名空间}

@;{A @tech{namespace} encapsulates two pieces of information:}
一个@tech{命名空间（namespace）}封装两条信息：

@itemize[

 @item{
  @;{A mapping from identifiers to bindings. For example, a
       namespace might map the identifier @racketidfont{lambda} to the
       @racket[lambda] form. An ``empty'' namespace is one that maps
       every identifier to an uninitialized top-level variable.}
从标识符到绑定的一个映射。例如，一个命名空间可以将标识符@racketidfont{lambda}映射到@racket[lambda]表。一个“空”的命名空间是一个映射之一，它映射每个标识符到一个未初始化的顶层变量。
    }

 @item{
  @;{A mapping from module names to module declarations and
       instances.}
    从模块名称到模块声明和实例的一个映射。
    }

]

@;{The first mapping is used for evaluating expressions in a top-level
context, as in @racket[(eval '(lambda (x) (+ x 1)))]. The second
mapping is used, for example, by @racket[dynamic-require] to locate a
module. The call @racket[(eval '(require racket/base))] normally uses
both pieces: the identifier mapping determines the binding of
@racketidfont{require}; if it turns out to mean @racket[require], then
the module mapping is used to locate the @racketmodname[racket/base]
module.}
第一个映射是用于对在一个顶层上下文中的表达式求值，如@racket[(eval '(lambda (x) (+ x 1)))]中的。第二个映射是用于定位模块，例如通过@racket[dynamic-require]。对@racket[(eval '(require racket/base))]的调用通常使用两部分：标识符映射确定@racket[require]的绑定；如果它原来的意思是@racketidfont{require}，那么模块映射用于定位@racketmodname[racket/base]模块。

@;{From the perspective of the core Racket run-time system, all
evaluation is reflective. Execution starts with an initial namespace
that contains a few primitive modules, and that is further populated
by loading files and modules as specified on the command line or as
supplied in the @tech{REPL}. Top-level @racket[require] and
@racket[define] forms adjusts the identifier mapping, and module
declarations (typically loaded on demand for a @racket[require] form)
adjust the module mapping.}
从核心Racket运行系统的角度来看，所有求值都是反射性的。执行从初始的命名空间包含一些原始的模块，并进一步由命令行上或在@tech{REPL}提供指定加载的文件和模块。顶层@racket[require]表和@racket[define]表调整标识符映射，模块声明（通常根据@racket[require]表加载）调整模块映射。

@; ----------------------------------------

@;{@subsection{Creating and Installing Namespaces}}
@subsection[#:tag "Creating-and-Installing-Namespaces"]{创建和安装命名空间}

@;{The function @racket[make-empty-namespace] creates a new, empty
@tech{namespace}. Since the namespace is truly empty, it cannot at
first be used to evaluate any top-level expression---not even
@racket[(require racket)]. In particular,}
函数@racket[make-empty-namespace]创建一个新的空命名空间。由于命名空间确实是空的，所以它不能首先用来求值任何顶级表达式——甚至不能求值@racket[(require racket)]。特别地,

@racketblock[
(parameterize ([current-namespace (make-empty-namespace)])
  (namespace-require 'racket))
]

@;{fails, because the namespace does not include the primitive modules on
which @racket[racket] is built.}
失败，因为命名空间不包括建立@racket[racket]的原始模块。

@;{To make a namespace useful, some modules must be @deftech{attached}
from an existing namespace. Attaching a module adjusts the mapping of
module names to instances by transitively copying entries (the module
and all its imports) from an existing namespace's mapping. Normally,
instead of just attaching the primitive modules---whose names and
organization are subject to change---a higher-level module is
attached, such as @racketmodname[racket] or
@racketmodname[racket/base].}
为了使命名空间有用，必须从现有命名空间中@deftech{附加（attached）}一些模块。附加模块通过从现有的命名空间的映射传递复制条目（模块及它的所有导入）调整模块名称映射到实例。通常情况下，而不是仅仅附加原始模块——其名称和组织有可能发生变化——附加一个高级模块，如@racketmodname[racket]或@racketmodname[racket/base]。

@;{The @racket[make-base-empty-namespace] function provides a namespace
that is empty, except that @racketmodname[racket/base] is
attached. The resulting namespace is still ``empty'' in the sense that
the identifiers-to-bindings part of the namespace has no mappings;
only the module mapping has been populated. Nevertheless, with an
initial module mapping, further modules can be loaded.}
@racket[make-base-empty-namespace]函数提供一个空的命名空间，除非附加了@racketmodname[racket/base]。生成的命名空间仍然是“空的”，在这个意义上，绑定名称空间部分的标识符没有映射；只有模块映射已经填充。然而，通过初始模块映射，可以加载更多模块。

@;{A namespace created with @racket[make-base-empty-namespace] is
suitable for many basic dynamic tasks. For example, suppose that a
@racketmodfont{my-dsl} library implements a domain-specific language
in which you want to execute commands from a user-specified file. A
namespace created with @racket[make-base-empty-namespace] is enough to
get started:}
一个用@racket[make-base-empty-namespace]创建的命名空间适合于许多基本的动态任务。例如，假设@racketmodfont{my-dsl}库实现了一个特定定义域的语言，你希望在其中执行来自用户指定文件的命令。一个用@racket[make-base-empty-namespace]的命名空间足以启动：

@racketblock[
(define (run-dsl file)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require 'my-dsl)
    (load file)))
]

@;{Note that the @racket[parameterize] of @racket[current-namespace] does
not affect the meaning of identifiers like @racket[namespace-require]
within the @racket[parameterize] body. Those identifiers obtain their
meaning from the enclosing context (probably a module). Only
expressions that are dynamic with respect to this code, such as the
content of @racket[load]ed files, are affected by the
@racket[parameterize].}
注意，@racket[current-namespace]的@racket[parameterize]（参数）不影响像在@racket[parameterize]主体中的@racket[namespace-require]那样的标识符的意义。这些标识符从封闭上下文（可能是一个模块）获得它们的含义。只有对代码具有动态性的表达式，如@racket[load]（加载）的文件的内容，通过@racket[parameterize]（参数化）影响。

@;{Another subtle point in the above example is the use of
@racket[(namespace-require 'my-dsl)] instead of @racket[(eval
'(require my-dsl))]. The latter would not work, because @racket[eval]
needs to obtain a meaning for @racket[require] in the namespace, and
the namespace's identifier mapping is initially empty. The
@racket[namespace-require] function, in contrast, directly imports the
given module into the current namespace.  Starting with
@racket[(namespace-require 'racket/base)] would introduce a binding
for @racketidfont{require} and make a subsequent @racket[(eval
'(require my-dsl))] work. The above is better, not only because it is
more compact, but also because it avoids introducing bindings that are
not part of the domain-specific languages.}
在上面的例子中，一个微妙的一点是使用@racket[(namespace-require 'my-dsl)]代替@racket[(eval
'(require my-dsl))]。后者不会运行，因为@racket[eval]需要对在命名空间中的@racket[require]获得意义，并且命名空间的标识符映射最初是空的。与此相反，@racket[namespace-require]函数直接将给定的模块导入（require）当前命名空间。从@racket[(namespace-require 'racket/base)]运行。从@racket[(namespace-require 'racket/base)]将为@racketidfont{require}引入绑定并使后续的@racket[(eval
'(require my-dsl))]运行。上面的比较好，不仅仅是因为它更紧凑，还因为它避免引入不属于特定领域语言的绑定。

@; ----------------------------------------

@;{@subsection{Sharing Data and Code Across Namespaces}}
@subsection[#:tag "Sharing-Data-and-Code-Across-Namespaces"]{共享数据和代码的命名空间}

@;{Modules not attached to a new namespace will be loaded and
instantiated afresh if they are demanded by evaluation. For example,
@racketmodname[racket/base] does not include
@racketmodname[racket/class], and loading @racketmodname[racket/class]
again will create a distinct class datatype:}
如果不需要对新命名空间附加的模块，则将重新加载并实例化它们。例如，@racketmodname[racket/base]不包括@racketmodname[racket/class]，加载@racketmodname[racket/class]又将创造一个不同的类数据类型：

@interaction[
(require racket/class)
(class? object%)
(class?
 (parameterize ([current-namespace (make-base-empty-namespace)])
   (namespace-require 'racket/class) (code:comment @#,t{loads again})
   (eval 'object%)))
]

@;{For cases when dynamically loaded code needs to share more code and
data with its context, use the @racket[namespace-attach-module]
function. The first argument to @racket[namespace-attach-module] is a
source namespace from which to draw a module instance; in some cases,
the current namespace is known to include the module that needs to be
shared:}
对于动态加载的代码需要与其上下文共享更多代码和数据的，使用@racket[namespace-attach-module]函数。 @racket[namespace-attach-module]的第一个参数是从中提取模块实例的源命名空间；在某些情况下，已知的当前命名空间包含需要共享的模块：

@interaction[
(require racket/class)
(class?
 (let ([ns (make-base-empty-namespace)])
   (namespace-attach-module (current-namespace)
                            'racket/class
                            ns)
   (parameterize ([current-namespace ns])
     (namespace-require 'racket/class) (code:comment @#,t{uses attached})
     (eval 'object%))))
]

@;{Within a module, however, the combination of
@racket[define-namespace-anchor] and
@racket[namespace-anchor->empty-namespace] offers a more reliable
method for obtaining a source namespace:}
然而，在一个模块中，@racket[define-namespace-anchor]和@racket[namespace-anchor->empty-namespace]的组合提供了一种更可靠的获取源命名空间的方法：

@racketmod[
racket/base

(require racket/class)

(define-namespace-anchor a)

(define (load-plug-in file)
  (let ([ns (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace a)
                             'racket/class
                              ns)
    (parameterize ([current-namespace ns])
      (dynamic-require file 'plug-in%))))
]

@;{The anchor bound by @racket[namespace-attach-module] connects the
run time of a module with the namespace in which a module is loaded
(which might differ from the current namespace).  In the above
example, since the enclosing module requires
@racketmodname[racket/class], the namespace produced by
@racket[namespace-anchor->empty-namespace] certainly contains an
instance of @racketmodname[racket/class]. Moreover, that instance is
the same as the one imported into the module, so the class datatype is
shared.}
由@racket[namespace-attach-module]绑定的锚将模块的运行时间与加载模块的命名空间（可能与当前命名空间不同）连接在一起。在上面的示例中，由于封闭模块需要@racketmodname[racket/class]，由@racket[namespace-anchor->empty-namespace]生成的名称空间肯定包含了一个@racketmodname[racket/class]的实例。此外，该实例与一个导入模块的一个相同，因此类数据类型共享。

@; ----------------------------------------------------------------------

@;{@section[#:tag "load"]{Scripting Evaluation and Using @racket[load]}}
@section[#:tag "load"]{脚本求值和使用@racket[load]}

@;{Historically, Lisp implementations did not offer module
systems. Instead, large programs were built by essentially scripting
the @tech{REPL} to evaluate program fragments in a particular order.
While @tech{REPL} scripting turns out to be a bad way to structure
programs and libraries, it is still sometimes a useful capability.}
从历史上看，Lisp实现没有提供模块系统。相反，大的程序是由基本的脚本@tech{REPL}来求值一个特定的顺序的程序片段。而@tech{REPL}脚本是结构化程序和库的好办法，它仍然有时是一个有用的性能。

@margin-note{
 @;{Describing a program via @racket[load] interacts
especially badly with macro-defined language extensions
@cite["Flatt02"].}
   通过@racket[load]用宏定义语言扩展@cite["Flatt02"]描述一个程序其交互特别差。
   }

@;{The @racket[load] function runs a @tech{REPL} script by
@racket[read]ing S-expressions from a file, one by one, and passing
them to @racket[eval]. If a file @filepath{place.rkts} contains}
@racket[load]函数通过从文件中一个接一个地@racket[read]（读取）S表达式来运行一个@tech{REPL}脚本，并把它们传递给@racket[eval]。如果一个文件@filepath{place.rkts}包含以下内容

@racketblock[
(define city "Salt Lake City")
(define state "Utah")
(printf "~a, ~a\n" city state)
]

@;{then it can be loaded in a @tech{REPL}:}
那么，它可以@racket[load]（加载）进一个@tech{REPL}：

@interaction[
(eval:alts (load "place.rkts") (begin (define city "Salt Lake City")
                                     (printf "~a, Utah\n" city)))
city
]

@;{Since @racket[load] uses @racket[eval], however, a module like the
following generally will not work---for the same reasons described in
@secref["namespaces"]:}
然而，由于@racket[load]使用@racket[eval]，像下面的一个模块一般不会运行——基于@secref["namespaces"]中的相同原因描述：

@racketmod[
racket

(define there "Utopia")

(load "here.rkts")
]

@;{The current namespace for evaluating the content of
@filepath{here.rkts} is likely to be empty; in any case, you cannot get
@racket[there] from @filepath{here.rkts}. Also, any definitions in
@filepath{here.rkts} will not become visible for use within the module;
after all, the @racket[load] happens dynamically, while references to
identifiers within the module are resolved lexically, and therefore
statically.}
对求值@filepath{here.rkts}的上下文的当前命名空间可能是空的；在任何情况下，你不能从@filepath{here.rkts}到@racket[there]（那里）。同时，在@filepath{here.rkts}里的任何定义对模块里的使用不会变得可见；毕竟，@racket[load]是动态发生，而在模块标识符引用是从词法上解决，因此是静态的。

@;{Unlike @racket[eval], @racket[load] does not accept a namespace
argument. To supply a namespace to @racket[load], set the
@racket[current-namespace] @tech{parameter}. The following example evaluates
the expressions in @filepath{here.rkts} using the bindings of the
@racketmodname[racket/base] module:}
不像@racket[eval]，@racket[load]不接受一个命名空间的参数。为了提供一个用于@racket[load]的命名空间，设置@racket[current-namespace]@tech{参数（parameter）}。下面的示例求值在@filepath{here.rkts}中使用@racketmodname[racket/base]模块绑定的表达式：

@racketmod[
racket

(parameterize ([current-namespace (make-base-namespace)])
  (load "here.rkts"))
]

@;{You can even use @racket[namespace-anchor->namespace] to make the
bindings of the enclosing module accessible for dynamic evaluation. In
the following example, when @filepath{here.rkts} is @racket[load]ed, it
can refer to @racket[there] as well as the bindings of
@racketmodname[racket]:}
你甚至可以使用@racket[namespace-anchor->namespace]使封闭模块的绑定可用于动态求值。在下面的例子中，当@filepath{here.rkts}被@racket[load]（加载）时，它既可以指@racket[there]，也可以指@racketmodname[racket]的绑定：

@racketmod[
racket

(define there "Utopia")

(define-namespace-anchor a)
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (load "here.rkts"))
]

@;{Still, if @filepath{here.rkts} defines any identifiers, the definitions
cannot be directly (i.e., statically) referenced by in the enclosing
module.}
不过，如果@filepath{here.rkts}定义任意的标识符，这个定义不能直接（即静态地）在外围模块中引用。

@;{The @racketmodname[racket/load] module language is different from
@racketmodname[racket] or @racketmodname[racket/base]. A module using
@racketmodname[racket/load] treats all of its content as dynamic,
passing each form in the module body to @racket[eval] (using a
namespace that is initialized with @racketmodname[racket]). As a
result, uses of @racket[eval] and @racket[load] in the module body see
the same dynamic namespace as immediate body forms. For example, if
@filepath{here.rkts} contains}
@racketmodname[racket/load]模块语言不同于@racketmodname[racket]或@racketmodname[racket/base]。一个模块使用@racketmodname[racket/load]对其所有上下文以动态对待，通过模块主体里的每一个表去@racket[eval]（使用以@racketmodname[racket]初始化的命名空间）。作为一个结果，@racket[eval]和@racket[load]在模块中的使用看到相同的动态命名空间作为直接主体表。例如，如果@filepath{here.rkts}包含以下内容

@racketblock[
(define here "Morporkia")
(define (go!) (set! here there))
]

@;{then running}
那么运行

@racketmod[
racket/load

(define there "Utopia")

(load "here.rkts")

(go!)
(printf "~a\n" here)
]

@;{prints ``Utopia''.}
打印“Utopia”。

@;{Drawbacks of using @racketmodname[racket/load] include reduced
error checking, tool support, and performance. For example, with the
program}
使用@racketmodname[racket/load]的缺点包括减少错误检查、工具支持和性能。例如，用程序

@racketmod[
racket/load

(define good 5)
(printf "running\n")
good
bad
]

@;{DrRacket's @onscreen{Check Syntax} tool cannot tell that the second
@racket[good] is a reference to the first, and the unbound reference
to @racket[bad] is reported only at run time instead of rejected
syntactically.}
DrRacket的@onscreen{语法检查（Check Syntax）}工具不能告诉第二个@racket[good]是对第一个的参考，而对@racket[bad]的非绑定参考仅在运行时报告而不是在语法上拒绝。
