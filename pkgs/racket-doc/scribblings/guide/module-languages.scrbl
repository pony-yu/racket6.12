#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "modfile.rkt"
          (for-label racket/date))

@;{@title[#:tag "module-languages"]{Module Languages}}
@title[#:tag "module-languages"]{模块（module）语言}

@;{When using the longhand @racket[module] form for writing modules, the
module path that is specified after the new module's name provides the
initial imports for the module. Since the initial-import module
determines even the most basic bindings that are available in a
module's body, such as @racket[require], the initial import can be
called a @deftech{module language}.}
当使用手写@racket[module]表来书写模块，模块路径在新模块名提供模块最初导入后指定。由于初始导入模块决定了模块主体中可用的最基本绑定，就像@racket[require]，最初的导入可以被称为一个@deftech{模块语言（module language）}。

@;{The most common @tech{module languages} are @racketmodname[racket] or
@racketmodname[racket/base], but you can define your own
@tech{module language} by defining a suitable module. For example,
using @racket[provide] subforms like @racket[all-from-out],
@racket[except-out], and @racket[rename-out], you can add, remove, or
rename bindings from @racketmodname[racket] to produce a @tech{module
language} that is a variant of @racketmodname[racket]:}
最常见的@tech{模块语言（module languages）}是@racketmodname[racket]或@racketmodname[racket/base]，但你可以通过定义一个合适的模块定义你自己的@tech{模块语言（module languages）}。例如，使用像@racket[all-from-out]、@racket[except-out]和@racket[rename-out]那样的@racket[provide]子表，你可以添加、删除或重命名绑定 从@racketmodname[racket]创作一个@tech{模块语言（module languages）}，这个语言是@racketmodname[racket]}的一个变体：

@;{@guideother{@secref["module-syntax"] introduces the longhand
@racket[module] form.}}
《@secref["module-syntax"]介绍了@racket[module]表的正常写法。

@interaction[
(module raquet racket
  (provide (except-out (all-from-out racket) lambda)
           (rename-out [lambda function])))
(module score 'raquet
  (map (function (points) (case points
                           [(0) "love"] [(1) "fifteen"]
                           [(2) "thirty"] [(3) "forty"]))
       (list 0 2)))
(require 'score)
]

@; ----------------------------------------
@;{@section[#:tag "implicit-forms"]{Implicit Form Bindings}}
@section[#:tag "implicit-forms"]{隐式绑定}

@;{If you try to remove too much from @racketmodname[racket] in defining
your own @tech{module language}, then the resulting module
will no longer work right as a @tech{module language}:}
如果你试图从定义你自己的@tech{模块语言（module language）}中的@racketmodname[racket]里删除太多  ，那么生成的模块将不会作为一个@tech{模块语言（module language）}正确工作：

@interaction[
(module just-lambda racket
  (provide lambda))
(module identity 'just-lambda
  (lambda (x) x))
]

@;{The @racket[#%module-begin] form is an implicit form that wraps the
body of a module. It must be provided by a module that is to be used
as @tech{module language}:}
@racket[#%module-begin]表是一个隐式表，它封装一个模块的主体。它必须由一个模块来提供，该模块将作为@tech{模块语言（module language）}使用：

@interaction[
(module just-lambda racket
  (provide lambda #%module-begin))
(module identity 'just-lambda
  (lambda (x) x))
(require 'identity)
]

@;{The other implicit forms provided by @racket[racket/base] are
@racket[#%app] for function calls, @racket[#%datum] for literals, and
@racket[#%top] for identifiers that have no binding:}
被@racket[racket/base]提供的其它隐式表是给函数调用的@racket[#%app]、给字面量的@racket[#%datum]和给没有绑定标识符的@racket[#%top]：

@interaction[
(module just-lambda racket
  (provide lambda #%module-begin
           (code:comment @#,t{@racketidfont{ten} needs these, too:})
           #%app #%datum))
(module ten 'just-lambda
  ((lambda (x) x) 10))
(require 'ten)
]

@;{Implicit forms such as @racket[#%app] can be used explicitly in a module,
but they exist mainly to allow a module language to restrict or change
the meaning of implicit uses. For example, a @racket[lambda-calculus]
@tech{module language} might restrict functions to a single argument,
restrict function calls to supply a single argument, restrict the
module body to a single expression, disallow literals, and treat
unbound identifiers as uninterpreted symbols:}
像@racket[#%app]这样的隐式表可以在一个模块中明确地使用，但它们的存在主要是为了使一个模块语言限制或改变隐式使用的意义。例如，一个@racket[lambda-calculus]@tech{模块语言（module language）}可能限制函数为一个单一的参数，限制函数调用提供一个单一的参数，限制模块主体到一个单一的表达式，不允许字面语法，以及处理非绑定标识符作为无解释的符号：

@interaction[
(module lambda-calculus racket
  (provide (rename-out [1-arg-lambda lambda]
                       [1-arg-app #%app]
                       [1-form-module-begin #%module-begin]
                       [no-literals #%datum]
                       [unbound-as-quoted #%top]))
  (define-syntax-rule (1-arg-lambda (x) expr)
    (lambda (x) expr))
  (define-syntax-rule (1-arg-app e1 e2)
    (#%app e1 e2))
  (define-syntax-rule (1-form-module-begin e)
    (#%module-begin e))
  (define-syntax (no-literals stx)
    (raise-syntax-error #f "no" stx))
  (define-syntax-rule (unbound-as-quoted . id)
    'id))
(module ok 'lambda-calculus
  ((lambda (x) (x z))
   (lambda (y) y)))
(require 'ok)
(module not-ok 'lambda-calculus
  (lambda (x y) x))
(module not-ok 'lambda-calculus
  (lambda (x) x)
  (lambda (y) (y y)))
(module not-ok 'lambda-calculus
  (lambda (x) (x x x)))
(module not-ok 'lambda-calculus
  10)
]

@;{Module languages rarely redefine @racket[#%app], @racket[#%datum], and
@racket[#%top], but redefining @racket[#%module-begin] is more
frequently useful. For example, when using modules to construct
descriptions of HTML pages where a description is exported from the
module as @racketidfont{page}, an alternate @racket[#%module-begin]
can help eliminate @racket[provide] and quasiquoting
boilerplate, as in @filepath{html.rkt}:}
模块语言很少重定@racket[#%app]、@racket[#%datum]和@racket[#%top]，但重定义@racket[#%module-begin]往往更为有用。例如，当使用模块构建HTML页面的描述，那里一个描述以@racketidfont{页（page）}的形式从模块被导出，一个交替的@racket[#%module-begin]能帮助消除@racket[provide]和反引用（Quasiquoting）样板文件，就像在@filepath{html.rkt}那样：

@racketmodfile["html.rkt"]

@;{Using the @filepath{html.rkt} @tech{module language}, a simple web page
can be described without having to explicitly define or export
@racketidfont{page} and starting in @racket[quasiquote]d mode instead
of expression mode:}
使用@filepath{html.rkt}@tech{模块语言（module language）}，一个简单的网页可以不需要明确的定义或导出@racketidfont{页（page）}而被描述，并且以@racket[quasiquote]模式开始而不是表达式模式开始：

@interaction[
(module lady-with-the-spinning-head "html.rkt"
  (title "Queen of Diamonds")
  (p "Updated: " ,(now)))
(require 'lady-with-the-spinning-head)
page
]

@; ----------------------------------------
@;{@section[#:tag "s-exp"]{Using @racket[@#,hash-lang[] @#,racketmodname[s-exp]]}}
@section[#:tag "s-exp"]{使用@racket[@#,hash-lang[] @#,racketmodname[s-exp]]}

@;{Implementing a language at the level of @hash-lang[] is more complex
than declaring a single module, because @hash-lang[] lets programmers
control several different facets of a language. The
@racketmodname[s-exp] language, however, acts as a kind of
meta-language for using a @tech{module language} with the
@hash-lang[] shorthand:}
在@hash-lang[]级别上实现一个语言比声明一个单一的模块要复杂得多，因为@hash-lang[]允许程序员控制语言的几个不同方面。然而，@racketmodname[s-exp]语言，为了使用带@hash-lang[]简写的@tech{模块语言（module language）}扮演了一种元语言：

@racketmod[
s-exp _module-name
_form ...]

@;{is the same as}
和以下内容是一样的

@racketblock[
(module _name _module-name
  _form ...)
]

@;{where @racket[_name] is derived from the source file containing the
@hash-lang[] program. The name @racketmodname[s-exp] is short for
``@as-index{S-expression},'' which is a traditional name for
Racket's @tech{reader}-level lexical conventions: parentheses,
identifiers, numbers, double-quoted strings with certain backslash
escapes, and so on.}
这里@racket[_name]来自包含@hash-lang[]程序的源文件。这个名称@racketmodname[s-exp]是@as-index{S-expression}的简写形式，这是一个@tech{读取器（reader）}层的传统的名称的 词法约定：括号、标识符、数字、带反斜杠转义的双引号字符串等等。

@;{Using @racket[@#,hash-lang[] @#,racketmodname[s-exp]], the
@racket[lady-with-the-spinning-head] example from before can be
written more compactly as:}
使用@racket[@#,hash-lang[] @#,racketmodname[s-exp]]，这个来自于以前的@racket[lady-with-the-spinning-head]例子可以写得更紧凑：

@racketmod[
s-exp "html.rkt"

(title "Queen of Diamonds")
(p "Updated: " ,(now))
]

@;{Later in this guide, @secref["hash-languages"] explains how to define
your own @hash-lang[] language, but first we explain how you can write
@tech{reader}-level extensions to Racket.}
在这个指南的稍后边，《@secref["hash-languages"]》会讲解如何定义自己的@hash-lang[]语言，但是首先我们讲解你如何写针对Racket@tech{读取器（reader）}级的扩展。