#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label rackunit))

@(define cake-eval (make-base-eval))

@;{@title{Module Syntax}}
@title[#:tag "Module-Syntax"]{模块语法}

@;{The @litchar{#lang} at the start of a module file begins a shorthand
for a @racket[module] form, much like @litchar{'} is a shorthand for a
@racket[quote] form. Unlike @litchar{'}, the @litchar{#lang}
shorthand does not work well in a @tech{REPL}, in part because it must be
terminated by an end-of-file, but also because the longhand expansion
of @litchar{#lang} depends on the name of the enclosing file.}
@litchar{#lang}在一个模块文件的开始，它开始一个对@racket[module]表的简写，就像@litchar{'}是一种对@racket[quote]表的简写。不同于@litchar{'}，@litchar{#lang}简写在@tech{REPL}内不能正常执行，部分是因为它必须由end-of-file（文件结束）终止，也因为@litchar{#lang}的普通写法依赖于封闭文件的名称。

@;------------------------------------------------------------------------
@;{@section[#:tag "module-syntax"]{The @racket[module] Form}}
@section[#:tag "module-syntax"]{@racket[module]表}

@;{The longhand form of a module declaration, which works in a
@tech{REPL} as well as a file, is}
一个模块声明的普通写法形式，既可在@tech{REPL}又可在一个文件中执行的是

@specform[
(module name-id initial-module-path
  decl ...)
]

@;{where the @racket[_name-id] is a name for the module,
@racket[_initial-module-path] is an initial import, and each
@racket[_decl] is an import, export, definition, or expression.  In
the case of a file, @racket[_name-id] normally matches the name of the
containing file, minus its directory path or file extension, but
@racket[_name-id] is ignored when the module is @racket[require]d
through its file's path.}
其中的@racket[_name-id]是一个模块名，@racket[_initial-module-path]是一个初始的导入口，每个@racket[_decl]是一个导入口、导出口、定义或表达式。在文件的情况下，@racket[_name-id]通常与包含文件的名称相匹配，减去其目录路径或文件扩展名，但在模块通过其文件路径@racket[require]时@racket[_name-id]被忽略。

@;{The @racket[_initial-module-path] is needed because even the
@racket[require] form must be imported for further use in the module
body. In other words, the @racket[_initial-module-path] import
bootstraps the syntax that is available in the body. The most commonly used
@racket[_initial-module-path] is @racketmodname[racket], which supplies most
of the bindings described in this guide, including @racket[require],
@racket[define], and @racket[provide]. Another commonly used
@racket[_initial-module-path] is @racketmodname[racket/base], which provides
less functionality, but still much of the most commonly needed
functions and syntax.}
@racket[_initial-module-path]是必需的，因为即使是@racket[require]表必须导入，以便在模块主体中进一步使用。换句话说，@racket[_initial-module-path]导入在主体内可供使用的引导语法。最常用的@racket[_initial-module-path]是@racketmodname[racket]，它提供了本指南中描述的大部分绑定，包括@racket[require]、@racket[define]和@racket[provide]。另一种常用的@racket[_initial-module-path]是@racketmodname[racket/base]，它提供了较少的函数，但仍然是大多数最常用的函数和语法。

@;{For example, the @filepath{cake.rkt} example of the
@seclink["module-basics"]{previous section} could be written as}
例如，@seclink["module-basics"]{前面一节}里的@filepath{cake.rkt}例子可以写为

@racketblock+eval[
#:eval cake-eval
(module cake racket
  (provide print-cake)

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline)))
]

@;{Furthermore, this @racket[module] form can be evaluated in a
@tech{REPL} to declare a @racket[cake] module that is not associated
with any file. To refer to such an unassociated module, quote the
module name:}
此外，@racket[module]表可以在@tech{REPL}中求值以申明一个@racket[cake]模块，不与任何文件相关联。为指向是这样一个独立模块，这样引用模块名称：

@examples[
#:eval cake-eval
(require 'cake)
(eval:alts (print-cake 3) (eval '(print-cake 3)))
]

@;{Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The module must be explicitly
@racket[require]d at the top level to trigger evaluation. After
evaluation is triggered once, later @racket[require]s do not
re-evaluate the module body.}
声明模块不会立即求值这个模块的主体定义和表达式。这个模块必须在顶层明确地被@racket[require]以来触发求值。在求值被触发一次之后，后续的@racket[require]不会重新对模块主体求值。

@examples[
(module hi racket
  (printf "Hello\n"))
(require 'hi)
(require 'hi)
]

@;------------------------------------------------------------------------
@;{@section[#:tag "hash-lang"]{The @racketmodfont{#lang} Shorthand}}
@section[#:tag "hash-lang"]{@racketmodfont{#lang}简写}

@;{The body of a @racketmodfont{#lang} shorthand has no specific syntax,
because the syntax is determined by the language name that follows
@racketmodfont{#lang}.}
@racketmodfont{#lang}简写的主体没有特定的语法，因为语法是由如下@racketmodfont{#lang}语言名称确定的。

@;{In the case of @racketmodfont{#lang} @racketmodname[racket], the syntax
is}
在@racketmodfont{#lang} @racketmodname[racket]的情况下，语法为：

@racketmod[
racket
_decl ...]

@;{which reads the same as}
其读作如下同一内容：

@racketblock[
(module _name racket
  _decl ...)
]

@;{where @racket[_name] is derived from the name of the file that
contains the @racketmodfont{#lang} form.}
这里的@racket[_name]是来自包含@racketmodfont{#lang}表的文件名称。

@;{The @racketmodfont{#lang} @racketmodname[racket/base] form has the same
syntax as @racketmodfont{#lang} @racketmodname[racket], except that
the longhand expansion uses @racketmodname[racket/base] instead of
@racketmodname[racket]. The @racketmodfont{#lang} @racketmodname[scribble/manual] form, in
contrast, has a completely different syntax that doesn't even look
like Racket, and which we do not attempt to describe in this guide.}
@racketmodfont{#lang} @racketmodname[racket/base]表具有和@racketmodfont{#lang} @racketmodname[racket]同样的语法，除了普通写法的扩展使用@racketmodname[racket/base]而不是@racketmodname[racket]。@racketmodfont{#lang} @racketmodname[scribble/manual]表相反，有一个完全不同的语法，甚至看起来不像Racket，在这个指南里我们不准备去描述。

@;{Unless otherwise specified, a module that is documented as a
``language'' using the @racketmodfont{#lang} notation will expand to
@racket[module] in the same way as @racketmodfont{#lang}
@racketmodname[racket]. The documented language name can be used
directly with @racket[module] or @racket[require], too.}
除非另有规定，一个模块是一个文档，它作为“语言”使用@racketmodfont{#lang}标记法表示将以和@racketmodfont{#lang}
@racketmodname[racket]同样的方式扩大到@racket[module]中。文档的语言名也可以直接使用@racket[module]或@racket[require]。

@; ----------------------------------------------------------------------
@;{@section[#:tag "submodules"]{Submodules}}
@section[#:tag "submodules"]{子模块}

@;{A @racket[module] form can be nested within a module, in which case
the nested @racket[module] form declares a
@deftech{submodule}. Submodules can be referenced directly by the
enclosing module using a quoted name. The following example prints
@racket["Tony"] by importing @racket[tiger] from the @racket[zoo]
submodule:}
一个@racket[module]表可以被嵌套在一个模块内，在这种情况下，这个嵌套@racket[module]表声明一个@deftech{子模块（submodule）}。子模块可以通过外围模块使用一个引用名称直接引用。下面的例子通过从@racket[zoo]子模块导入@racket[tiger]打印@racket["Tony"]：

@racketmod[
  #:file "park.rkt"
  racket

  (module zoo racket
    (provide tiger)
    (define tiger "Tony"))

  (require 'zoo)

  tiger
]

@;{Running a module does not necessarily run its submodules. In the above
example, running @filepath{park.rkt} runs its submodule @racket[zoo]
only because the @filepath{park.rkt} module @racket[require]s the
@racket[zoo] submodule. Otherwise, a module and each of its submodules can be run
independently. Furthermore, if @filepath{park.rkt} is compiled to a
bytecode file (via @exec{raco make}), then the code for
@filepath{park.rkt} or the code for @racket[zoo] can be loaded independently.}
运行一个模块不是必须运行子模块。在上面的例子中，运行@filepath{park.rkt}运行它的子模块@racket[zoo]仅因为@filepath{park.rkt}模块@racket[require]了这个@racket[zoo]子模块。否则，一个模块及其子模块可以独立运行。此外，如果@filepath{park.rkt}被编译成字节码文件（通过@exec{raco make}），那么@filepath{park.rkt}代码或@racket[zoo]代码可以独立下载。

@;{Submodules can be nested within submodules, and a submodule can be
referenced directly by a module other than its enclosing module by
using a @elemref["submod"]{submodule path}.}
子模块可以嵌套子模块，而且子模块可以被一个模块通过使用@elemref["submod"]{子模块路径（submodule path）}直接引用，不同于它的外围模块。

@;{A @racket[module*] form is similar to a nested @racket[module] form:}
一个@racket[module*]表类似于一个嵌套的@racket[module]表：

@specform[
(module* name-id initial-module-path-or-#f
  decl ...)
]

@;{The @racket[module*] form differs from @racket[module] in that it
inverts the possibilities for reference between the submodule and
enclosing module:}
@racket[module*]表不同于@racket[module]，它反转这个对于子模块和外围模块的参考的可能性：

@itemlist[

 @item{
  @;{A submodule declared with @racket[module] can be
       @racket[require]d by its enclosing module, but the submodule
       cannot @racket[require] the enclosing module or lexically
       reference the enclosing module's bindings.}
用@racket[module]申明的一个子模块模块可通过其外围模块@racket[require]，但子模块不能@racket[require]外围模块或在词法上参考外围模块的绑定。
 }

 @item{
  @;{A submodule declared with @racket[module*] can @racket[require]
       its enclosing module, but the enclosing module cannot
       @racket[require] the submodule.}
用@racket[module*]申明的一个子模块可以@racket[require]其外围模块，但外围模块不能@racket[require]子模块。
    }

]

@;{In addition, a @racket[module*] form can specify @racket[#f] in place of an
@racket[_initial-module-path], in which case the submodule sees all of
the enclosing module's bindings---including bindings that are not
exported via @racket[provide].}
此外，一个@racket[module*]表可以在@racket[_initial-module-path]的位置指定@racket[#f]，在这种情况下，所有外围模块的绑定对子模块可见——包括没有使用@racket[provide]输出的绑定。

@;{One use of submodules declared with @racket[module*] and @racket[#f] is
to export additional bindings through a submodule that are not
normally exported from the module:}
用@racket[module*]和@racket[#f]申明的子模块的一个应用是通过子模块输出附加绑定，那不是通常的从模块输出：

@racketmod[
#:file "cake.rkt"
racket

(provide print-cake)

(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))

(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))

(module* extras #f
  (provide show))
]

@;{In this revised @filepath{cake.rkt} module, @racket[show] is not
imported by a module that uses @racket[(require "cake.rkt")], since
most clients of @filepath{cake.rkt} will not want the extra function.  A
module can require the @racket[extra] @tech{submodule} using
@racket[(require (submod "cake.rkt" extras))] to access the otherwise
hidden @racket[show] function.@margin-note*{See @elemref["submod"]{submodule paths}
for more information on @racket[submod].}}
在这个修订的@filepath{cake.rkt}模块，@racket[show]不是被一个模块输入，它采用@racket[(require "cake.rkt")]，因为大部分@filepath{cake.rkt}的用户不想要那些额外的函数。一个模块可以要求@racket[extra]@tech{子模块（submodule）}使用@racket[(require (submod "cake.rkt" extras))]访问另外的隐藏的@racket[show]函数。

@; ----------------------------------------------------------------------
@;{@section[#:tag "main-and-test"]{Main and Test Submodules}}
@section[#:tag "main-and-test"]{main和test子模块}

@;{The following variant of @filepath{cake.rkt} includes a @racket[main]
submodule that calls @racket[print-cake]:}
下面@filepath{cake.rkt}的变体包括一个@racket[main]子模块，它调用@racket[print-cake]：

@racketmod[
#:file "cake.rkt"
racket

(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))

(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))

(module* main #f
  (print-cake 10))
]

@;{Running a module does not run its @racket[module*]-defined
submodules. Nevertheless, running the above module via @exec{racket}
or DrRacket prints a cake with 10 candles, because the @racket[main]
@tech{submodule} is a special case.}
运行一个模块不会运行@racket[module*]定义的子模块。尽管如此，还是可以通过@exec{racket}或DrRacket运行上面的模块打印一个带10支蜡烛的蛋糕，因为@racket[main]@tech{子模块（submodule）}是一个特殊情况。

@;{When a module is provided as a program name to the @exec{racket}
executable or run directly within DrRacket, if the module has a
@as-index{@racket[main] submodule}, the @racket[main] submodule is run
after its enclosing module. Declaring a @racket[main] submodule
thus specifies extra actions to be performed when a module is run directly,
instead of @racket[require]d as a library within a larger program.}
当一个模块作为一个可执行程序的名称提供给@exec{racket}在DrRacket中直接运行或执行在，如果模块有一个@as-index{@racket[main]子模块}，@racket[main]子模块会在其外围模块之后运行。当一个模块直接运行时，声明一个@racket[main]子模块从而指定额外的行为去被执行，以代替@racket[require]作为在一个较大程序里的一个库。

@;{A @racket[main] submodule does not have to be declared with
@racket[module*]. If the @racket[main] module does not need to use
bindings from its enclosing module, it can be declared with
@racket[module]. More commonly, @racket[main] is declared using
@racket[module+]:}
一个@racket[main]子模块不必用@racket[module*]声明。如果@racket[main]模块不需要使用其外围模块的绑定，则可以用@racket[module]声明它。更通常的是，@racket[main]使用@racket[module+]声明：

@specform[
(module+ name-id
  decl ...)
]

@;{A submodule declared with @racket[module+] is like one declared with
@racket[module*] using @racket[#f] as its
@racket[_initial-module-path].  In addition,
multiple @racket[module+] forms can specify the same submodule name,
in which case the bodies of the @racket[module+] forms are combined to
create a single submodule.}
用@racket[module+]申明的一个子模块就像一个由@racket[module*]用@racket[#f]代替@racket[_initial-module-path]申明的模块。此外，多个@racket[module+]表可以指定相同的子模块名称，在这种情况下，@racket[module+]表的主体被组合起来以创建一个单独的子模块。

@;{The combining behavior of @racket[module+] is particularly useful for
defining a @racket[test] submodule, which can be conveniently run
using @exec{raco test} in much the same way that @racket[main] is
conveniently run with @exec{racket}. For example, the following
@filepath{physics.rkt} module exports @racket[drop] and
@racket[to-energy] functions, and it defines a @racket[test] module to
hold unit tests:}
@racket[module+]的组合行为对定义一个@racket[test]子模块是非常有用的，它可以方便地使用@exec{raco test}运行，用同样的方式@racket[main]也可以方便地使用@exec{racket}运行。例如，下面的@filepath{physics.rkt}模块输出@racket[drop]和@racket[to-energy]函数，它定义了一个@racket[test]模块支持单元测试：

@racketmod[
#:file "physics.rkt"
racket
(module+ test
  (require rackunit)
  (define ε 1e-10))

(provide drop
         to-energy)

(define (drop t)
  (* 1/2 9.8 t t))

(module+ test
  (check-= (drop 0) 0 ε)
  (check-= (drop 10) 490 ε))

(define (to-energy m)
  (* m (expt 299792458.0 2)))

(module+ test
  (check-= (to-energy 0) 0 ε)
  (check-= (to-energy 1) 9e+16 1e+15))
]

@;{Importing @filepath{physics.rkt} into a larger program does not run
the @racket[drop] and @racket[to-energy] tests---or even trigger the
loading of the test code, if the module is compiled---but running
@exec{raco test physics.rkt} at a command line runs the tests.}
引入@filepath{physics.rkt}到一个更大的程序不会运行@racket[drop]和@racket[to-energy]测试——即使引发这个测试代码的加载，如果模块被编译——但在运行@exec{raco test physics.rkt}的时候会同时运行这个测试。

@;{The above @filepath{physics.rkt} module is equivalent to using
@racket[module*]:}
上述@filepath{physics.rkt}模块相当于使用@racket[module*]：

@racketmod[
#:file "physics.rkt"
racket

(provide drop
         to-energy)

(define (drop t)
  (* 1/2 #e9.8 t t))

(define (to-energy m)
  (* m (expt 299792458 2)))

(module* test #f
  (require rackunit)
  (define ε 1e-10)
  (check-= (drop 0) 0 ε)
  (check-= (drop 10) 490 ε)
  (check-= (to-energy 0) 0 ε)
  (check-= (to-energy 1) 9e+16 1e+15))
]

@;{Using @racket[module+] instead of @racket[module*] allows tests to be
interleaved with function definitions.}
使用@racket[module+]代替@racket[module*]允许测试与函数定义交叉。

@;{The combining behavior of @racket[module+] is also sometimes helpful
for a @racket[main] module. Even when combining is not needed,
@racket[(module+ main ....)] is preferred as it is more readable than
@racket[(module* main #f ....)].}
@racket[module+]的组合行为有时对@racket[main]模块也有帮助。即使组合是不需要的，@racket[(module+ main ....)]仍是首选，因为它比@racket[(module* main #f ....)]更具可读性。

@; ----------------------------------------------------------------------

@close-eval[cake-eval]
