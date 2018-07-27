#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-set"]{Assignment and Redefinition}}
@title[#:tag "module-set"]{赋值和重定义}

@;{The use of @racket[set!] on variables defined within a module is
limited to the body of the defining module. That is, a module is
allowed to change the value of its own definitions, and such changes
are visible to importing modules. However, an importing context is not
allowed to change the value of an imported binding.}
在一个模块中的变量定义上的@racket[set!]使用仅限于定义模块的主体。也就是说，一个模块被允许去改变它自己定义的值，这样的变化对于输入模块是可见的。但是，一个输入上下文不允许去更改一个被输入的绑定的值。

@examples[
(module m racket
  (provide counter increment!)
  (define counter 0)
  (define (increment!)
    (set! counter (add1 counter))))
(require 'm)
(eval:alts counter (eval 'counter))
(eval:alts (increment!) (eval '(increment!)))
(eval:alts counter (eval 'counter))
(eval:alts (set! counter -1) (eval '(set! counter -1)))
]

@;{As the above example illustrates, a module can always grant others the
ability to change its exports by providing a mutator function, such as
@racket[increment!].}
在上述例子中，一个模块通过提供一个突变函数总是能够授予其它能力以改变其输出，如@racket[increment!]。

@;{The prohibition on assignment of imported variables helps support
modular reasoning about programs. For example, in the module,}
在输入变量工作上的禁令有助于支持程序的模块化推理。例如，在这个模块中，

@racketblock[
(module m racket
  (provide rx:fish fishy-string?)
  (define rx:fish #rx"fish")
  (define (fishy-string? s)
    (regexp-match? rx:fish s)))
]

@;{the function @racket[fishy-string?] will always match strings that
contain ``fish'', no matter how other modules use the @racket[rx:fish]
binding.  For essentially the same reason that it helps programmers,
the prohibition on assignment to imports also allows many programs to
be executed more efficiently.}
函数@racket[fishy-string?]将始终匹配包含“fish”的字符串，不管其它模块如何使用@racket[rx:fish]绑定。出于同样的帮助程序员的原因，在对输入工作上的禁令也允许许多程序去被更有效地执行。

@;{Along the same lines, when a module contains no @racket[set!] of a
particular identifier that is defined within the module, then the
identifier is considered a @defterm{constant} that cannot be
changed---not even by re-declaring the module.}
在同一行中，当一个模块不包含一个在这个模块中定义的特定标识符的@racket[set!]，那么该标识符被认为一个不会被改变的@defterm{常量（constant）}——即使通过重新声明该模块。

@;{Consequently, re-declaration of a module is not generally allowed.
For file-based modules, simply changing the file does not lead to a
re-declaration in any case, because file-based modules are loaded on
demand, and the previously loaded declarations satisfy future
requests. It is possible to use Racket's reflection support to
re-declare a module, however, and non-file modules can be re-declared
in the @tech{REPL}; in such cases, the re-declaration may fail if it
involves the re-definition of a previously constant binding.}
因此，一个模块的重定义通常不被允许。对于基于文件的模块，简单地更改该文件不会导致任何情况下的一个重新声明，因为基于文件的模块是按需加载的，而先前加载的声明满足将来的请求。它有可能使用Racket的反射支持以重新声明一个模块，然而，非文件模块可以在@tech{REPL}中被重新声明；在这种情况下，如果重新申明涉及一个以前的静态绑定的重定义，也许会失败。

@interaction[
(module m racket
  (define pie 3.141597))
(require 'm)
(module m racket
  (define pie 3))
]

@;{For exploration and debugging purposes, the Racket reflective layer
provides a @racket[compile-enforce-module-constants] parameter
to disable the enforcement of constants.}
作为探索和调试目的，Racket反射层提供一个@racket[compile-enforce-module-constants]参数来使常量的执行无效。

@interaction[
(compile-enforce-module-constants #f)
(module m2 racket
  (provide pie)
  (define pie 3.141597))
(require 'm2)
(module m2 racket
  (provide pie)
  (define pie 3))
(compile-enforce-module-constants #t)
(eval:alts pie (eval 'pie))
]
