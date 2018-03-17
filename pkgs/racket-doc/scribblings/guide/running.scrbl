#lang scribble/doc
@(require scribble/manual "guide-utils.rkt" (for-syntax racket/pretty))

@;{@title[#:tag "running" #:style 'toc]{Running and Creating Executables}}
@title[#:tag "running" #:style 'toc]{运行和创建可执行程序}

@;{While developing programs, many Racket programmers use the
@seclink["top" #:doc '(lib "scribblings/drracket/drracket.scrbl")
#:indirect? #t]{DrRacket} programming environment. To run a program without the
development environment, use @exec{racket} (for console-based
programs) or @exec{gracket} (for GUI programs). This chapter mainly
explains how to run @exec{racket} and @exec{gracket}.}
在开发程序时，很多Racket程序员使用@seclink["top" #:doc '(lib "scribblings/drracket/drracket.scrbl")
#:indirect? #t]{DrRacket}编程环境。要不用开发环境去运行一个程序，使用@exec{racket}（控制台应用程序）或@exec{gracket}（GUI程序）。本章主要说明如何运行@exec{racket}和@exec{gracket}。

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@;{@section[#:tag "racket"]{Running @exec{racket} and @exec{gracket}}}
@section[#:tag "racket"]{运行@exec{racket}和@exec{gracket}}

@;{The @exec{gracket} executable is the same as @exec{racket}, but with
small adjustments to behave as a GUI application rather than a console
application. For example, @exec{gracket} by default runs in
interactive mode with a GUI window instead of a console prompt. GUI
applications can be run with plain @exec{racket}, however.}
可执行的@exec{gracket}是和@exec{racket}一样的，但是与一个控制台应用程序相比，作为一个GUI（图形用户界面）应用程序的表现上有小的调整。例如，@exec{gracket}默认运行在GUI窗口交互方式代替控制台提示符。然而，GUI应用程序可以以普通的@exec{racket}方式运行。

@;{Depending on command-line arguments, @exec{racket} or @exec{gracket}
runs in @seclink["start-interactive-mode"]{interactive mode},
@seclink["start-module-mode"]{module mode}, or
@seclink["start-load-mode"]{load mode}.}
根据命令行参数、@exec{racket}或@exec{gracket}运行在@seclink["start-interactive-mode"]{交互模式（interactive mode）}、@seclink["start-module-mode"]{模块模式（module mode）}或@seclink["start-load-mode"]{加载模式（load mode）}下。

@;{@subsection[#:tag "start-interactive-mode"]{Interactive Mode}}
@subsection[#:tag "start-interactive-mode"]{交互模式}

When @exec{racket} is run with no command-line arguments (other than
confguration options, like @Flag{j}), then it starts a @tech{REPL}
with a @litchar{> } prompt:
当@exec{racket}用没有命令行参数（比其它的配置选项，如@Flag{j}）运行时，那么用一个@litchar{> }格式开始一个@tech{REPL}：

@verbatim[#:indent 2]{
  @(regexp-replace #rx"\n+$" (banner) "")
  > 
}

@;{@margin-note{For enhancing your @tech{REPL} experience, see
  @racketmodname[xrepl]; for information on GNU Readline support, see
  @racketmodname[readline].}}
@margin-note{为了提高你的@tech{REPL}经验，参见@racketmodname[xrepl]；为获取GNU Readline支持信息，参见@racketmodname[readline]。}

@;{To initialize the @tech{REPL}'s environment, @exec{racket} first
requires the @racketmodname[racket/init] module, which provides all of
@racket[racket], and also installs @racket[pretty-print] for display
results. Finally, @exec{racket} loads the file reported by
@racket[(find-system-path 'init-file)], if it exists, before starting
the @tech{REPL}.}
为初始化@tech{REPL}的环境，@exec{racket}首先需要@racketmodname[racket/init]模块，它提供了@racket[racket]的所有，并且为了显示结果还安装了@racket[pretty-print]。最后，如果存在的话，在启动@tech{REPL}之前@exec{racket}通过@racket[(find-system-path 'init-file)]加载文件报告。

@;{If any command-line arguments are provided (other than configuration
options), add @Flag{i} or @DFlag{repl} to re-enable the
@tech{REPL}. For example,}
如果任何命令行参数被提供（除了配置选项），添加@Flag{i}或@DFlag{repl}以重启REPL。例如,

@commandline{racket -e '(display "hi\n")' -i}

@;{displays ``hi'' on start-up, but still presents a @tech{REPL}.}
在启动时显示“hi”，但还是出现了一个@tech{REPL}。

@;{If module-requiring flags appear before @Flag{i}/@DFlag{repl}, they
cancel the automatic requiring of @racketmodname[racket/init]. This
behavior can be used to initialize the @tech{REPL}'s environment with
a different language. For example,}
如果模块导入标志出现在@Flag{i}/@DFlag{repl}之前，它们取消@racketmodname[racket/init]的自动导入。这种行为可以被用来用一个不同的语言初始化@tech{REPL}环境。例如,

@commandline{racket -l racket/base -i}

@;{starts a @tech{REPL} using a much smaller initial language (that loads
much faster). Beware that most modules do not provide the basic syntax
of Racket, including function-call syntax and @racket[require]. For
example,}
使用更小的初始语言（加载速度更快）启动@tech{REPL}。请注意，大多数模块都没有提供Racket的基本语法，包括函数调用语法和@racket[require]。例如,

@commandline{racket -l racket/date -i}

@;{produces a @tech{REPL} that fails for every expression, because
@racketmodname[racket/date] provides only a few functions, and not the
@racket[#%top-interaction] and @racket[#%app] bindings that are needed
to evaluate top-level function calls in the @tech{REPL}.}
产生一个@tech{REPL}，它在每一表达中失败，因为@racketmodname[racket/date]只提供很少的函数，而不是@racket[#%top-interaction]和@racket[#%app]绑定，它需要求值在@tech{REPL}里的顶层函数调用。

@;{If a module-requiring flag appears after @Flag{i}/@DFlag{repl} instead
of before it, then the module is required after
@racketmodname[racket/init] to augment the initial environment. For
example,}
如果一个模块导入标志出现在@Flag{i}/@DFlag{repl}之后而不是在它之前，那么模块在@racketmodname[racket/init]之后导入以增强初始环境。例如,

@commandline{racket -i -l racket/date}

@;{starts a useful @tech{REPL} with @racketmodname[racket/date] available
in addition to the exports of @racketmodname[racket].}
除了@racketmodname[racket]的导出，用可获得的@racketmodname[racket/date]开始一个有用的@tech{REPL}。

@; ----------------------------------------

@;{@subsection[#:tag "start-module-mode"]{Module Mode}}
@subsection[#:tag "start-module-mode"]{模块模式}

@;{If a file argument is supplied to @exec{racket} before any
command-line switch (other than configuration options), then the file
is required as a module, and (unless @Flag{i}/@DFlag{repl} is
specified), no @tech{REPL} is started. For example,}
如果一个文件参数在任何命令行开关（除了其它配置选项）之前提供给@exec{racket}，那么这个文件作为一个模块导入，没有@tech{REPL}启动。例如,

@commandline{racket hello.rkt}

@;{requires the @filepath{hello.rkt} module and then exits. Any argument
after the file name, flag or otherwise, is preserved as a command-line
argument for use by the required module via
@racket[current-command-line-arguments].}
导入@filepath{hello.rkt}模块，然后退出。在文件名、标志或其它文件名之后的任何参数都被保存为命令行参数，以便通过@racket[current-command-line-arguments]被导入模块使用。

@;{If command-line flags are used, then the @Flag{u} or
@DFlag{require-script} flag can be used to explicitly require a file
as a module.  The @Flag{t} or @DFlag{require} flag is similar, except
that additional command-line flags are processed by @exec{racket},
instead of preserved for the required module. For example,}
如果使用命令行标志，那么@Flag{u}或@DFlag{require-script}标志可以被用于显式地导入一个文件作为模块。 @Flag{t}或@DFlag{require}标志是相似的，除了额外的命令行标志由@exec{racket}处理，而不是保存为导入的模块。例如,

@commandline{racket -t hello.rkt -t goodbye.rkt}

@;{requires the @filepath{hello.rkt} module, then requires the
@filepath{goodbye.rkt} module, and then exits.}
导入@filepath{hello.rkt}模块，然后导入@filepath{goodbye.rkt}模块，再然后退出。

@;{The @Flag{l} or @DFlag{lib} flag is similar to
@Flag{t}/@DFlag{require}, but it requires a module using a
@racket[lib] module path instead of a file path. For example,}
@Flag{l}或@DFlag{lib}标志类似于@Flag{t}/@DFlag{require}，但它使用一个@racket[lib]模块路径而不是一个文件路径导入一个模块。例如,

@commandline{racket -l raco}

@;{is the same as running the @exec{raco} executable with no arguments,
since the @racket[raco] module is the executable's main module.}
同样不用参数运行可执行的@exec{raco}，因为@racket[raco]模块是可执行的的主模块。

@;{Note that if you wanted to pass command-line flags to
@racket[raco] above, you would need to protect the flags with a
@Flag{-}, so that @exec{racket} doesn't try to parse them itself:}
注意，如果你想传递命令行标志给上面的@racket[raco]，你需要用一个@Flag{-}保护这个标志，因此@exec{racket}不会试图解析它们本身：

@commandline{racket -l raco -- --help}

@; ----------------------------------------

@;{@subsection[#:tag "start-load-mode"]{Load Mode}}
@subsection[#:tag "start-load-mode"]{加载模式}

@;{The @Flag{f} or @DFlag{load} flag supports @racket[load]ing top-level
expressions in a file directly, as opposed to expressions within a
module file. This evaluation is like starting a @tech{REPL} and typing
the expressions directly, except that the results are not printed.
For example,}
@Flag{f}或@DFlag{load}标志支持直接@racket[load]一个文件中的顶层表达式，而不是一个模块文件中的表达式。这种求值就像开始一个@tech{REPL}并直接键入表达式，除非结果不打印。例如,

@commandline{racket -f hi.rkts}

@;{@racket[load]s @filepath{hi.rkts} and exits. Note that load mode is
generally a bad idea, for the reasons explained in
@secref["use-module"]; using module mode is typically better.}
@racket[load] @filepath{hi.rkts}并退出。请注意，加载模式通常是一个坏主意，在@secref["use-module"]中获取原因的讲解；使用模块模式通常更好。

@;{The @Flag{e} or @DFlag{eval} flag accepts an expression to evaluate
directly. Unlike file loading, the result of the expression is
printed, as in a @tech{REPL}. For example,}
@Flag{e}或@DFlag{eval}标志接受一个表达式直接求值。不同于文件加载，该表达式的结果被打印，就像在一个@tech{REPL}中。例如,

@commandline{racket -e '(current-seconds)'}

@;{prints the number of seconds since January 1, 1970.}
打印自1970年1月1日以来的秒数。

@;{For file loading and expression evaluation, the top-level environment
is created in the same way for
@seclink["start-interactive-mode"]{interactive mode}:
@racketmodname[racket/init] is required unless another module is
specified first. For example,}
为了文件加载和表达式求值，顶层环境以与@seclink["start-interactive-mode"]{交互模式（interactive mode）}相同的方式创建：除非首先指定另一个模块，否则必须导入@racketmodname[racket/init]。例如,

@commandline{racket -l racket/base -e '(current-seconds)'}

@;{likely runs faster, because it initializes the environment for
evaluation using the smaller @racketmodname[racket/base] language,
instead of @racketmodname[racket/init].}
可能运行得更快，因为它使用较小的@racketmodname[racket/base]语言来初始化求值环境，而不是用 @racketmodname[racket/init]。

@; ----------------------------------------------------------------------

@include-section["scripts.scrbl"]

@; ----------------------------------------------------------------------

@;{@section[#:tag "exe"]{Creating Stand-Alone Executables}}
@section[#:tag "exe"]{创建独立的可执行文件}

@(define raco-doc '(lib "scribblings/raco/raco.scrbl"))

@;{For information on creating and distributing executables, see
@secref[#:doc raco-doc "exe"] and @secref[#:doc raco-doc "exe-dist"] in
@other-manual[raco-doc].}
有关创建和分发的可执行文件的信息，参见在@other-manual[raco-doc]中的@secref[#:doc raco-doc "exe"]和@secref[#:doc raco-doc "exe-dist"]。

@other-manual[raco-doc].