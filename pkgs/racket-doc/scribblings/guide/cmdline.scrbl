#lang scribble/manual
@(require (only-in xrepl/doc-utils [cmd xreplcmd])
          "guide-utils.rkt")

@(define xrepl-doc '(lib "xrepl/xrepl.scrbl"))

@;{@title[#:tag "cmdline-tools"]{Command-Line Tools}}
@title[#:tag "cmdline-tools"]{命令行工具}

@;{Racket provides, as part of its standard distribution, a number of
command-line tools that can make racketeering more pleasant.}
Racket提供，作为其标准配置的一部分，许多命令行工具可以让racket使用者更愉快。

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@include-section["compile.scrbl"] @; raco

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Interactive evaluation}}
@section{互动求值}

@;{The Racket REPL provides everything you expect from a modern interactive
environment. For example, it provides an @xreplcmd{enter} command to have a
REPL that runs in the context of a given module, and an @xreplcmd{edit} command
to invoke your editor (as specified by the @envvar{EDITOR} environment
variable) on the file you entered. A @xreplcmd{drracket} command makes it easy
to use your favorite editor to write code, and still have DrRacket at hand to
try things out.}
Racket的REPL提供一个现代的一切互动环境的你的期望。例如，它提供了一个@xreplcmd{enter}命令以获取一个REPL运行在一个给定的模块的上下文里，并且一个@xreplcmd{edit}命令来调用你的编辑器（如@envvar{EDITOR}环境变量中指定的）在你输入的文件。一个@xreplcmd{drracket}命令可以很容易地使用你喜欢的编辑器编来写代码，而且还有DrRacket手试一试。

@;{For more information, see @other-doc[xrepl-doc].}
为了更多的利益，参见@other-doc[xrepl-doc]。

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@;{@section{Shell completion}}
@section{Shell补全}

@;{Shell auto-completion for @exec{bash} and @exec{zsh} is available in
@filepath{share/pkgs/shell-completion/racket-completion.bash} and
@filepath{share/pkgs/shell-completion/racket-completion.zsh},
respectively.}
对于@exec{bash}和@exec{zsh}的自动完在@filepath{share/pkgs/shell-completion/racket-completion.bash}和@filepath{share/pkgs/shell-completion/racket-completion.zsh}里是分别可获取的。

@;{To enable it, just run the appropriate file from your @tt{.bashrc} or
your @tt{.zshrc}.}
要启用它，只要从你的@tt{.bashrc}或者你的@tt{.zshrc}里运行相应的文件。

@;{The @filepath{shell-completion} collection is only available in the Racket Full
distribution. The completion scripts are also available
@hyperlink["https://github.com/racket/shell-completion"]{online}.}
@filepath{shell-completion}集合仅在Racket完全分发中可用。完成脚本也可@hyperlink["https://github.com/racket/shell-completion"]{在线（online）}使用。 