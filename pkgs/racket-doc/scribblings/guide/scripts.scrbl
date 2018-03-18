#lang scribble/doc
@(require scribble/manual scheme/cmdline "guide-utils.rkt")

@;{@title[#:tag "scripts"]{Scripts}}
@title[#:tag "scripts"]{脚本}

@;{Racket files can be turned into executable scripts on Unix and Mac
OS.  On Windows, a compatibility layer like Cygwin support the
same kind of scripts, or scripts can be implemented as batch files.}
在UNIX和Mac OS上，Racket文件可以转换成可执行脚本。在Windows中，一个兼容层如Cygwin支持相同的脚本，或者脚本可以作为批处理文件执行。

@;{@section{Unix Scripts}}
@section[#:tag "Unix_Scripts"]{Unix脚本}

@;{In a Unix environment (including Linux and Mac OS), a Racket file can
be turned into an executable script using the shell's @as-index{@tt{#!}}
convention. The first two characters of the file must be @litchar{#!};
the next character must be either a space or @litchar{/}, and the
remainder of the first line must be a command to execute the script. For
some platforms, the total length of the first line is restricted to 32
characters, and sometimes the space is required.}
在一个UNIX环境里（包括Linux和Mac OS），一个Racket文件可以使用shell的@as-index{@tt{#!}}公约变成一个可执行的脚本。该文件的最先两个字符必须是@litchar{#!}；接下来的字符必须是一个空格或@litchar{/}，并且第一行的其余部分必须是执行脚本的命令。对于某些平台，第一行的总长度限制为32个字符，而且有时需要空格。

@;{@margin-note{Use @racketmodfont{#lang} @racketmodname[racket/base] instead
of @racketmodfont{#lang} @racketmodname[racket] to produce scripts with a
faster startup time.}}
@margin-note{使用@racketmodfont{#lang} @racketmodname[racket/base]而不是@racketmodfont{#lang} @racketmodname[racket]产生具有更快的启动时间的脚本。}

@;{The simplest script format uses an absolute path to a @exec{racket}
executable followed by a module declaration. For example, if
@exec{racket} is installed in @filepath{/usr/local/bin}, then a file
containing the following text acts as a ``hello world'' script:}
最简单的脚本格式使用一个绝对路径到一个@exec{racket}可执行文件，随后是一个模块声明。例如，如果@exec{racket}被安装在@filepath{/usr/local/bin}里，那么一个文件包含以下文本作为一个“Hello World”脚本：

@verbatim[#:indent 2]{
  #! /usr/local/bin/racket
  #lang racket/base
  "Hello, world!"
}

@;{In particular, if the above is put into a file @filepath{hello} and
the file is made executable (e.g., with @exec{chmod a+x hello}), then
typing @exec{./hello} at the shell prompt produces the output
@tt{"Hello, world!"}.}
特别是，如果以上都放在一个文件@filepath{hello}里并且这个文件被制作成可执行文件（例如，用@exec{chmod a+x hello}），然后在shell提示符下键入@exec{./hello}产生输出@tt{"Hello, world!"}。

@;{The above script works because the operating system automatically puts
the path to the script as the argument to the program started by the
@tt{#!} line, and because @exec{racket} treats a single non-flag
argument as a file containing a module to run.}
上面的脚本由于操作系统自动将路作为参数添加到脚本让程序通过@tt{#!}启动，并且因为@exec{racket}将单个非标志参数视为一个包含要运行的模块的文件。

@;{Instead of specifying a complete path to the @exec{racket}
executable, a popular alternative is to require that @exec{racket}
is in the user's command path, and then ``trampoline'' using
@exec{/usr/bin/env}:}
代替指定的@exec{racket}可执行文件的完整路径，一种流行的替代方法是要求@exec{racket}在用户的命令路径中。，然后“trampoline”使用@exec{/usr/bin/env}：

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket/base
  "Hello, world!"
}

@;{In either case, command-line arguments to a script are available via
@racket[current-command-line-arguments]:}
在这两种情况下，一个脚本的命令行参数都可以通过@racket[current-command-line-arguments]获得：

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket/base
  (printf "Given arguments: ~s\n"
          (current-command-line-arguments))
}

@;{If the name of the script is needed, it is available via
@racket[(find-system-path 'run-file)], instead of via
@racket[(current-command-line-arguments)].}
如果需要脚本名称，可以通过@racket[(find-system-path 'run-file)]获取，而不是通过@racket[(current-command-line-arguments)]来获取。

@;{Usually, the best way to handle command-line arguments is to parse
them using the @racket[command-line] form provided by
@racketmodname[racket]. The @racket[command-line] form extracts
command-line arguments from @racket[(current-command-line-arguments)]
by default:}
通常，处理命令行参数的最好方法是通过@racketmodname[racket]提供的@racket[command-line]表解析它们。@racket[command-line]表默认地从@racket[(current-command-line-arguments)]中提取命令行参数：

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket

  (define verbose? (make-parameter #f))

  (define greeting
    (command-line
     #:once-each
     [("-v") "Verbose mode" (verbose? #t)]
     #:args 
     (str) str))

  (printf "~a~a\n"
          greeting
          (if (verbose?) " to you, too!" ""))
}

@;{Try running the above script with the @DFlag{help} flag to see what
command-line arguments are allowed by the script.}
尝试使用@DFlag{help}标志运行上面的脚本来查看脚本允许的命令行参数。

@;{An even more general trampoline uses @exec{/bin/sh} plus some lines
that are comments in one language and expressions in the other. This
trampoline is more complicated, but it provides more control over
command-line arguments to @exec{racket}:}
一个更一般的trampoline使用@exec{/bin/sh}加上一些行，它们在一种语言中注释并在另一种语言中表达。这种trampoline更复杂，但它对@exec{racket}提供了命令行参数上的更多的控制权：

@verbatim[#:indent 2]|{
  #! /bin/sh
  #|
  exec racket -e '(printf "Running...\n")' -u "$0" ${1+"$@"}
  |#
  #lang racket/base
  (printf "The above line of output had been produced via\n")
  (printf "a use of the `-e' flag.\n")
  (printf "Given arguments: ~s\n"
          (current-command-line-arguments))
}|

@;{Note that @litchar{#!} starts a line comment in Racket, and
@litchar{#|}...@litchar{|#} forms a block comment. Meanwhile,
@litchar{#} also starts a shell-script comment, while @exec{exec
racket} aborts the shell script to start @exec{racket}. That way,
the script file turns out to be valid input to both @exec{/bin/sh} and
@exec{racket}.}
注意，@litchar{#!}在Racket中开始一行注释，以及@litchar{#|}...@litchar{|#}形成一块注释。同时，@litchar{#}还启动一个shell脚本注释，而@exec{exec
racket}中止这个shell脚本以启动@exec{racket}。这样，脚本文件证明有效输入到了@exec{/bin/sh}和@exec{racket}。

@;{@section{Windows Batch Files}}
@section[#:tag "Windows_Batch_Files"]{Windows批处理文件}

@;{A similar trick can be used to write Racket code in Windows
@as-index{@tt{.bat}} batch files:}
一个类似的技巧也可以用来在windows的@as-index{@tt{.bat}}批处理文件中编写Racket代码：

@verbatim[#:indent 2]|{
  ; @echo off
  ; Racket.exe "%~f0" %*
  ; exit /b
  #lang racket/base
  "Hello, world!"
  }|

@;{
@;{Original trick from Ben Goetter, who used:}
来自于Ben Goetter的最初的技巧，他使用：

  ; @echo off && REM -*- racket -*-
  ; "%RACKET%" "%~f0" %*
  ; exit /b
  #lang racket
  ...

@;{it might be worth documenting the Emacs "-*-" convention and a way to
set environment variables -- but that would be needed in the unix part
too.}
这可能是值得记录Emacs "-*-"公约和一种设置环境变量的途径——但这也在UNIX部分被需要。
}
