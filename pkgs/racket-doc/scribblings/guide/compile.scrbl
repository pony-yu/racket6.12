#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@;{@title[#:tag "compile"]{Compilation and Configuration: @exec{raco}}}
@title[#:tag "compile"]{同时编译和配置：@exec{raco}}

@;{The @exec{raco} (short for ``@bold{Ra}cket @bold{co}mmand'') program
provides a command-line interface to many additional tools for
compiling Racket programs and maintaining a Racket installation.}
@exec{raco}（以下简称“@bold{Ra}cket @bold{co}mmand”）程序为了编译Racket程序和维护一个Racket安装提供了一个命令行界面给许多额外的工具。

@itemize[

 @item{
  @;{@exec{raco make} compiles Racket source to bytecode.}
    @exec{raco make}将Racket源文件编译成字节码。

 @;{For example, if you have a program @filepath{take-over-world.rkt} and
 you'd like to compile it to bytecode, along with all of its
 dependencies, so that it loads more quickly, then run}
例如，如果你有一个程序@filepath{take-over-world.rkt}并且你想把它编译成字节码，连同其所有的依赖，使其加载速度更快，然后运行

   @commandline{raco make take-over-the-world.rkt}

@;{The bytecode file is written as @filepath{take-over-the-world_rkt.zo}
 in a @filepath{compiled} subdirectory; @index[".zo"]{@filepath{.zo}}
 is the file suffix for a bytecode file.}
  字节码文件在一个@filepath{compiled}子文件夹中被写为@filepath{take-over-the-world_rkt.zo}“；@index[".zo"]{@filepath{.zo}}是一个字节码文件的文件后缀。
 }


 @item{
  @;{@exec{raco setup} manages a Racket installation, including
 manually installed packages.}
    @exec{raco setup}管理一个Racket安装，包括手动安装包。

 @;{For example, if you create your own library @techlink{collection}
 called @filepath{take-over}, and you'd like to build all bytecode and
 documentation for the collection, then run}
例如，如果你创建了自己的名为@filepath{take-over}的库@techlink{集合（collection）}，并且希望为集合构建所有字节码和文档，则运行

   @commandline{raco setup take-over}}


 @item{
  @;{@exec{raco pkg} manages @tech{package}s that can be installed
 through the Racket package manager.}
    @exec{raco pkg}管理@tech{package}，它可以通过Racket包管理器被安装。

 @;{For example, to see the list of installed packages run:}
   例如，要查看已安装包的列表,运行：

    @commandline{raco pkg show}

 @;{To install a new package named @tt{<package-name>} run:}
   安装一个名为@tt{<package-name>}的新包，运行：

    @commandline{raco pkg install <package-name>}

 @;{See @other-doc['(lib "pkg/scribblings/pkg.scrbl")] for more details
 about package management.}
   参见@other-doc['(lib "pkg/scribblings/pkg.scrbl")]以获得关于包管理器的更多细节。
 }
]

@;{For more information on @exec{raco}, see @other-manual['(lib
"scribblings/raco/raco.scrbl")].}
为了获得有关@exec{raco}的更多细节，见@other-manual['(lib
"scribblings/raco/raco.scrbl")]。
