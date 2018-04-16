#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt"
          (only-in scribble/core link-element)
          (for-label racket/enter))

@(define piece-eval (make-base-eval))

@;{@title[#:tag "intro"]{Welcome to Racket}}
@title[#:tag "intro"]{欢迎来到Racket!}

@;{Depending on how you look at it, @bold{Racket} is}
取决于你如何看待它，Racket语言是：

@itemize[

 @item{@;{a @defterm{programming language}---a dialect of Lisp and a
       descendant of Scheme;}
@defterm{一种@defterm{编程语言（programming language）}}——Lisp语言的一种方言和Scheme的一种派生语言；

       @;{@margin-note{See @secref["dialects"] for more information on
       other dialects of Lisp and how they relate to Racket.}}
@margin-note{参见《@secref["dialects"]》以获取更多关于Lisp其它方言的信息以及它们与Racket的关系。}
  }

 @item{@;{a @defterm{family} of programming languages---variants of
       Racket, and more; or}
@defterm{编程语言的一个@defterm{家族（family）}}——Racket的变体，以及更多的；
  }

 @item{@;{a set of @defterm{tools}---for using a family of programming languages.}
@defterm{一系列@defterm{工具（tools）}}——用于编程语言的一个家族。
  }
   
]


@;{Where there is no room for confusion, we use simply @defterm{Racket}.}
在不会有混乱的情况下，我们简单地使用@defterm{Racket}。

@;{Racket's main tools are}
Racket的主要工具是：

@itemize[

 @tool[@exec{racket}]{
   @;{the core compiler, interpreter, and run-time system;}
  核心编译器、解释器和运行时系统；}

 @tool["DrRacket"]{
   @;{the programming environment; and}
  编程环境；}

 @tool[@exec{raco}]{
   @;{a command-line tool for executing @bold{Ra}cket
 @bold{co}mmands that install packages, build libraries, and more.}
用于执行为安装软件包、建立库等等的@bold{Ra}cket@bold{命令}的一个命令行（command-line）工具。}

]

@;{Most likely, you'll want to explore the Racket language using
DrRacket, especially at the beginning. If you prefer, you can also
work with the command-line @exec{racket} interpreter and your favorite
text editor; see also @secref["other-editors"]. The rest of this guide
presents the language mostly independent of your choice of editor.}
最有可能的是，你想使用DrRacket探索Racket语言，尤其是在开始阶段。如果你要更进一步，你还可以使用命令行@exec{racket}解释器和你喜欢的文本编辑器；也可以参见《@secref["other-editors"]》。本指南的其余部分介绍这个语言大多都无关于你的编辑器选择。

@;{If you're using DrRacket, you'll need to choose the proper language,
because DrRacket accommodates many different variants of Racket, as
well as other languages. Assuming that you've never used DrRacket
before, start it up, type the line}
如果你正在使用DrRacket，就将需要选择适当的语言，因为DrRacket可以容纳许多不同Racket变体，以及其它语言。如果你以前从未使用DrRacket，启动它，在DrRacket顶上的文本区域键入这一行：

@racketmod[racket]

@;{in DrRacket's top text area, and then click the @onscreen{Run} button
that's above the text area. DrRacket then understands that you mean to
work in the normal variant of Racket (as opposed to the smaller
@racketmodname[racket/base] or many other possibilities).}
然后点击文本区域上方的@onscreen{Run}（运行）按钮。DrRacket接着就明白你的意思是在Racket的正常变体下工作（相对于较小的@racketmodname[racket/base]或许多其它的可能性来讲）。

@margin-note{@;{@secref["more-hash-lang"] describes some of the other
             possibilities.}
《@secref["more-hash-lang"]》描述一些其它的可能性。
}

@;{If you've used DrRacket before with something other than a program
that starts @hash-lang[], DrRacket will remember the last language
that you used, instead of inferring the language from the @hash-lang[]
line. In that case, use the @menuitem["Language" "Choose Language..."]
menu item.  In the dialog that appears, select the first item, which
tells DrRacket to use the language that is declared in a source
program via @hash-lang[]. Put the @hash-lang[] line above in the top
text area, still.}
如果你之前用除了以@hash-lang[]开始的一个程序之外程序使用过DrRacket，那么DrRacket会记住你使用的这个最后的语言，而不是从@hash-lang[]推断这个语言。在这种情况下，使用@menuitem["Language" "Choose Language..."]（语言|选择语言……）菜单项。在出现的对话框中，选择第一项，它告诉DrRacket使用通过@hash-lang[]在源程序中申明的这个语言。仍然要把@hash-lang[]行放在文本区域的顶部上面。

@; ----------------------------------------------------------------------
@;{@section{Interacting with Racket}}
@section[#:tag "Interacting_with_Racket"]{用Racket进行交互}

@;{DrRacket's bottom text area and the @exec{racket} command-line program
(when started with no options) both act as a kind of calculator. You
type a Racket expression, hit the Return key, and the answer is
printed. In the terminology of Racket, this kind of calculator is
called a @idefterm{read-eval-print loop} or @deftech{REPL}.}
DrRacket的底部文本区和@exec{racket}的命令行程序（当不带选项启动时）都可以扮作一种计算器。你打出一个racket的表达式，按下回车键，答案就打印出来了。在Racket的术语里，这种计算器叫做一个@idefterm{读取求值打印循环}（read-eval-print loop）或@deftech{REPL}。

@;{A number by itself is an expression, and the answer is just the
number:}
一个数字本身就是一个表达式，而答案就是数字：

@interaction[5]

@;{A string is also an expression that evaluates to itself. A string is
written with double quotes at the start and end of the string:}
一个字符串也是一个求值为自身的表达式。一个字符串在字符串的开始和结尾使用双引号来书写：

@interaction["Hello, world!"]

@;{Racket uses parentheses to wrap larger expressions---almost any kind
of expression, other than simple constants. For example, a function
call is written: open parenthesis, function name, argument
expression, and closing parenthesis. The following expression calls
the built-in function @racket[substring] with the arguments
@racket["the boy out of the country"], @racket[4], and @racket[7]:}
Racket使用圆括号包裹较大的表达式——几乎任何一种表达式，而不是简单的常数。例如，一个函数调用被写为：开括号，函数名，参数表达式和闭括号。下面的表达式用参数@racket["the boy out of the country"]、@racket[4]和@racket[7]调用内置函数@racket[substring]：

@interaction[(substring "the boy out of the country" 4 7)]

@; ----------------------------------------------------------------------
@;{@section{Definitions and Interactions}}
@section[#:tag "Definitions_and_Interactions"]{定义和交互}

@;{You can define your own functions that work like @racket[substring] by
using the @racket[define] form, like this:}
你能够通过使用@racket[define]表定义像@racket[substring]那样工作的你自己的函数，像这样：

@def+int[
#:eval piece-eval
(define (extract str)
  (substring str 4 7))
(extract "the boy out of the country")
(extract "the country out of the boy")
]

@;{Although you can evaluate the @racket[define] form in the @tech{REPL},
definitions are normally a part of a program that you want to keep and
use later. So, in DrRacket, you'd normally put the definition in the
top text area---called the @deftech{definitions area}---along with the
@hash-lang[] prefix:}
虽然你可以在@tech{REPL}中求值@racket[define]表，但定义通常是你想去保持并今后使用的一个程序的一部分。所以，在DrRacket中，你通常会把定义放在顶部文本区——被称作@deftech{定义区域（definitions area）}——随着@hash-lang[]前缀一起：

@racketmod[
racket
code:blank
(define (extract str)
  (substring str 4 7))
]

@;????????????????????????????????????????????????????????????????????????
@;{If calling @racket[(extract "the boy")] is part of the main action of
your program, that would go in the @tech{definitions area}, too. But
if it was just an example expression that you were using to explore
@racket[extract], then you'd more likely leave the @tech{definitions
area} as above, click @onscreen{Run}, and then evaluate
@racket[(extract "the boy")] in the @tech{REPL}.}
如果调用@racket[(extract "the boy")]是程序的主要行为的一部分，那么它也将进入@tech{定义区域}。但如果这只是一个例子，你用来测试@racket[extract]，那么你会更容易如上面那样离开定义区域，点击@onscreen{运行（Run）}，然后将在@tech{REPL}中求值@racket[(extract "the boy")]。

@;{When using command-line @exec{racket} instead of DrRacket, you'd save
the above text in a file using your favorite editor. If you save it as
@filepath{extract.rkt}, then after starting @exec{racket} in the same
directory, you'd evaluate the following sequence:}
当使用命令行的@exec{racket}代替DrRacket，你会在一个文件中用你喜欢的编辑器保存上面的文本。如果你将它保存为@filepath{extract.rkt}，然后在同一目录开始@exec{racket}，你会对以下序列求值：

@;{@margin-note{If you use @racketmodname[xrepl], you can use
  @(link-element "plainlink" (litchar ",enter extract.rkt") `(xrepl "enter")).}}
@margin-note{如果你使用@racketmodname[xrepl]，你可以使用@(link-element "plainlink" (litchar ",enter extract.rkt") `(xrepl "enter"))。}

@interaction[
#:eval piece-eval
(eval:alts (enter! "extract.rkt") (void))
(extract "the gal out of the city")
]

@;{The @racket[enter!] form both loads the code and switches the
evaluation context to the inside of the module, just like DrRacket's
@onscreen{Run} button.}
@racket[enter!]表加载代码和开关的求值语境到模块里面，就像DrRacket的@onscreen{运行（Run）}按钮一样。

@; ----------------------------------------------------------------------
@;{@section{Creating Executables}}
@section[#:tag "Creating_Executables"]{创建可执行文件}

@;{If your file (or @tech{definitions area} in DrRacket) contains}
如果你的文件（或在DrRacket的@tech{定义区域}）包含：

@racketmod[
racket

(define (extract str)
  (substring str 4 7))

(extract "the cat out of the bag")
]

@;{then it is a complete program that prints ``cat'' when run. You can
run the program within DrRacket or using @racket[enter!] in
@exec{racket}, but if the program is saved in @nonterm{src-filename},
you can also run it from a command line with}
那么它是一个在运行时打印“cat” 的完整程序。你可以在DrRacket中运行程序或在racket中使用enter!，但如果程序被保存在‹src-filename›中，你也可以从命令行运行

@commandline{racket @nonterm{src-filename}}

@;{To package the program as an executable, you have a few options:}
将程序打包为可执行文件，您有几个选项：

@itemize[

 @;{@item{In DrRacket, you can select the @menuitem["Racket" "Create
       Executable..."] menu item.}}
   @item{在DrRacket，你可以选择@menuitem["Racket" "Create Executable..."]菜单项。}

 @;{@item{From a command-line prompt, run @exec{raco exe
       @nonterm{src-filename}}, where @nonterm{src-filename} contains
       the program. See @secref[#:doc '(lib
       "scribblings/raco/raco.scrbl") "exe"] for more information.}}
   @item{从命令提示符，运行@exec{raco exe @nonterm{src-filename}}，这里nonterm{src-filename}包含程序。（参见《raco exe: Creating Stand-Alone Executables 》部分获取更多信息。）}

 @;{@item{With Unix or Mac OS, you can turn the program file into an
       executable script by inserting the line

       @margin-note{See @secref["scripts"] for more information on
                    script files.}

        @verbatim[#:indent 2]{#! /usr/bin/env racket}

       at the very beginning of the file. Also, change the file
       permissions to executable using @exec{chmod +x
       @nonterm{filename}} on the command line.

       The script works as long as @exec{racket} is in the user's
       executable search path.  Alternately, use a full path to
       @exec{racket} after @tt{#!}  (with a space between @tt{#!}
       and the path), in which case the user's executable search path
       does not matter.}}
   @item{在UNIX或Mac OS中，可以通过在文件的开头插入以下行将程序文件转换为可执行脚本：
@margin-note{参见@secref["scripts"]获取有关脚本文件的更多信息。}

@verbatim[#:indent 2]{#! /usr/bin/env racket }

同时，在命令行中用@exec{chmod +x @nonterm{filename}} 改变文件权限去执行。
 
只要@exec{racket}在用户的可执行搜索路径中脚本就会工作。另外，在@tt{#!}后使用完整路径提交给@exec{racket}（在#!和路径之间有空格），在这种情况下用户的可执行搜索路径无关紧要。}

]

@; ----------------------------------------------------------------------
@;{@section[#:tag "use-module"]{A Note to Readers with Lisp/Scheme Experience}}
@section[#:tag "use-module"]{给有LISP/Scheme经验的读者的一个说明}

@;{If you already know something about Scheme or Lisp, you might be
tempted to put just}
如果你已经知道一些关于Scheme或Lisp的东西，你可能会试图这样将

@racketblock[
(define (extract str)
  (substring str 4 7))
]

@;{into @filepath{extract.rktl} and run @exec{racket} with}
放入@filepath{extract.rkt}并且如下运行@exec{racket}

@interaction[
#:eval piece-eval
(eval:alts (load "extract.rktl") (void))
(extract "the dog out")
]

@;{That will work, because @exec{racket} is willing to imitate a
traditional Lisp environment, but we strongly recommend against using
@racket[load] or writing programs outside of a module.}
这将起作用，因为@exec{racket}会模仿传统的Lisp环境，但我们强烈建议不要在模块之外使用@racket[load]或编写程序。

@;{Writing definitions outside of a module leads to bad error messages,
bad performance, and awkward scripting to combine and run
programs. The problems are not specific to @exec{racket}; they're
fundamental limitations of the traditional top-level environment,
which Scheme and Lisp implementations have historically fought with ad
hoc command-line flags, compiler directives, and build tools. The
module system is designed to avoid these problems, so start with
@hash-lang[], and you'll be happier with Racket in the long run.}
在模块之外编写定义会导致糟糕的错误消息、差的性能和笨拙的脚本来组合和运行程序。这些问题并不是特别针对@exec{racket}，它们是传统顶层环境的根本限制，Scheme和Lisp实现在历史上与临时命令行标志、编译器指令和构建工具进行了斗争。模块系统的设计是为了避免这些问题，所以以@hash-lang[]开始，你会在长期工作中与Racket更愉快。

@; ----------------------------------------------------------------------

@close-eval[piece-eval]
