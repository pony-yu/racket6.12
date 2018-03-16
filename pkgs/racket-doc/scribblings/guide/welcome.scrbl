#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt"
          (only-in scribble/core link-element)
          (for-label racket/enter))

@(define piece-eval (make-base-eval))

@;{@title[#:tag "intro"]{Welcome to Racket}}
@title[#:tag "intro"]{Racket语言欢迎你!}

@;{Depending on how you look at it, @bold{Racket} is}
取决于你如何看待它，Racket语言是：

@;{@itemize[

 @item{a @defterm{programming language}---a dialect of Lisp and a
       descendant of Scheme;

       @margin-note{See @secref["dialects"] for more information on
       other dialects of Lisp and how they relate to Racket.}}

 @item{a @defterm{family} of programming languages---variants of
       Racket, and more; or}

 @item{a set of @defterm{tools}---for using a family of programming languages.}
]}
@itemize[
         
 @item{@defterm{一种@defterm{编程语言（programming language）}}——一种Lisp语言的方言，继承于Scheme；
 @margin-note{参见@secref["dialects"]以获取更多关于Lisp其它方言的信息，以及它们与Racket的关系。}}
  
 @item{@defterm{一@defterm{系列（family）}编程语言}——如Racket或者其它等等；}
 
 @item{@defterm{一系列@defterm{工具（tools）}}——用于一系列编程语言的。}
 
 ]

@;{Where there is no room for confusion, we use simply @defterm{Racket}.}
当不会出现混乱的地方，我们就用简单的@defterm{Racket}。

@;{Racket's main tools are}
Racket的主要工具是包括：

@;{@itemize[

 @tool[@exec{racket}]{the core compiler, interpreter, and run-time system;}

 @tool["DrRacket"]{the programming environment; and}

 @tool[@exec{raco}]{a command-line tool for executing @bold{Ra}cket
 @bold{co}mmands that install packages, build libraries, and more.}

]}
@itemize[
         
 @tool[@exec{racket}]{核心编译器、解释器和运行时系统；}
  
 @tool["DrRacket"]{编程环境；}
 
 @tool[@exec{raco}]{一个用于执行@bold{Ra}cket的@bold{co}mmand（命令）以安装软件包、建立库等等的命令行工具。}

 ]

@;{Most likely, you'll want to explore the Racket language using
DrRacket, especially at the beginning. If you prefer, you can also
work with the command-line @exec{racket} interpreter and your favorite
text editor; see also @secref["other-editors"]. The rest of this guide
presents the language mostly independent of your choice of editor.}
最有可能的是，你想使用DrRacket探索Racket语言，尤其是在开始阶段。如果您愿意，您还可以使用命令行@exec{racket}解释器和您喜欢的文本编辑器,也可以参见@secref["other-editors"]部分内容。本指南的其余部分介绍了与语言无关的编辑器的选择。

@;{If you're using DrRacket, you'll need to choose the proper language,
because DrRacket accommodates many different variants of Racket, as
well as other languages. Assuming that you've never used DrRacket
before, start it up, type the line}
如果你使用DrRacket，就需要选择适当的语言，因为DrRacket可以容纳许多不同的变种如Racket，以及其他语言。如果你以前从未使用DrRacket，启动它，在DrRacket顶上的文本区域输入这一行：

@racketmod[racket]

@;{in DrRacket's top text area, and then click the @onscreen{Run} button
that's above the text area. DrRacket then understands that you mean to
work in the normal variant of Racket (as opposed to the smaller
@racketmodname[racket/base] or many other possibilities).}
然后单击@onscreen{运行（Run）}按钮的上方的文本区。DrRacket就明白你的意思在Racket执行正常变体的工作（相对于较小的@racketmodname[racket/base]或许多其他的可能性）。

@margin-note{@secref["more-hash-lang"] describes some of the other
             possibilities.}

@;{If you've used DrRacket before with something other than a program
that starts @hash-lang[], DrRacket will remember the last language
that you used, instead of inferring the language from the @hash-lang[]
line. In that case, use the @menuitem["Language" "Choose Language..."]
menu item.  In the dialog that appears, select the first item, which
tells DrRacket to use the language that is declared in a source
program via @hash-lang[]. Put the @hash-lang[] line above in the top
text area, still.}
如果你使用DrRacket之前已经使用了其它以@hash-lang[]开始，那么DrRacket会记得你上次使用的语言，而不是从@hash-lang[]推断的语言。在这种情况下，使用@menuitem["Language" "Choose Language..."]（语言|选择语言……）菜单项去改变。在出现的对话框中，选择第一项，它告诉DrRacket使用通过@hash-lang[]申明在源程序中的语言。仍然要把把@hash-lang[]放在文本区域的顶部的。

@; ----------------------------------------------------------------------
@;{@section{Interacting with Racket}}
@section{与Racket语言交互}

@;{DrRacket's bottom text area and the @exec{racket} command-line program
(when started with no options) both act as a kind of calculator. You
type a Racket expression, hit the Return key, and the answer is
printed. In the terminology of Racket, this kind of calculator is
called a @idefterm{read-eval-print loop} or @deftech{REPL}.}
DrRacket底部的文本区和@exec{racket}的命令行程序（启动时没有选择）作为一种计算器。你打出一个racket的表达式，按下回车键，答案就打印出来了。在Racket的术语里，这种计算器叫做@idefterm{读取求值打印（read-eval-print）}循环或@deftech{REPL}。

@;{A number by itself is an expression, and the answer is just the
number:}
一个数字本身就是一个表达式，而答案就是数字：

@interaction[5]

@;{A string is also an expression that evaluates to itself. A string is
written with double quotes at the start and end of the string:}
字符串也是一个求值的表达式。字符串在字符串的开始和结尾使用双引号：

@interaction["Hello, world!"]

@;{Racket uses parentheses to wrap larger expressions---almost any kind
of expression, other than simple constants. For example, a function
call is written: open parenthesis, function name, argument
expression, and closing parenthesis. The following expression calls
the built-in function @racket[substring] with the arguments
@racket["the boy out of the country"], @racket[4], and @racket[7]:}
Racket使用圆括号包装较大的表达式——几乎任何一种表达式，而不是简单的常数。例如，函数调用被写入：大括号，函数名，参数表达式，闭括号。下面的表达式用参数调用@racket["the boy out of the country"]、@racket[4]和@racket[7]调用内置函数@racket[substring]：

@interaction[(substring "the boy out of the country" 4 7)]

@; ----------------------------------------------------------------------
@;{@section{Definitions and Interactions}}
@section{定义和交互}
@;?????????????????????????????????????????????????????

You can define your own functions that work like @racket[substring] by
using the @racket[define] form, like this:

@def+int[
#:eval piece-eval
(define (extract str)
  (substring str 4 7))
(extract "the boy out of the country")
(extract "the country out of the boy")
]

Although you can evaluate the @racket[define] form in the @tech{REPL},
definitions are normally a part of a program that you want to keep and
use later. So, in DrRacket, you'd normally put the definition in the
top text area---called the @deftech{definitions area}---along with the
@hash-lang[] prefix:

@racketmod[
racket
code:blank
(define (extract str)
  (substring str 4 7))
]

If calling @racket[(extract "the boy")] is part of the main action of
your program, that would go in the @tech{definitions area}, too. But
if it was just an example expression that you were using to explore
@racket[extract], then you'd more likely leave the @tech{definitions
area} as above, click @onscreen{Run}, and then evaluate
@racket[(extract "the boy")] in the @tech{REPL}.

When using command-line @exec{racket} instead of DrRacket, you'd save
the above text in a file using your favorite editor. If you save it as
@filepath{extract.rkt}, then after starting @exec{racket} in the same
directory, you'd evaluate the following sequence:

@margin-note{If you use @racketmodname[xrepl], you can use
  @(link-element "plainlink" (litchar ",enter extract.rkt") `(xrepl "enter")).}

@interaction[
#:eval piece-eval
(eval:alts (enter! "extract.rkt") (void))
(extract "the gal out of the city")
]

The @racket[enter!] form both loads the code and switches the
evaluation context to the inside of the module, just like DrRacket's
@onscreen{Run} button.

@; ----------------------------------------------------------------------
@section{Creating Executables}

If your file (or @tech{definitions area} in DrRacket) contains

@racketmod[
racket

(define (extract str)
  (substring str 4 7))

(extract "the cat out of the bag")
]

then it is a complete program that prints ``cat'' when run. You can
run the program within DrRacket or using @racket[enter!] in
@exec{racket}, but if the program is saved in @nonterm{src-filename},
you can also run it from a command line with

@commandline{racket @nonterm{src-filename}}

To package the program as an executable, you have a few options:

@itemize[

 @item{In DrRacket, you can select the @menuitem["Racket" "Create
       Executable..."] menu item.}

 @item{From a command-line prompt, run @exec{raco exe
       @nonterm{src-filename}}, where @nonterm{src-filename} contains
       the program. See @secref[#:doc '(lib
       "scribblings/raco/raco.scrbl") "exe"] for more information.}

 @item{With Unix or Mac OS, you can turn the program file into an
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
       does not matter.}

]

@; ----------------------------------------------------------------------
@section[#:tag "use-module"]{A Note to Readers with Lisp/Scheme Experience}

If you already know something about Scheme or Lisp, you might be
tempted to put just

@racketblock[
(define (extract str)
  (substring str 4 7))
]

into @filepath{extract.rktl} and run @exec{racket} with

@interaction[
#:eval piece-eval
(eval:alts (load "extract.rktl") (void))
(extract "the dog out")
]

That will work, because @exec{racket} is willing to imitate a
traditional Lisp environment, but we strongly recommend against using
@racket[load] or writing programs outside of a module.

Writing definitions outside of a module leads to bad error messages,
bad performance, and awkward scripting to combine and run
programs. The problems are not specific to @exec{racket}; they're
fundamental limitations of the traditional top-level environment,
which Scheme and Lisp implementations have historically fought with ad
hoc command-line flags, compiler directives, and build tools. The
module system is designed to avoid these problems, so start with
@hash-lang[], and you'll be happier with Racket in the long run.

@; ----------------------------------------------------------------------

@close-eval[piece-eval]
