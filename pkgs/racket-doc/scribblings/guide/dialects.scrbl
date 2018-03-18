#lang scribble/base
@(require scribble/manual
          "guide-utils.rkt")

@;{@title[#:tag "dialects" #:style 'toc]{Dialects of Racket and Scheme}}
@title[#:tag "dialects" #:style 'toc]{Racket和Scheme的方言}

@;{We use ``Racket'' to refer to a specific dialect of the Lisp language,
and one that is based on the Scheme branch of the Lisp family.
Despite Racket's similarity to Scheme, the @hash-lang[] prefix on
modules is a particular feature of Racket, and programs that start
with @hash-lang[] are unlikely to run in other implementations of
Scheme. At the same time, programs that do not start with @hash-lang[]
do not work with the default mode of most Racket tools.}
我们使用“Racket”来指Lisp语言的特定方言，以及以Lisp家族的Scheme分支为基础的方言。尽管Racket与Scheme相似，对模块的@hash-lang[]前缀是Racket的一个特定功能，程序用@hash-lang[]开始是不可能运行在其它Scheme实现中。同时，如不用@hash-lang[]开始则大多数Racket工具默认模式不会工作。

@;{``Racket'' is not, however, the only dialect of Lisp that is supported
by Racket tools. On the contrary, Racket tools are designed to support
multiple dialects of Lisp and even multiple languages, which allows
the Racket tool suite to serve multiple communities. Racket also gives
programmers and researchers the tools they need to explore and create
new languages.}
然而，“Racket”并不是由Racket工具支持的Lisp的唯一方言。相反，Racket工具的设计目的是支持多种Lisp语言，甚至是多种语言，这使得Racket工具套件可以为多个社区服务。Racket也给程序员和研究人员以开发和创建新语言所需的工具。

@local-table-of-contents[]

@; --------------------------------------------------

@;{@section[#:tag "more-hash-lang"]{More Rackets}}
@section[#:tag "more-hash-lang"]{更多的Racket}

@;{``Racket'' is more of an idea about programming languages than a
language in the usual sense. Macros can extend a base language (as
described in @secref["macros"]), and alternate parsers can
construct an entirely new language from the ground up (as described in
@secref["languages"]).}
“Racket”和一个通常意义上的语言相比更多的是编程语言的概念。宏可以扩展一个基础语言（就像@secref["macros"]中所描述的那样），交互的解析器可以从头构建一个全新的语言（就像@secref["languages"]描述的那样）。

@;{The @hash-lang[] line that starts a Racket module declares the
base language of the module. By ``Racket,'' we usually mean
@hash-lang[] followed by the base language @racketmodname[racket] or
@racketmodname[racket/base] (of which @racketmodname[racket] is an
extension). The Racket distribution provides additional languages,
including the following:}
开始一个Racket模块的@hash-lang[]行声明模块的基本语言。通过“Racket,”我们通常指@hash-lang[]被基础语言@racketmodname[racket]或@racketmodname[racket/base]（其中@racketmodname[racket]是一个扩展）跟着。Racket分配提供了额外的语言，包括以下内容：

@itemize[

 @item{
  @;{@racketmodname[typed/racket] --- like
       @racketmodname[racket], but statically typed; see
       @other-manual['(lib "typed-racket/scribblings/ts-guide.scrbl")]}
@racketmodname[typed/racket]——像@racketmodname[racket]一样，但属于静态类型；参见@other-manual['(lib "typed-racket/scribblings/ts-guide.scrbl")]。
 }

 @item{
  @;{@racketmodname[lazy #:indirect] --- like
       @racketmodname[racket/base], but avoids evaluating an
       expression until its value is needed; see @seclink["top" #:doc
       '(lib "lazy/lazy.scrbl") #:indirect? #t]{the Lazy Racket
       documentation}.}
@racketmodname[lazy #:indirect]——像@racketmodname[racket/base]一样，但避免对表达式求值，直到它的值是必需的；参见@seclink["top" #:doc
       '(lib "lazy/lazy.scrbl") #:indirect? #t]{惰性（Lazy）Racket文档}。
}

 @item{
  @;{@racketmodname[frtime #:indirect] --- changes evaluation in an
       even more radical way to support reactive programming; see
       @seclink["top" #:doc '(lib "frtime/scribblings/frtime.scrbl")
       #:indirect? #t]{the FrTime documentation}.}
@racketmodname[frtime #:indirect]——以更激进的方式进行求值以支持活性程序；参见@seclink["top" #:doc '(lib "frtime/scribblings/frtime.scrbl")
       #:indirect? #t]{FrTime文档}。 
    }
       
 @item{
  @;{@racketmodname[scribble/base] --- a language, which looks more
       like Latex than Racket, for writing documentation; see
       @other-manual['(lib "scribblings/scribble/scribble.scrbl")]}
@racketmodname[scribble/base]——一种语言，它看起来与其说是Racket不如说更像Latex，用于书写文档；参见@other-manual['(lib "scribblings/scribble/scribble.scrbl")]。
 }

]

@;{Each of these languages is used by starting module with the language
name after @hash-lang[]. For example, this source of this
document starts with @racket[@#,hash-lang[] scribble/base].}
这些每一个语言都是通过用@hash-lang[]之后的语言名称开始模块而被使用，例如，这个文件的源码用@racket[@#,hash-lang[] scribble/base]开始。

@;{Furthermore, Racket users can define their own languages, as discussed
in @secref["languages"]. Typically, a language name maps to its
implementation through a module path by adding
@racketidfont{/lang/reader}; for example, the language name
@racketmodname[scribble/base] is expanded to
@racket[scribble/base/lang/reader], which is the module that
implements the surface-syntax parser. Some language names act as
language loaders; for example, @racket[@#,hash-lang[]
@#,racketmodname[planet] _planet-path] downloads, installs, and uses a
language via @seclink["top" #:doc '(lib
"planet/planet.scrbl")]{@|PLaneT|}.}
此外，Racket用户可以定义自己的语言，像在@secref["languages"]里论述的。通常，一种语言的名称通过添加@racketidfont{/lang/reader}透过一个模块路径映射到它的实现；例如，语言名称@racketmodname[scribble/base]被扩展到@racket[scribble/base/lang/reader]，这是实现表面语法分析器的模块。一些语言名称充当语言加载器；例如，@racket[@#,hash-lang[]
@#,racketmodname[planet] _planet-path]通过@seclink["top" #:doc '(lib
"planet/planet.scrbl")]{@|PLaneT|}下载、安装和使用一个语言。

@; --------------------------------------------------

@;{@section[#:tag "standards"]{Standards}}
@section[#:tag "standards"]{标准}

@;{Standard dialects of Scheme include the ones defined by @|r5rs| and
@|r6rs|.}
Scheme的标准方言包括那些被r5rs和r6rs定义的。

@;@subsection[#:tag "r5rs"]{@|r5rs|}}
@subsection[#:tag "r5rs"]{@|r5rs|}

@;{``@|r5rs|'' stands for @link["../r5rs/r5rs-std/index.html"]{The
Revised@superscript{5} Report on the Algorithmic Language Scheme}, and
it is currently the most widely implemented Scheme standard.}
“r5rs”代表@link["../r5rs/r5rs-std/index.html"]{The
Revised@superscript{5} Report on the Algorithmic Language Scheme}，它目前是最广泛被实现的Scheme标准。

@;{Racket tools in their default modes do not conform to @|r5rs|,
mainly because Racket tools generally expect modules, and @|r5rs|
does not define a module system. Typical single-file @|r5rs| programs
can be converted to Racket programs by prefixing them with
@racket[@#,hash-lang[] @#,racketmodname[r5rs]], but other Scheme
systems do not recognize @racket[@#,hash-lang[]
@#,racketmodname[r5rs]]. The @exec{plt-r5rs} executable (see
@secref[#:doc '(lib "r5rs/r5rs.scrbl") "plt-r5rs"]) more directly
conforms to the @|r5rs| standard.}
在默认模式的Racket工具不符合@|r5rs|，主要因为Racket工具通常期待模块，同时@|r5rs|并不定义一个模块系统。典型的单文件@|r5rs|程序可以通过用@racket[@#,hash-lang[] @#,racketmodname[r5rs]]对其进行前缀而转换为Racket程序，但其它的Scheme系统不识别@racket[@#,hash-lang[]
@#,racketmodname[r5rs]]。@exec{plt-r5rs}可执行文件（见@secref[#:doc '(lib "r5rs/r5rs.scrbl") "plt-r5rs"]）更直接地符合r5rs标准。

@;{Aside from the module system, the syntactic forms and functions of
@|r5rs| and Racket differ. Only simple @|r5rs| become Racket
programs when prefixed with @racket[@#,hash-lang[] racket], and
relatively few Racket programs become @|r5rs| programs when a
@hash-lang[] line is removed. Also, when mixing ``@|r5rs| modules''
with Racket modules, beware that @|r5rs| pairs correspond to
Racket mutable pairs (as constructed with @racket[mcons]).}
除了模块系统，对@|r5rs|和Racket的句法表和函数有所不同。当以@racket[@#,hash-lang[] racket]进行前缀时，只有简单的@|r5rs|成为Racket程序，并且当一个@hash-lang[]行被删除时，相对较少的Racket程序成为@|r5rs|程序。另外，当用Racket模块混合”@|r5rs|模块”时，注意@|r5rs|配对相当于Racket可变配对（就像用@racket[mcons]构造一样）。

@;{See @other-manual['(lib "r5rs/r5rs.scrbl")] for more
information about running @|r5rs| programs with Racket.}
参见@other-manual['(lib "r5rs/r5rs.scrbl")]以获取关于用Racket运行@|r5rs|程序的更多信息。

@;{@subsection{@|r6rs|}}
@subsection[#:tag "r6rs"]{@|r6rs|}

@;{``@|r6rs|'' stands for @link["../r6rs/r6rs-std/index.html"]{The
Revised@superscript{6} Report on the Algorithmic Language Scheme},
which extends @|r5rs| with a module system that is similar to the
Racket module system.}
“@|r6rs|”代表@link["../r6rs/r6rs-std/index.html"]{The
Revised@superscript{6} Report on the Algorithmic Language Scheme}，它用一个模块系统扩展了r5rs，类似于Racket的模块系统。

@;{When an @|r6rs| library or top-level program is prefixed with
@racketmetafont{#!}@racketmodname[r6rs] (which is valid @|r6rs|
syntax), then it can also be used as a Racket program. This works
because @racketmetafont{#!} in Racket is treated as a shorthand
for @hash-lang[] followed by a space, so
@racketmetafont{#!}@racketmodname[r6rs] selects the
@racketmodname[r6rs] module language. As with @|r5rs|, however, beware
that the syntactic forms and functions of @|r6rs| differ from
Racket, and @|r6rs| pairs are mutable pairs.}
当一个@|r6rs|库或顶层程序用@racketmetafont{#!}@racketmodname[r6rs]进行前缀（这是有效的@|r6rs|语法），那它也可以用作一个Racket程序。这是因为在Racket中的@racketmetafont{#!}被作为跟着一个空格的@hash-lang[]简写对待，所以@racketmetafont{#!}@racketmodname[r6rs]选择@racketmodname[r6rs]模块语言。但是注意，与@|r5rs|相比， @|r6rs|的句法表和函数与Racket不同，并且@|r6rs|配对是可变配对。

@;{See @other-manual['(lib "r6rs/scribblings/r6rs.scrbl")] for more
information about running @|r6rs| programs with Racket.}
参见@other-manual['(lib "r6rs/scribblings/r6rs.scrbl")]以获得更多关于用Racket运行@|r6rs|程序的信息。

@; --------------------------------------------------

@;{@section[#:tag "teaching-langs"]{Teaching}}
@section[#:tag "teaching-langs"]{教学}

@;{The @|HtDP| textbook relies on pedagogic variants of Racket that
smooth the introduction of programming concepts for new programmers.
See @other-doc['(lib "scribblings/htdp-langs/htdp-langs.scrbl")
#:indirect @list{@|HtDP| language}].}
@|HtDP|教科书依赖于Racket的教学变体，从而为新程序员顺利地引入编程概念。请参见@other-doc['(lib "scribblings/htdp-langs/htdp-langs.scrbl")
#:indirect @list{@|HtDP|语言}]。

@;{The @|HtDP| languages are typically not used with @hash-lang[]
prefixes, but are instead used within DrRacket by selecting the
language from the @onscreen{Choose Language...} dialog.}
@|HtDP|语言通常不使用带@hash-lang[]前缀的方式，而是在DrRacket内通过从@onscreen{Choose Language...}对话框选择语言来使用。