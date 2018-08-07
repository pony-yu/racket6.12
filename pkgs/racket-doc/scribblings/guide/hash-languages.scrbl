#lang scribble/doc
@(require scribble/manual scribble/eval scribble/racket
          "guide-utils.rkt" "modfile.rkt"
          (for-syntax racket/base)
          (for-label setup/dirs
                     syntax/strip-context
                     syntax-color/default-lexer))

@(define-syntax ! (make-element-id-transformer (lambda (v) #'@tt{|})))
@(define-syntax !- (make-element-id-transformer (lambda (v) #'@tt{|-})))


@;{@title[#:tag "hash-languages" #:style 'toc]{Defining new @hash-lang[] Languages}}
@title[#:tag "hash-languages" #:style 'toc]{定义新的@hash-lang[]语言}

@;{When loading a module as a source program that starts}
将模块加载为启动的源程序时

@racketmod[
@#,racket[_language]
]

@;{the @racket[_language] determines the way that the rest of the module
is parsed at the @tech{reader} level. The @tech{reader}-level parse
must produce a @racket[module] form as a @tech{syntax object}. As
always, the second sub-form after @racket[module] specifies the
@tech{module language} that controls the meaning of the module's body
forms. Thus, a @racket[_language] specified after @hash-lang[]
controls both the @tech{reader}-level and @tech{expander}-level
parsing of a module.}
该@racket[_language]决定了模块的其余部分在@tech{读取器（reader）}级别上被解析的方式。@tech{读取器（reader）}级别解析必须生成一个@racket[module]（模块）表作为一个@tech{语法对象（syntax object）}。与以往一样，@racket[module]（模块）后面的第二个子表指定了@tech{模块语言（module language）}来控制模块主体表的含义。因此，一种@racket[_language]（语言）被指定在@hash-lang[]控制@tech{读取器（reader）}级和@tech{扩展器（expander）}级的一个模块的解析之后。

@local-table-of-contents[]

@; ----------------------------------------
@;{@section[#:tag "hash-lang syntax"]{Designating a @hash-lang[] Language}}
@section[#:tag "hash-lang syntax"]{指定一个@hash-lang[]语言}

@;{The syntax of a @racket[_language] intentionally overlaps with the
syntax of a module path as used in @racket[require] or as a
@tech{module language}, so that names like @racketmodname[racket],
@racketmodname[racket/base], @racketmodname[slideshow], or
@racketmodname[scribble/manual] can be used both as @hash-lang[]
languages and as module paths.}
一种@racket[_language]的语法故意与一个模块路径的语法重叠，以作为用于@racket[require]或作为一个@tech{模块语言（module language）}，这样的名字像@racketmodname[racket]、@racketmodname[racket/base]、@racketmodname[slideshow]或@racketmodname[scribble/manual]可以用于作为@hash-lang[]语言和模块路径。

@;{At the same time, the syntax of @racket[_language] is far more
restricted than a module path, because only @litchar{a}-@litchar{z},
@litchar{A}-@litchar{Z}, @litchar{0}-@litchar{9},
@litchar{/} (not at the start or end),
@litchar{_}, @litchar{-}, and @litchar{+} are allowed in a
@racket[_language] name. These restrictions keep the syntax of
@hash-lang[] as simple as possible. Keeping the syntax of @hash-lang[]
simple, in turn, is important because the syntax is inherently
inflexible and non-extensible; the @hash-lang[] protocol allows a
@racket[_language] to refine and define syntax in a practically
unconstrained way, but the @hash-lang[] protocol itself must remain
fixed so that various different tools can ``boot'' into the extended
world.}
同时，@racket[_language]的语法的限制比模块路径远远多，因为只@litchar{a}-@litchar{z}、@litchar{A}-@litchar{Z}、@litchar{0}-@litchar{9}、@litchar{/}（不在开始或结束）、@litchar{_}、@litchar{-}和@litchar{+}在一个@racket[_language]（语言）名称中被允许。这些限制使@hash-lang[]语法尽量简单。保持@hash-lang[]简单，相应地，语法又很重要，因为语法是固有不变和非可扩展的；@hash-lang[]协议允许语言精炼，在几乎无约束的方式定义的语法，但@hash-lang[]协议本身必须保持固定，使各种不同的工具可以“引导（boot）”到扩展的世界。

@;{Fortunately, the @hash-lang[] protocol provides a natural way to refer
to languages in ways other than the rigid @racket[_language] syntax:
by defining a @racket[_language] that implements its own nested
protocol. We have already seen one example (in @secref["s-exp"]): the
@racketmodname[s-exp] @racket[_language] allows a programmer to
specify a @tech{module language} using the general @tech{module path}
syntax. Meanwhile, @racketmodname[s-exp] takes care of the
@tech{reader}-level responsibilities of a @hash-lang[] language.}
幸运的是，这@hash-lang[]协议提供了一个参考其它比刚性语言语法方面语言的自然方式：通过定义一种@racket[_language]，实现自己的嵌套协议。我们已经看到一个例子（在@secref["s-exp"]里）：该@racketmodname[s-exp]的@racket[_language]允许程序员使用通用@tech{模块路径（module path）}语法指定一个@tech{模块语言（module language）}。同时，@racketmodname[s-exp]照顾了一个@hash-lang[]语言@tech{读取器（reader）}级的职责。

@;{Unlike @racketmodname[racket], @racketmodname[s-exp] cannot be used as a
module path with @racket[require]. Although the syntax of
@racket[_language] for @hash-lang[] overlaps with the syntax of module
paths, a @racket[_language] is not used directly as a module
path. Instead, a @racket[_language] obtains a module path by trying two 
locations:  first, it looks for a @racketidfont{reader} submodule of the 
main module for  @racket[_language]. If this is not a valid module path, 
then @racket[_language]  is suffixed with @racketidfont{/lang/reader}. 
(If neither is a valid module path, an error is raised.) The resulting 
module supplies @racketidfont{read} and @racketidfont{read-syntax}
functions using a protocol that is similar to the one for
@racketmetafont{#reader}.}
不同于@racketmodname[racket]，@racketmodname[s-exp]不能作为一个带@racket[require]的模块路径。虽然@hash-lang[]为重叠模块路径语法的@racket[_language]的语法，一个@racket[_language]是不能直接作为一个模块的路径。相反，一个@racket[_language]在两个地点获得模块路径：第一，它寻找一个@racket[_language]的@racketidfont{读取器（reader）}的主模块的子模块。如果这不是一个有效的模块路径，那么@racket[_language]用@racketidfont{/lang/reader}添加后缀。（如果不是一个有效的模块路径，引发错误。）作为结果的模块提供@racketidfont{read}和@racketidfont{read-syntax}函数使用一个协议，它类似于@racketmetafont{#reader}的一个。

@;{@guideother{@secref["hash-reader"] introduces @racketmetafont{#reader}.}}
@guideother{《@secref["hash-reader"]》介绍了@racketmetafont{#reader}.}

@;{A consequence of the way that a @hash-lang[] @racket[_language] is
turned into a module path is that the language must be installed in a
@tech{collection}, similar to the way that @filepath{racket} or
@filepath{slideshow} are collections that are distributed with Racket.
Again, however, there's an escape from this restriction: the
@racketmodname[reader] language lets you specify a @tech{reader}-level
implementation of a language using a general @tech{module path}.}
一个@hash-lang[]的@racket[_language]变成了一个模块路径的方式的结果是，语言必须被安装在一个@tech{集合（collection）}里，类似于@filepath{racket}或@filepath{slideshow}的方式是用Racket分发的集合。然而，还有一种方法可以避免这种限制：@racketmodname[reader]（读取器）语言允许使用通用@tech{模块路径（module path）}指定语言的@tech{读取器（reader）}级实现。

@; ----------------------------------------
@;{@section[#:tag "hash-lang reader"]{Using @racket[@#,hash-lang[] @#,racketmodname[reader]]}}
@section[#:tag "hash-lang reader"]{使用@racket[@#,hash-lang[] @#,racketmodname[reader]]}

@;{The @racketmodname[reader] language for @hash-lang[] is similar to
@racketmodname[s-exp], in that it acts as a kind of meta-language.
Whereas @racketmodname[s-exp] lets a programmer specify a @tech{module
language} at the @tech{expander} layer of parsing,
@racketmodname[reader] lets a programmer specify a language at the
@tech{reader} level.}
对于@hash-lang[]的@racketmodname[reader]语言类似于@racketmodname[s-exp]，在它里边扮演一种元语言。而@racketmodname[s-exp]让一个程序员在解析的@tech{扩展器（expander）}层指定一个@tech{模块语言（module
language）}，@racketmodname[reader]可以让一个程序员在@tech{读取器（reader）}层指定一个语言。

@;{A @racket[@#,hash-lang[] @#,racketmodname[reader]] must be followed by
a module path, and the specified module must provide two functions:
@racketidfont{read} and @racketidfont{read-syntax}. The protocol is
the same as for a @racketmetafont{#reader} implementation, but for
@hash-lang[], the @racketidfont{read} and @racketidfont{read-syntax}
functions must produce a @racket[module] form that is based on the
rest of the input file for the module.}
一个@racket[@#,hash-lang[] @#,racketmodname[reader]]必须跟着一个模块路径，并指定模块必须提供两个函数：@racketidfont{read}和@racketidfont{read-syntax}。该协议是一个和@racketmetafont{#reader}实现相同的，但对于@hash-lang[]，@racketidfont{read}和@racketidfont{read-syntax}函数必须产生一个@racket[module]表，它基于对模块的输入文件的其余部分。

@;{The following @filepath{literal.rkt} module implements a language that
treats its entire body as literal text and exports the text as a
@racketidfont{data} string:}
下面的@filepath{literal.rkt}模块实现了一种语言，它把整个主体作为字面文本并且导出文本作为@racketidfont{数据（data）}字符串：

@racketmodfile["literal.rkt"]

@;{The @filepath{literal.rkt} language uses @racket[strip-context] on the
generated @racket[module] expression, because a
@racketidfont{read-syntax} function should return a syntax object with
no lexical context. Also, the @filepath{literal.rkt} language creates
a module named @racketidfont{anything}, which is an arbitrary choice;
the language is intended to be used in a file, and the longhand module
name is ignored when it appears in a @racket[require]d file.}
@filepath{literal.rkt}语言在生成@racket[module]表达式上使用@racket[strip-context]，因为一个@racketidfont{read-syntax}函数应该返回一个没有词汇语境的语法对象。同时，@filepath{literal.rkt}语言创建一个模块命名@racketidfont{任何东西（anything）}，这是一种随意的选择；语言的目的是要在文件中使用，并在它出现在一个被@racket[require]的文件里时，普通的模块名被忽略。

@;{The @filepath{literal.rkt} language can be used in a module
@filepath{tuvalu.rkt}:}
@filepath{literal.rkt}语言可以用在一个模块@filepath{tuvalu.rkt}中：

@racketmodfile["tuvalu.rkt"]

@;{Importing @filepath{tuvalu.rkt} binds @racketidfont{data} to a
string version of the module content:}
导入@filepath{tuvalu.rkt}绑定@racketidfont{数据（data）}给一个模块内容的字符串版本：

@interaction[
(require "tuvalu.rkt")
data
]

@; ----------------------------------------
@;{@section[#:tag "syntax/module-reader"]{Using @racket[@#,hash-lang[] @#,racketmodname[s-exp] @#,racketmodname[syntax/module-reader]]}}
@section[#:tag "syntax/module-reader"]{使用@racket[@#,hash-lang[] @#,racketmodname[s-exp] @#,racketmodname[syntax/module-reader]]}

@;{Parsing a module body is usually not as trivial as in
@filepath{literal.rkt}. A more typical module parser must iterate to
parse multiple forms for a module body. A language is also more likely
to extend Racket syntax---perhaps through a @tech{readtable}---instead
of replacing Racket syntax completely.}
在@filepath{literal.rkt}里解析模块主体通常不是无足轻重的。一个更典型的模块解析器必须迭代解析模块主体的多个表。一个语言也更可能扩展Racket语法——可能通过@tech{读取表格（readtable）}——而不是彻底更换Racket语法。

@;{The @racketmodname[syntax/module-reader] @tech{module language}
abstracts over common parts of a language implementation to simplify
the creation of new languages. In its most basic form, a language
implemented with @racketmodname[syntax/module-reader] simply specifies
the @tech{module language} to be used for the language, in which case
the @tech{reader} layer of the language is the same as Racket. For
example, with}
@racketmodname[syntax/module-reader]@tech{模块语言（module language）}抽象语言实现的公共部分，以简化新语言的创建。在其最基础的表里，用@racketmodname[syntax/module-reader]实现的语言简单地指定用于该语言的@tech{模块语言（module language）}，在这种情况下，语言的@tech{读取器（reader）}层与Racket相同。例如，

@racketmod[
#:file "raquet-mlang.rkt"
racket
(provide (except-out (all-from-out racket) lambda)
         (rename-out [lambda function]))
]

@;{and}
以及

@racketmod[
#:file "raquet.rkt"
s-exp syntax/module-reader
"raquet-mlang.rkt"
]

@;{then}
那么

@racketmod[
reader "raquet.rkt"
(define identity (function (x) x))
(provide identity)
]

@;{implements and exports the @racket[identity] function, since
@filepath{raquet-mlang.rkt} exports @racket[lambda] as
@racket[function].}
实现和导出@racket[identity]函数，因为@filepath{raquet-mlang.rkt}导出@racket[lambda]函数。

@;{The @racketmodname[syntax/module-reader] language accepts many optional
specifications to adjust other features of the language. For example,
an alternate @racketidfont{read} and @racketidfont{read-syntax} for
parsing the language can be specified with @racket[#:read] and
@racket[#:read-syntax], respectively. The following
@filepath{dollar-racket.rkt} language uses @filepath{dollar.rkt} (see
@secref["readtable"]) to build a language that is like
@racketmodname[racket] but with a @litchar{$} escape to simple infix
arithmetic:}
@racketmodname[syntax/module-reader]语言接受许多可选规范来调整语言的其它特性。例如，另一个解析语言的@racketidfont{read}和@racketidfont{read-syntax}可以用@racket[#:read]和@racket[#:read-syntax]分别指定。下面的@filepath{dollar-racket.rkt}语言运用@filepath{dollar.rkt}（见@secref["readtable"]）建立一个像@racketmodname[racket]但用一个@litchar{$}避开简单中缀算术的语言：

@racketmodfile["dollar-racket.rkt"]

@;{The @racket[require] form appears at the end of the module,
because all of the keyword-tagged optional specifications for
@racketmodname[syntax/module-reader] must appear before any helper
imports or definitions.}
@racket[require]表出现在模块的结尾，因为所有@racketmodname[syntax/module-reader]的关键字标记可选规范必须出现在任何助手导入或定义之前。

@;{The following module uses @filepath{dollar-racket.rkt} to implement a
@racket[cost] function using a @litchar{$} escape:}
以下模块采用@filepath{dollar-racket.rkt}使用一个@litchar{$}实现一个@racket[cost]函数逃逸：

@racketmodfile["store.rkt"]

@; ----------------------------------------
@;{@section[#:tag "language-collection"]{Installing a Language}}
@section[#:tag "language-collection"]{安装一门语言}

@;{So far, we have used the @racketmodname[reader] meta-language to
access languages like @filepath{literal.rkt} and
@filepath{dollar-racket.rkt}. If you want to use something like
@racket[@#,hash-lang[] literal] directly, then you must move
@filepath{literal.rkt} into a Racket @tech{collection} named
@filepath{literal} (see also @secref["link-collection"]).
Specifically, move @filepath{literal.rkt} to a @racketidfont{reader} 
submodule of @filepath{literal/main.rkt} for any directory name
@filepath{literal}, like so:}
到目前为止，我们已经使用了@racketmodname[reader]的元语言来访问语言如@filepath{literal.rkt}和@filepath{dollar-racket.rkt}。如果你想直接使用像@racket[@#,hash-lang[] literal]这样的，然后你必须把@filepath{literal.rkt}移入一个命名为@filepath{literal}的Racket@tech{集合（collection）}（参见@secref["link-collection"]）。具体而言，将@filepath{literal.rkt}移入@filepath{literal/main.rkt}的一个@racketidfont{读取器（reader）}子模块给任何目录名@filepath{literal}，像这样：

@racketmodfile["literal-main.rkt" "literal/main.rkt"]

@;{Then, install the @filepath{literal}
directory as a package:}
然后，将@filepath{literal}目录安装为一个包：

@commandline{cd /path/to/literal ; raco pkg install}

@;{After moving the file and installing the package, you can use
@racket[literal] directly after @hash-lang[]:}
移动文件并安装包后，可以在@hash-lang[]后面直接使用@racket[literal]。

@racketmod[
@#,racket[literal]
Technology!
System!
Perfect!
]

@;{@margin-note{See @other-manual['(lib "scribblings/raco/raco.scrbl")]
for more information on using @exec{raco}.}}
@margin-note{参见@other-manual['(lib "scribblings/raco/raco.scrbl")]以在使用@exec{宏（raco）}时得到更多信息。}

@;{You can also make your language available for others to install by
using the Racket package manager (see @other-doc['(lib
"pkg/scribblings/pkg.scrbl")]). After you create a @filepath{literal}
package and register it with the Racket package catalog (see
@secref["concept:catalog" #:doc '(lib "pkg/scribblings/pkg.scrbl")]),
others can install it using @exec{raco pkg}:}
你也可以让你的语言通过使用Racket安装包管理器提供给他人去安装（参见@other-doc['(lib
"pkg/scribblings/pkg.scrbl")]）。你创造了一个@filepath{literal}包并用Racket包目录登记后（见@secref["concept:catalog" #:doc '(lib "pkg/scribblings/pkg.scrbl")]），其他人就可以使用@exec{raco pkg}安装它：

@commandline{raco pkg install literal}

@;{Once installed, others can invoke the language the same way: by using
@racket[@#,hash-lang[] literal] at the top of a source file.}
一旦安装，其他人可以用同样的方式调用该语言：通过在一个源文件的顶部使用@racket[@#,hash-lang[] literal]。

@;{If you use a public source repository (e.g., GitHub), you can link
your package to the source. As you improve the package, others can
update their version using @exec{raco pkg}:}
如果你使用一个公共的源库（例如，GitHub），你可以将你的包链接到这个源。当你升级这个包，其他人可以使用@exec{raco pkg}更新他们的版本：

@commandline{raco pkg update literal}

@;{@margin-note{See @other-doc['(lib "pkg/scribblings/pkg.scrbl")] for more
information about the Racket package manager.}}
@margin-note{参见@other-doc['(lib "pkg/scribblings/pkg.scrbl")]以了解有关Racket包管理器的更多信息。}

@; ----------------------------------------
@section[#:tag "language-get-info"]{源处理配置}

@;{The Racket distribution includes a Scribble language for writing prose
documents, where Scribble extends the normal Racket to better support
text. Here is an example Scribble document:}
Racket分发包括一个用于编写单调的文档的Scribble语言，这里Scribble扩展了通常的Racket以更好地支持文本。这里有一个Scribble文档例子：

@verbatim[#:indent 2]|{
#lang scribble/base

@(define (get-name) "Self-Describing Document")

@title[(get-name)]

The title of this document is ``@(get-name).''
}|

@;{If you put that program in DrRacket's @tech{definitions area} and
click @onscreen{Run}, then nothing much appears to happen. The
@racketmodname[scribble/base] language just binds and exports
@racketidfont{doc} as a description of a document, similar to the way
that @filepath{literal.rkt} exports a string as @racketidfont{data}.}
如果你把程序放入DrRacket的@tech{定义区域（definitions area）}并点击@onscreen{Run（运行）}，那么不会有更多的呈现会发生。@racketmodname[scribble/base]语言只是绑定和导出@racketidfont{doc（文档）}作为一个文档的一种描述，类似于@filepath{literal.rkt}导出一个字符串作为@racketidfont{data（数据）}。

@;{Simply opening a module with the language
@racketmodname[scribble/base] in DrRacket, however, causes a
@onscreen{Scribble HTML} button to appear. Furthermore, DrRacket knows
how to colorize Scribble syntax by coloring green those parts of the
document that correspond to literal text. The language name
@racketmodname[scribble/base] is not hard-wired into
DrRacket. Instead, the implementation of the
@racketmodname[scribble/base] language provides button and
syntax-coloring information in response to a query from DrRacket.}
然而，在DrRacket里简单地打开一个带@racketmodname[scribble/base]的模块，会引起一个@onscreen{Scribble HTML}按钮出现。此外，DrRacket知道如何通过着色绿色所对应的文本文档的部分着色Scribble的语法。语言名称@racketmodname[scribble/base]不是硬连接到DrRacket里的。相反，@racketmodname[scribble/base]语言的实现提供按钮和语法着色信息响应来自DrRacket查询。

@;{If you have installed the @racket[literal] language as described in
@secref["language-collection"], then you can adjust
@filepath{literal/main.rkt} so that DrRacket treats the content
of a module in the @racket[literal] language as plain text instead of
(erroneously) as Racket syntax:}
如果你已经安装了@racket[literal]（文字）语言作为@secref["language-collection"]（语言集合）中的描述，那你可以调整@filepath{literal/main.rkt}使DrRacket把@racket[literal]（文字）语言里一个模块的内容作为纯文本对待，而不是（错误地）作为Racket的语法：

@racketmodfile["literal-main-get-info.rkt" "literal/main.rkt"]

@;{This revised @racket[literal] implementation provides a
@racketidfont{get-info} function. The @racketidfont{get-info} function
is called by @racket[read-language] (which DrRacket calls) with the 
source input stream and location information,
in case query results should depend on the content of the module after
the language name (which is not the case for @racket[literal]). The
result of @racketidfont{get-info} is a function of two arguments. The
first argument is always a symbol, indicating the kind of information
that a tool requests from the language; the second argument is the
default result to be returned if the language does not recognize the
query or has no information for it.}
修改后的@racket[literal]（文字）实现提供了一个@racketidfont{get-info}函数。这个@racketidfont{get-info}函数称为@racket[read-language]（那是DrRacket称谓）与源输入流和位置信息，如果查询结果应该取决于语言名称后的这个模块的内容（这是没有@racket[literal]（文字）的情况下）。@racketidfont{get-info}的结果是一个带两个参数的函数。第一个参数总是符号，指示那种一个从语言请求的工具的信息类型；第二个参数是如果语言不识别查询或没有信息的话将返回的默认结果。

@;{After DrRacket obtains the result of @racketidfont{get-info} for a
language, it calls the function with a @racket['color-lexer] query;
the result should be a function that implements syntax-coloring
parsing on an input stream. For @racket[literal], the
@racketmodname[syntax-color/default-lexer] module provides a
@racket[default-lexer] syntax-coloring parser that is suitable for
plain text, so @racket[literal] loads and returns that parser in
response to a @racket['color-lexer] query.}
在DrRacket为一个语言获得@racketidfont{get-info}的结果后，它用一个@racket['color-lexer]请求调用函数；结果应该是一个在输入流上实现语法着色解析的函数。对于@racket[literal]@racketmodname[syntax-color/default-lexer]模块提供了一个适用于纯文本的@racket[default-lexer]语法着色解析器，所以@racket[literal]加载和返回在对一个@racket['color-lexer]请求的响应里的解析器。

@;{The set of symbols that a programming tool uses for queries
is entirely between the tool and the languages that choose to
cooperate with it. For example, in addition to @racket['color-lexer],
DrRacket uses a @racket['drracket:toolbar-buttons] query to determine
which buttons should be available in the toolbar to operate on modules
using the language.}
编程工具用于查询的符号集完全位于选择与之合作的工具和语言之间。例如，除了@racket['color-lexer]之外，DrRacket采用一种@racket['drracket:toolbar-buttons]查询以确定哪个按钮在工具栏上应该可用于在使用这个语言的模块上操作。

@;{The @racketmodname[syntax/module-reader] language lets you specify
@racketidfont{get-info} handling through a @racket[#:info] optional
specification. The protocol for an @racket[#:info] function is
slightly different from the raw @racketidfont{get-info} protocol; the
revised protocol allows @racketmodname[syntax/module-reader] the
possibility of handling future language-information queries
automatically.}
@racketmodname[syntax/module-reader]语言可以让你指定@racketidfont{get-info}通过一个@racket[#:info]可选说明处理。一个@racket[#:info]函数协议与原生的@racketidfont{get-info}协议略有不同；修订后的协议允许@racketmodname[syntax/module-reader]自动处理未来语言信息查询的可能性。

@; ----------------------------------------
@;{@section[#:tag "module-runtime-config"]{Module-Handling Configuration}}
@section[#:tag "module-runtime-config"]{模块处理配置}

@;{Suppose that the file @filepath{death-list-5.rkt} contains}
假设文件@filepath{death-list-5.rkt}包含

@racketmodfile["death-list-5.rkt"]

@;{If you @racket[require] @filepath{death-list-5.rkt} directly, then it
prints the list in the usual Racket result format:}
如果你直接@racket[require] @filepath{death-list-5.rkt}，那么它用通常的Racket结果格式打印列表：

@interaction[
(require "death-list-5.rkt")
]

@;{However, if @filepath{death-list-5.rkt} is required by a
@filepath{kiddo.rkt} that is implemented with @racketmodname[scheme]
instead of @racketmodname[racket]:}
但是，如果@filepath{death-list-5.rkt}是被一个@filepath{kiddo.rkt}所需求，而不是用@racketmodname[scheme]代替@racketmodname[racket]实现：

@racketmodfile["kiddo.rkt"]

@;{then, if you run @filepath{kiddo.rkt} file in DrRacket or if you run it
directly with @exec{racket}, @filepath{kiddo.rkt} causes
@filepath{death-list-5.rkt} to print its list in traditional Scheme
format, without the leading quote:}
然后，如果你在DrRacket中运行@filepath{kiddo.rkt}文件或如果你用@exec{racket}直接运行它，@filepath{kiddo.rkt}导致@filepath{death-list-5.rkt}用传统Scheme的格式打印列表，不带前列的引用：

@racketblock[
@#,racketoutput{("O-Ren Ishii" "Vernita Green" "Budd" "Elle Driver" "Bill")}
]

@;{The @filepath{kiddo.rkt} example illustrates how the format for
printing a result value can depend on the main module of a program
instead of the language that is used to implement it.}
这个@filepath{kiddo.rkt}的例子说明了打印一个结果值的格式如何取决于一个程序的主模块，而不是用来实现它的语言。

@;{More broadly, certain features of a language are only invoked when
a module written in that language is run directly with @exec{racket}
(as opposed to being imported into another module). One example is
result-printing style (as shown above). Another example is REPL
behavior. These features are part of what's called the
@deftech{run-time configuration} of a language.}
更广泛地说，只有当用该语言编写的模块直接用@exec{racket}运行（而不是导入另一个模块）时，才会调用某些语言的某些特性。一个例子是结果打印样式（如上所示）。另一个例子是REPL行为。这些特性是所谓的语言@deftech{运行时配置（run-time configuration）}的一部分。

@;{Unlike the syntax-coloring property of a language (as described in
@secref["language-get-info"]), the run-time configuration is a
property of a @emph{module} per se as opposed to a property of
the @emph{source text} representing the module.
For that reason, the run-time configuration for a
module needs to be available even if the module is compiled
to bytecode form and the source is unavailable. Therefore,
run-time configuration cannot be handled by the
@racketidfont{get-info} function we're exporting from the language's
parser module.}
与一个语言的语法着色属性不同（如@secref["language-get-info"]中所描述的），运行时配置本身是一个@emph{模块（module）}的一个属性，而不是表示模块的@emph{源文本(source text)}的属性。出于这个原因，即使模块编译成字节码形式且源不可用，模块的运行时配置也需要可用。因此，运行时配置不能通过我们从语言解析器模块导出的@racketidfont{get-info}函数来处理。

@;{Instead, it will be handled by a new
@racket[configure-runtime] submodule that we'll add inside
the parsed @racket[module] form. When a module is run directly
with @exec{racket}, @exec{racket} looks for a
@racket[configure-runtime] submodule. If it exists, @exec{racket}
runs it. But if the module is imported into another module,
the @racket['configure-runtime] submodule is ignored. (And if the
@racket[configure-runtime] submodule doesn't exist, @exec{racket}
just evaluates the module as usual.) That means that the
@racket[configure-runtime] submodule can be used for any special
setup tasks that need to happen when the module is run directly.}
相反，它将由一个新的@racket[configure-runtime](配置运行时)子模块处理，我们会在里面添加解析@racket[module]（模块）表。当一个模块直接用@exec{racket}运行，@exec{racket}查找@racket[configure-runtime]（配置运行时）子模块。如果它存在，@exec{racket}就运行它。但如果这个模块被导入到另一个模块，这个 @racket['configure-runtime]（配置运行时）子模块被忽略。（如果@racket['configure-runtime]（配置运行时）子模块不存在，@exec{racket}只是像通常一样对模块求值。）那意味着@racket['configure-runtime]（配置运行时）子模块可用于任何特殊设置任务，它在模块直接运行时需要出现。

@;{Going back to the @racket[literal] language (see
@secref["language-get-info"]), we can adjust the language so that
directly running a @racket[literal] module causes it to print out its
string, while using a @racket[literal] module in a larger program
simply provides @racketidfont{data} without printing. To make this
work, we will need an extra module. (For clarity here, we will implement 
this module as a separate file. But it could equally well be 
a submodule of an existing file.)}
回到@racket[literal]（字面）语言（参见@secref["language-get-info"]），我们可以调整语言，这样直接运行一个@racket[literal]（文字）模块造成它打印出它的字符串，而在一个较大的程序中使用一个@racket[literal]（文字）模块只提供@racketidfont{数据（data）}而不打印。为了使这项工作，我们将需要一个额外的模块。（为了清楚起见，我们将把这个模块作为一个单独的文件来实现。但它同样可以是一个已存在的文件的一个模块。）

@racketblock[
.... @#,elem{(the main installation or the user's space)}
!- @#,filepath{literal}
   !- @#,filepath{main.rkt}            @#,elem{(with reader submodule)}
   !- @#,filepath{show.rkt}            @#,elem{(new)}
]

@itemlist[

 @;{@item{The @filepath{literal/show.rkt} module will provide a
       @racketidfont{show} function to be applied to the string
       content of a @racket[literal] module, and also provide a
       @racketidfont{show-enabled} parameter that controls whether
       @racketidfont{show} actually prints the result.}}
  @item{@filepath{literal/show.rkt}模块将提供一个@racketidfont{show}函数以被应用到一个@racket[literal]（文本）模块的字符串内容，同时也提供一个@racketidfont{show-enabled}参数，它控制是否@racketidfont{show}的实际打印结果。}

 @;{@item{The new @racket[configure-runtime] submodule in
       @filepath{literal/main.rkt} will set the
       @racketidfont{show-enabled} parameter to @racket[#t]. The
       net effect is that @racketidfont{show} will print the strings
       that it's given, but only when a module using the @racket[literal]
       language is run directly (because only then will the
       @racket[configure-runtime] submodule be invoked).}}
@item{在@filepath{literal/main.rkt}中的新的@racket[configure-runtime]（配置运行时）子模块将设置@racketidfont{show-enabled}参数为@racket[#t]。净效果是@racketidfont{show}将打印给定的字符串，但只有当一个模块使用@racket[literal]（文字）语言直接运行时（因为只有决定@racket[configure-runtime]（配置运行时）子模块被调用）。}

]


@;{These changes are implemented in the following revised
@filepath{literal/main.rkt}:}
这些变化在以下修订过的@filepath{literal/main.rkt}中被实现：

@racketmodfile["literal-main-language-info.rkt" "literal/main.rkt"]

@;{Then the @filepath{literal/show.rkt} module must provide
the @racketidfont{show-enabled} parameter and @racketidfont{show}
function:}
那么@filepath{literal/show.rkt}模块必须提供@racketidfont{show-enabled}参数和@racketidfont{show}函数：

@racketmod[
#:file "literal/show.rkt"
racket

(provide show show-enabled)

(define show-enabled (make-parameter #f))

(define (show v)
  (when (show-enabled)
    (display v)))
]

@;{With all of the pieces for @racket[literal] in place, try running the
following variant of @filepath{tuvalu.rkt} directly and through a
@racket[require] from another module:}
在恰当的位置用所有为@racket[literal]编写的片段，试图运行以下@filepath{tuvalu.rkt}的变体，直接地和通过一个来自另一个模块的@racket[require]：

@racketmod[
#:file "tuvalu.rkt"
@#,racket[literal]
Technology!
System!
Perfect!
]

@;{When run directly, we'll see the result printed like so, because
our @racket[configure-runtime] submodule will have set the
@racketidfont{show-enabled} parameter to @racket[#t]:}
当直接运行时，我们会看到结果打印像这样，因为我们的@racket[configure-runtime](配置运行时)子模块将设置@racketidfont{show-enabled}参数为@racket[#t]：

@racketblock[
@#,racketoutput{Technology!
@(linebreak)System!
@(linebreak)Perfect!}
]

@;{But when imported into another module, printing will be suppressed,
because the @racket[configure-runtime] submodule will not be invoked,
and therefore the @racketidfont{show-enabled} parameter will remain
at its default value of @racket[#f].}
但当导入到另一个模块时，打印将被抑制，因为@racket[configure-runtime]（配置运行时）子模块将不会被调用，因此@racketidfont{show-enabled}参数将保持其默认值@racket[#f]。