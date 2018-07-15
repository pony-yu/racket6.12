#lang scribble/doc
@(require scribble/manual 
          scribble/eval 
          "guide-utils.rkt"
          "module-hier.rkt"
          (for-label setup/dirs
                     setup/link
                     racket/date))

@;{@title[#:tag "module-basics"]{Module Basics}}
@title[#:tag "module-basics"]{模块基础}

@;{Each Racket module typically resides in its own file. For example,
suppose the file @filepath{cake.rkt} contains the following module:}
每个Racket模块通常驻留在自己的文件中。例如，假设文件@filepath{cake.rkt}包含以下模块：

@racketmod[
#:file "cake.rkt"
racket

(provide print-cake)

(code:comment @#,t{@;{draws a cake with @racket[n] candles}画一个带@racket[n]支蜡烛的蛋糕。})
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))

(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
]

@;{Then, other modules can import @filepath{cake.rkt} to use the
@racket[print-cake] function, since the @racket[provide] line in
@filepath{cake.rkt} explicitly exports the definition
@racket[print-cake]. The @racket[show] function is private to
@filepath{cake.rkt} (i.e., it cannot be used from other modules),
since @racket[show] is not exported.}
然后，其它模块可以导入@filepath{cake.rkt}以使用@racket[print-cake]函数，因为@filepath{cake.rkt}中的@racket[provide]行明确导出这个定义@racket[print-cake]。@racket[show]函数对@filepath{cake.rkt}是私有的（即它不能从其它模块被使用），因为@racket[show]没有被导出。

@;{The following @filepath{random-cake.rkt} module imports
@filepath{cake.rkt}:}
下面的@filepath{random-cake.rkt}模块导入@filepath{cake.rkt}：

@racketmod[
#:file "random-cake.rkt"
racket

(require "cake.rkt")

(print-cake (random 30))
]

@;{The relative reference @racket["cake.rkt"] in the import
@racket[(require "cake.rkt")] works if the @filepath{cake.rkt} and
@filepath{random-cake.rkt} modules are in the same
directory. Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs in HTML pages.}
如果@filepath{cake.rkt}和@filepath{random-cake.rkt}模块在同一个目录里，在导入@racket[(require "cake.rkt")]中的这个相对引用内的引用@racket["cake.rkt"]就会工作。UNIX样式的相对路径用于所有平台上的相对模块引用，就像HTML页面中的相对的URL一样。

@; ----------------------------------------
@;{@section[#:tag "module-org"]{Organizing Modules}}
@section[#:tag "module-org"]{组织模块}

@;{The @filepath{cake.rkt} and @filepath{random-cake.rkt} example
demonstrates the most common way to organize a program into modules:
put all module files in a single directory (perhaps with
subdirectories), and then have the modules reference each other
through relative paths. A directory of modules can act as a
project, since it can be moved around on the filesystem or copied to
other machines, and relative paths preserve the connections among
modules.}
@filepath{cake.rkt}和@filepath{random-cake.rkt}示例演示如何组织一个程序模块的最常用的方法：把所有的模块文件在一个目录（也许是子目录），然后有模块通过相对路径相互引用。模块的一个目录可以作为一个项目，因为它可以在文件系统上移动或复制到其它机器上，并且相对路径保存模块之间的连接。

@;{As another example, if you are building a candy-sorting program, you
might have a main @filepath{sort.rkt} module that uses other modules
to access a candy database and a control sorting machine. If the
candy-database module itself is organized into sub-modules that handle
barcode and manufacturer information, then the database module could
be @filepath{db/lookup.rkt} that uses helper modules
@filepath{db/barcodes.rkt} and @filepath{db/makers.rkt}.  Similarly,
the sorting-machine driver @filepath{machine/control.rkt} might use
helper modules @filepath{machine/sensors.rkt} and
@filepath{machine/actuators.rkt}.}
作为另一个例子，如果你正在构建一个糖果分类程序，你可能有一个主@filepath{sort.rkt}模块，它使用其它模块访问一个糖果数据库和一个控制分拣机。如果这个糖果数据库模块本身被组织进了处理条码和厂家信息的子模块，那么这个数据库模块可以是@filepath{db/lookup.rkt}，它使用辅助器模块@filepath{db/barcodes.rkt}和@filepath{db/makers.rkt}。同样，这个分拣机驱动器@filepath{machine/control.rkt}可能会使用辅助器模块@filepath{machine/sensors.rkt}和@filepath{machine/actuators.rkt}。

@centerline[module-hierarchy]

@;{The @filepath{sort.rkt} module uses the relative paths
@filepath{db/lookup.rkt} and @filepath{machine/control.rkt} to import
from the database and machine-control libraries:}
@filepath{sort.rkt}模块使用相对路径@filepath{db/lookup.rkt}和@filepath{machine/control.rkt}从数据库和机器控制库导入：

@racketmod[
#:file "sort.rkt"
racket
(require "db/lookup.rkt" "machine/control.rkt")
....]

@;{The @filepath{db/lookup.rkt} module similarly uses paths relative to
its own source to access the @filepath{db/barcodes.rkt} and
@filepath{db/makers.rkt} modules:}
@filepath{db/lookup.rkt}模块类似地使用相对路径给它自己的源来访问@filepath{db/barcodes.rkt}和@filepath{db/makers.rkt}模块：

@racketmod[
#:file "db/lookup.rkt"
racket
(require "barcode.rkt" "makers.rkt")
....]

@;{Ditto for @filepath{machine/control.rkt}:}
同上，@filepath{machine/control.rkt}：

@racketmod[
#:file "machine/control.rkt"
racket
(require "sensors.rkt" "actuators.rkt")
....]

@;{Racket tools all work automatically with relative paths. For example,}
Racket工具所有工作自动使用相对路径。例如，

@commandline{racket sort.rkt}

@;{on the command line runs the @filepath{sort.rkt} program and
automatically loads and compiles required modules. With a large enough
program, compilation from source can take too long, so use}
在命令行运行@filepath{sort.rkt}程序和自动加载并编译所需的模块。对于一个足够大的程序，从源编译可能需要很长时间，所以使用

@commandline{raco make sort.rkt}

@;{@margin-note{See @secref[#:doc '(lib "scribblings/raco/raco.scrbl")
"make"] for more information on @exec{raco make}.}}
@margin-note{参见《raco：Racket命令行工具（raco:Racket Command-Line Tools）》中的“raco make: Compiling Source to Bytecode”部分以获取更多关于@exec{raco make}的信息。}

@;{to compile @filepath{sort.rkt} and all its dependencies to bytecode
files. Running @exec{racket sort.rkt} will automatically use bytecode
files when they are present.}
编译@filepath{sort.rkt}及其所有依赖成为字节码文件。如果字节码文件存在，运行@exec{racket sort.rkt}，将自动使用字节码文件。

@; ----------------------------------------
@;{@section{Library Collections}}
@section[#:tag "Library-Collections"]{库集合}

@;{A @deftech{collection} is a hierarchical grouping of installed library modules.  A
module in a @tech{collection} is referenced through an unquoted,
suffixless path. For example, the following module refers to the
@filepath{date.rkt} library that is part of the @filepath{racket}
@tech{collection}:}
一个@deftech{集合（collection）}是一个已安装的库模块的按等级划分的组。一个@tech{集合}中的一个模块通过一个引号引用，无后缀路径。例如，下面的模块引用@filepath{date.rkt}库，它是@filepath{racket}@tech{集合}的一部分：

@racketmod[
racket

(require racket/date)

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
]

@;{When you search the online Racket documentation, the search results
indicate the module that provides each binding. Alternatively, if you
reach a binding's documentation by clicking on hyperlinks, you can
hover over the binding name to find out which modules provide
it.}
当你搜索在线Racket文档时，搜索结果显示提供每个绑定的模块。或者，如果你通过单击超链接到达一个绑定文档，则可以在绑定名称上悬停以查找哪些模块提供了它。

@;{A module reference like @racketmodname[racket/date] looks like an
identifier, but it is not treated in the same way as @racket[printf]
or @racket[date->string]. Instead, when @racket[require] sees a module
reference that is unquoted, it converts the reference to a
collection-based module path:}
一个模块的引用，像@racketmodname[racket/date]，看起来像一个标识符，但它并不是和@racket[printf]或@racket[date->string]相同的方式对待。相反，当@racket[require]发现一个被引号包括的模块引用，它转化这个引用为一个基于@tech{集合}的模块路径：

@itemlist[

 @item{
  @;{First, if the unquoted path contains no @litchar{/}, then
       @racket[require] automatically adds a @filepath{/main} to the
       reference. For example, @racket[(require
       @#,racketmodname[slideshow])] is equivalent to @racket[(require
       slideshow/main)].}
首先，如果这个引用路径不包含@litchar{/}，那么@racket[require]自动添加一个@filepath{/main}给这个引用。例如，@racket[(require @#,racketmodname[slideshow])]等价于@racket[(require
       slideshow/main)]。
    }

 @item{
  @;{Second, @racket[require] implicitly adds a @filepath{.rkt}
       suffix to the path.}
    其次，@racket[require]隐式添加一个@filepath{.rkt}后缀给这个路径。
    }

 @item{
  @;{Finally, @racket[require] resolves the path by searching among
       installed @tech{collections}, instead of treating the path as relative to
       the enclosing module's path.}
最后，@racket[require]在已安装的@tech{集合}中通过搜索来决定路径，而不是将路径处理为相对于封闭模块的路径。
    }

]

@;{To a first approximation, a @tech{collection} is implemented as a
filesystem directory. For example, the @filepath{racket} collection is
mostly located in a @filepath{racket} directory within the Racket
installation's @filepath{collects} directory, as reported by}
作为一个最近似情况，一个@tech{集合}被实现为一个文件系统目录。例如，@filepath{racket}集合大多位于@filepath{racket}安装的@filepath{collects}目录中的一个@filepath{racket}目录中，如以下报告：

@racketmod[
racket

(require setup/dirs)

(build-path (find-collects-dir) (code:comment @#,t{@;{main collection directory}主集合目录})
            "racket")
]

@;{The Racket installation's @filepath{collects} directory, however, is
only one place that @racket[require] looks for collection directories.
Other places include the user-specific directory reported by
@racket[(find-user-collects-dir)] and directories configured through
the @envvar{PLTCOLLECTS} search path. Finally, and most typically,
collections are found through installed @tech{packages}.}
然而，Racket安装的@filepath{collects}目录仅仅是一个@racket[require]寻找集合目录的地方。其它地方包括用户指定通过@racket[(find-user-collects-dir)]报告的目录以及通过@envvar{PLTCOLLECTS}搜索路径配置的目录。最后，并且最典型，集合通过安装的@tech{包（packages）}找到。

@; ----------------------------------------
@;{@section[#:tag "packages-and-collections"]{Packages and Collections}}
@section[#:tag "packages-and-collections"]{包和集合}

@;{A @deftech{package} is a set of libraries that are installed through
the Racket package manager (or included as pre-installed in a Racket
distribution). For example, the @racketmodname[racket/gui] library is
provided by the @filepath{gui} package, while
@racketmodname[parser-tools/lex] is provided by the
@filepath{parser-tools} library.@margin-note{More precisely,
@racketmodname[racket/gui] is provided by @filepath{gui-lib},
@racketmodname[parser-tools/lex] is provided by
@filepath{parser-tools-lib}, and the @filepath{gui} and
@filepath{parser-tools} packages extend @filepath{gui-lib} and
@filepath{parser-tools-lib} with documentation.}}
一个@deftech{包（package）}是通过Racket包管理器（或者作为一个预安装包括在一个Racket分发中）。例如，@racketmodname[racket/gui]库是由@filepath{gui}包提供的，而@racketmodname[parser-tools/lex]是由@filepath{parser-tools}库提供的。@margin-note{更确切地说，@racketmodname[racket/gui]由 @filepath{gui-lib}提供，@racketmodname[parser-tools/lex]由@filepath{parser-tools-lib}提供，并且@filepath{gui}和@filepath{parser-tools}包用文档扩展@filepath{gui-lib}和@filepath{parser-tools-lib}。}

@;{Racket programs do not refer to @tech{packages} directly. Instead,
programs refer to libraries via @tech{collections}, and adding or
removing a @tech{package} changes the set of collection-based
libraries that are available. A single package can supply
libraries in multiple collections, and two different packages can
supply libraries in the same collection (but not the same libraries,
and the package manager ensures that installed packages do not
conflict at that level).}
Racket程序不直参考@tech{包（packages）}。相反，程序通过@tech{集合（collections）}参考库，添加或删除一个@tech{包}改变可获得的基于集合库的集合。一个单个包可以在多个集合中提供库，并且两个不同的包可以在同一集合（但不是同一个库，并且包管理器确保安装的包在该层级不冲突）中提供库。

@;{For more information about packages, see @other-manual['(lib
"pkg/scribblings/pkg.scrbl")].}
有关包的更多信息，请参阅《Racket中的包管理》（Package Management in Racket）。

@; ----------------------------------------
@;{@section[#:tag "link-collection"]{Adding Collections}}
@section[#:tag "link-collection"]{添加集合}

@;{Looking back at the candy-sorting example of @secref["module-org"],
suppose that modules in @filepath{db/} and @filepath{machine/} need a
common set of helper functions. Helper functions could be put in a
@filepath{utils/} directory, and modules in @filepath{db/} or
@filepath{machine/} could access utility modules with relative paths
that start @filepath{../utils/}. As long as a set of modules work
together in a single project, it's best to stick with relative paths.
A programmer can follow relative-path references without knowing about
your Racket configuration.}
回顾《@secref["module-org"]》部分的糖果排序示例，假设@filepath{db/}和@filepath{machine/}中的那个模块需要一套公共的助手函数。辅助函数可以被放在一个@filepath{utils/}目录里，同时@filepath{db/}或@filepath{machine/}中的模块可以用开始于@filepath{../utils/}的相对路径访问公用模块。只要一组模块在一个单一项目中协同工作，最好保持相对路径。一个程序员可以不用知道你的Racket配置而继承相对路径引用。

@;{Some libraries are meant to be used across multiple projects, so that
keeping the library source in a directory with its uses does not make
sense. In that case, the best option is add a new
@tech{collection}. After the library is in a collection, it can be
referenced with an unquoted path, just like libraries that are
included with the Racket distribution.}
有些库是为了被用于跨多个项目，因此将库的源保存在一个目录内与它的使用没有意义。在这种情况下，最好的选择是添加一个新@tech{集合}。这个库处于一个集合里后，它可以用一个非引用路径引用，就像是包括在Racket发行里的库一样。

@;{You could add a new collection by placing files in the Racket
installation or one of the directories reported by
@racket[(get-collects-search-dirs)]. Alternatively, you could add to
the list of searched directories by setting the @envvar{PLTCOLLECTS}
environment variable.@margin-note*{If you set @envvar{PLTCOLLECTS},
include an empty path in by starting the value with a colon (Unix and
Mac OS) or semicolon (Windows) so that the original search paths are
preserved.} The best option, however, is to add a @tech{package}.}
你可以通过将文件放置在Racket安装包里或通过@racket[(get-collects-search-dirs)]报告的一个目录下添加一个新的集合。或者，你可以通过设置@envvar{PLTCOLLECTS}环境变量添加到搜索目录列表。@margin-note*{如果你设置@envvar{PLTCOLLECTS}，通过用冒号（UNIX和Mac OS）或分号（Windows）启动这个值包括一个空路径，从而保留原始搜索路径。}然而，最好的选择是添加一个@tech{包}。

@;{Creating a package @emph{does not} mean that you have to register with
a package server or perform a bundling step that copies your source
code into an archive format. Creating a package can simply mean using
the package manager to make your libraries locally accessible as a
collection from their current source locations.}
创建一个包@emph{并不}意味着你必须用一个包服务器或者执行一个复制你的源代码到一个归档格式中的绑定步骤注册。创建一个包只简单地意味着使用包管理器将你的库作为一个来自它们当前源位置的的集合的本地访问。

@;{For example, suppose you have a directory @filepath{/usr/molly/bakery}
that contains the @filepath{cake.rkt} module (from the
@seclink["module-basics"]{beginning} of this section) and other
related modules. To make the modules available as a @filepath{bakery}
collection, either}
例如，假设你有一个目录@filepath{/usr/molly/bakery}，它包含@filepath{cake.rkt}模块（来自于本节的@seclink["module-basics"]{开始}部分）和其它相关模块。为了使模块可以作为一个@filepath{bakery}集合获取，或者

@itemlist[

 @item{
  @;{Use the @exec{raco pkg} command-line tool:}
    使用@exec{raco pkg}命令行工具：

        @commandline{raco pkg install --link /usr/molly/bakery}

       @;{where the @DFlag{link} flag is not actually needed when the
       provided path includes a directory separator.}
         当所提供的路径包含一个目录分隔符时，这里@DFlag{link}标记实际上不需要。
 }

 @item{
  @;{Use DrRacket's @onscreen{Package Manager} item from the
       @onscreen{File} menu. In the @onscreen{Do What I Mean} panel,
       click @onscreen{Browse...}, choose the
       @filepath{/usr/molly/bakery} directory, and click
       @onscreen{Install}.}
    从@onscreen{File（文件）}菜单使用DrRacket的@onscreen{Package Manager（包管理器）}项。在@onscreen{Do What I Mean（做我打算的）}面板，点击@onscreen{Browse...（浏览……）}，选择@filepath{/usr/molly/bakery}目录，并且单击@onscreen{Install（安装）}。
    }

]

@;{Afterward, @racket[(require bakery/cake)] from any module will import
the @racket[print-cake] function from
@filepath{/usr/molly/bakery/cake.rkt}.}
之后，从任何模块中@racket[(require bakery/cake)]将从@filepath{/usr/molly/bakery/cake.rkt}输入@racket[print-cake]函数。

@;{By default, the name of the directory that you install is used both as
the @tech{package} name and as the @tech{collection} that is provided
by the package.  Also, the package manager normally defaults to
installation only for the current user, as opposed to all users of a
Racket installation. See @other-manual['(lib
"pkg/scribblings/pkg.scrbl")] for more information.}
默认情况下，你安装的目录的名称既用作@tech{包}名称，又用作包提供的@tech{集合}。同样，包管理器通常默认只为当前用户安装，而不是在一个Racket安装的所有用户。有关更多信息，请参阅《Racket中的包管理（Package Management in Racket）。

@;{If you intend to distribute your libraries to others, choose
collection and package names carefully. The collection namespace is
hierarchical, but top-level collection names are global, and the
package namespace is flat. Consider putting one-off libraries under
some top-level name like @filepath{molly} that identifies the
producer.  Use a collection name like @filepath{bakery} when producing
the definitive collection of baked-goods libraries.}
如果你打算分发你的库给其他人，请仔细选择集合和包名称。集合名称空间是分层的，但顶级集合名是全局的，包名称空间是扁平的。考虑将一次性库放在一些顶级名称下，就像@filepath{molly}这种标识制造器。在制作烘焙食品库的最终集合时，使用像@filepath{bakery}这样的一个集合名。

@;{After your libraries are put in a @tech{collection} you can still
use @exec{raco make} to compile the library sources, but it's better
and more convenient to use @exec{raco setup}. The @exec{raco setup}
command takes a collection name (as opposed to a file name) and
compiles all libraries within the collection. In addition, @exec{raco setup} can
build documentation for the collection and add it to the documentation
index, as specified by a @filepath{info.rkt} module in the collection.
See @secref[#:doc '(lib "scribblings/raco/raco.scrbl") "setup"] for
more information on @exec{raco setup}.}
在你的库被放入一个@tech{集合}之后，你仍然可以使用@exec{raco make}以编译库源，但更好而且更方便的是使用@exec{raco setup}。@exec{raco setup}命令取得一个集合名（而不是一个文件名）并编译集合内所有的库。此外，@exec{raco setup}可以建立文档以收集和添加文档到文档索引，作为通过集合中的一个@filepath{info.rkt}模块做详细说明。有关@exec{raco setup}的详细信息请看《raco设置：安装管理（raco setup: Installation Management）》。