#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-paths"]{Module Paths}}
@title[#:tag "module-paths"]{模块的路径}

@;{A @deftech{module path} is a reference to a module, as used with
@racket[require] or as the @racket[_initial-module-path] in a
@racket[module] form. It can be any of several forms:}
@deftech{模块路径（module path）}是对模块的引用，作为@racket[require]的使用，或者作为@racket[module]表中的@racket[_initial-module-path]。它可以是几种形式中的任意一种：

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (quote) (#,(racket quote) id)]{

@;{A @tech{module path} that is a quoted identifier refers to a non-file
@racket[module] declaration using the identifier. This form of module
reference makes the most sense in a @tech{REPL}.}
引用标识符的@tech{模块路径（module path）}指的是使用标识符的非文件@racket[module]声明。这种模块引用形式做多的场景是在@tech{REPL}。

@examples[
(module m racket
  (provide color)
  (define color "blue"))
(module n racket
  (require 'm)
  (printf "my favorite color is ~a\n" color))
(require 'n)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[rel-string]{

@;{A string @tech{module path} is a relative path using Unix-style
conventions: @litchar{/} is the path separator, @litchar{..} refers to
the parent directory, and @litchar{.} refers to the same
directory. The @racket[rel-string] must not start or end with a path
separator. If the path has no suffix, @filepath{.rkt} is added
automatically.}
字符串@tech{模块路径（module path）}是使用UNIX样式约定的相对路径：@litchar{/}是路径分隔符，@litchar{..}指父目录，@litchar{.}指同一目录。@racket[rel-string]不必以路径分隔符开始或结束。如果路径没有后缀，@filepath{.rkt}会自动添加。

@;{The path is relative to the enclosing file, if any, or it is relative
to the current directory. (More precisely, the path is relative to the
value of @racket[(current-load-relative-directory)], which is set
while loading a file.)}
路径是相对于封闭文件，如果有的话，或者是相对于当前目录。（更确切地说，路径是相对于 @racket[(current-load-relative-directory)]的值），这是在加载文件时设置的。

@;{@secref["module-basics"] shows examples using relative paths.}
@secref["module-basics"]使用相对路径显示了示例。

@;{If a relative path ends with a @filepath{.ss} suffix, it is converted
to @filepath{.rkt}. If the file that implements the referenced module
actually ends in @filepath{.ss}, the suffix will be changed back when
attempting to load the file (but a @filepath{.rkt} suffix takes
precedence). This two-way conversion provides compatibility with older
versions of Racket.}
如果一个相对路径以@filepath{.ss}后缀结尾，它会被转换成@filepath{.rkt}。如果实现引用模块的文件实际上以@filepath{.ss}结束，当试图加载文件（但@filepath{.rkt}后缀优先）时后缀将被改回来。这种双向转换提供了与Racket旧版本的兼容。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

@;{A @tech{module path} that is an unquoted identifier refers to an
installed library. The @racket[id] is constrained to contain only
ASCII letters, ASCII numbers, @litchar{+}, @litchar{-}, @litchar{_},
and @litchar{/}, where @litchar{/} separates path elements within the
identifier. The elements refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories.}
一个@tech{模块路径（module path）}是一个引用标识符，引用一个已经安装的库。@racket[id]约束只包含ASCII字母、ASCII数字、@litchar{+}、@litchar{-}、@litchar{_}和@litchar{/}，@litchar{/}分隔标识符内的路径元素。元素指的是@tech{集合（collection）}和@tech{子集合（sub-collection）}，而不是目录和子目录。

@;{An example of this form is @racket[racket/date]. It refers to the
module whose source is the @filepath{date.rkt} file in the
@filepath{racket} collection, which is installed as part of
Racket. The @filepath{.rkt} suffix is added automatically.}
这种形式的一个例子是@racket[racket/date]。它是指模块的源是@filepath{racket}集合中的@filepath{date.rkt}文件，它被安装为Racket的一部分。@filepath{.rkt}后缀被自动添加。

@;{Another example of this form is @racketmodname[racket], which is commonly
used at the initial import. The path @racketmodname[racket] is shorthand for
@racket[racket/main]; when an @racket[id] has no @litchar{/}, then
@racket[/main] is automatically added to the end. Thus,
@racketmodname[racket] or @racket[racket/main] refers to the module whose
source is the @filepath{main.rkt} file in the @filepath{racket}
collection.}
这种形式的另一个例子是@racketmodname[racket]，在初始引入时它通常被使用。路径@racketmodname[racket]是对@racket[racket/main]的简写；当一个@racket[id]没有@litchar{/}，那么@racket[/main]自动被添加到结尾。因此，@racketmodname[racket]或@racket[racket/main]是指其源是@filepath{racket}集合里的@filepath{main.rkt}文件的模块。

@examples[
(module m racket
  (require racket/date)

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require 'm)
]

@;{When the full path of a module ends with @filepath{.rkt}, if no such
file exists but one does exist with the @filepath{.ss} suffix, then
the @filepath{.ss} suffix is substituted automatically. This
transformation provides compatibility with older versions of Racket.}
当一个模块的完整路径以@filepath{.rkt}结束，如果没有这样的文件存在但有一个@filepath{.ss}后缀的文件存在，那么这个@filepath{.ss}后缀是是自动替代的。这种转换提供了与旧版本的Racket的兼容。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (lib)
             (lib rel-string)]{

@;{Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the @racket[rel-string] can end with a file
suffix, in which case @filepath{.rkt} is not automatically added.}
像一个不带引号的标识符的路径，但表示为一个字符串而不是标识符。另外，@racket[rel-string]可以以一个文件的后缀结束，在这种情况下，@filepath{.rkt}不是自动添加的。

@;{Example of this form include @racket[(lib "racket/date.rkt")] and
@racket[(lib "racket/date")], which are equivalent to
@racket[racket/date]. Other examples include @racket[(lib "racket")],
@racket[(lib "racket/main")], and @racket[(lib "racket/main.rkt")],
which are all equivalent to @racketmodname[racket].}
这种形式的例子包括@racket[(lib "racket/date.rkt")]和@racket[(lib "racket/date")]，这是相当于@racket[racket/date]。其它的例子包括@racket[(lib "racket")]、@racket[(lib "racket/main")]和@racket[(lib "racket/main.rkt")]，都相当于@racketmodname[racket]。

@examples[
(module m (lib "racket")
  (require (lib "racket/date.rkt"))

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require 'm)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet id)]{

@;{Accesses a third-party library that is distributed through the
@|PLaneT| server. The library is downloaded the first time that it is
needed, and then the local copy is used afterward.}
访问通过@|PLaneT|服务器分发的第三方库。首先需要下载库，然后使用本地副本。

@;{The @racket[id] encodes several pieces of information separated by a
@litchar{/}: the package owner, then package name with optional
version information, and an optional path to a specific library with
the package. Like @racket[id] as shorthand for a @racket[lib] path, a
@filepath{.rkt} suffix is added automatically, and @racketidfont{/main}
is used as the path if no sub-path element is supplied.}
@racket[id]编码了用@litchar{/}分隔的几条信息：包所有者，然后是可选的版本信息的包名，以及一个特定的库与包的可选路径。像@racket[id]作为一个 @racket[lib]路径的简写，一个@filepath{.rkt}后缀被自动添加，并且当子路径没有提供时@racketidfont{/main}用作路径。

@examples[
(eval:alts
 (module m (lib "racket")
   (code:comment @#,t{Use @filepath{schematics}'s @filepath{random.plt} 1.0, file @filepath{random.rkt}:})
   (require (planet schematics/random:1/random))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]

@;{As with other forms, an implementation file ending with @filepath{.ss}
can be substituted automatically if no implementation file ending with
@filepath{.rkt} exists.}
与其它形式，一个用@filepath{.ss}作为文件结尾的实现可以自动取代如果没有用@filepath{.rkt}执行文件结尾存在。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet package-string)]{

@;{Like the symbol form of a @racket[planet], but using a string instead
of an identifier. Also, the @racket[package-string] can end with a
file suffix, in which case @filepath{.rkt} is not added.}
就像@racket[planet]的符号形式，但使用的是字符串而不是标识符。另外，@racket[package-string]可以一个文件的后缀结束，在这种情况下，@filepath{.rkt}不添加。

@;{As with other forms, an @filepath{.ss} extension is converted to
@filepath{.rkt}, while an implementation file ending with
@filepath{.ss} can be substituted automatically if no implementation
file ending with @filepath{.rkt} exists.}
与其他形式一样，当以@filepath{.ss}文件结尾的实现可以自动取代时，如果没有以@filepath{.rkt}执行文件结尾存在，@filepath{.ss}扩展为@filepath{.rkt}。
 }

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (planet = + -)
                  (planet rel-string (user-string pkg-string vers ...))
                  ([vers nat
                         (nat nat)
                         (= nat)
                         (+ nat)
                         (- nat)])]{

@;{A more general form to access a library from the @|PLaneT| server. In
this general form, a @|PLaneT| reference starts like a @racket[lib]
reference with a relative path, but the path is followed by
information about the producer, package, and version of the
library. The specified package is downloaded and installed on demand.}
从@|PLaneT|服务器访问库的更一般形式。在这种一般形式中，@|PLaneT|引用开始时像一个相对路径的@racket[库（lib）]引用，但路径后面是关于库的生产者、包和版本的信息。指定的包是按需下载和安装的。

@;{The @racket[vers]es specify a constraint on the acceptable version of
the package, where a version number is a sequence of non-negative
integers, and the constraints determine the allowable values for each
element in the sequence. If no constraint is provided for a particular
element, then any version is allowed; in particular, omitting all
@racket[vers]es means that any version is acceptable. Specifying at
least one @racket[vers] is strongly recommended.}
@racket[vers]在包的可接受版本中指定了一个约束，其中版本号是非负整数序列，约束确定序列中每个元素的允许值。如果没有为特定元素提供约束，则允许任何版本；特别是，省略所有@racket[vers]意味着任何版本都可以接受。至少指定一个@racket[vers]用于强烈推荐。

@;{For a version constraint, a plain @racket[nat] is the same as
@racket[(+ nat)], which matches @racket[nat] or higher for the
corresponding element of the version number.  A @racket[(_start-nat
_end-nat)] matches any number in the range @racket[_start-nat] to
@racket[_end-nat], inclusive. A @racket[(= nat)] matches only exactly
@racket[nat]. A @racket[(- nat)] matches @racket[nat] or lower.}
对于版本约束，普通@racket[nat]与@racket[(+ nat)]相同，对应于版本号的相应元素的@racket[nat]或更高的@racket[nat]。@racket[(_start-nat
_end-nat)]匹配范围内的任何@racket[_start-nat]到@racket[_end-nat]，包括，一个@racket[(= nat)]完全匹配@racket[nat]。一个@racket[(- nat)]匹配@racket[nat]或更低。

@examples[
(eval:alts
 (module m (lib "racket")
   (require (planet "random.rkt" ("schematics" "random.plt" 1 0)))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]

@;{The automatic @filepath{.ss} and @filepath{.rkt} conversions apply as
with other forms.}
自动的@filepath{.ss}和@filepath{.rkt}转换作为其它表添加。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (file)
             (file string)]{

@;{Refers to a file, where @racket[string] is a relative or absolute path
using the current platform's conventions. This form is not portable,
and it should @italic{not} be used when a plain, portable
@racket[rel-string] suffices.}
指定一个文件，其@racket[string]是一个使用当前平台的约定的相对或绝对路径。此表单不可移植，并且@italic{不应当（not）}使用一个扁平的、轻便的@racket[rel-string]满足使用。

@;{The automatic @filepath{.ss} and @filepath{.rkt} conversions apply as
with other forms.}
  自动的@filepath{.ss}和@filepath{.rkt}转换作为其它表添加。
 }

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (submod)
                  (@#,elemtag["submod"]{@racket[submod]} base element ...+)
                  ([base module-path
                         "."
                         ".."]
                   [element id
                            ".."])]{

@;{Refers to a submodule of @racket[base]. The sequence of
@racket[element]s within @racket[submod] specify a path of submodule
names to reach the final submodule. }
是指一个@racket[base]子模块。@racket[element]序列在@racket[submod]指定了一个子模块名称的路径以到达最终的子模块之间。

@examples[
  (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George")))
  (require (submod 'zoo monkey-house))
  monkey
]

@;{Using @racket["."] as @racket[base] within @racket[submod] stands for
the enclosing module. Using @racket[".."] as @racket[base] is
equivalent to using @racket["."] followed by an extra
@racket[".."]. When a path of the form @racket[(#,(racket quote) id)]
refers to a submodule, it is equivalent to @racket[(submod "."  id)].}
使用@racket["."]作为@racket[base]在@racket[submod]代表的外围模块之间。使用@racket[".."]作为@racket[base]相当于使用@racket["."]后跟一个额外的@racket[".."]。当一个路径的表@racket[(#,(racket quote) id)]是指一个子模块，它相当于@racket[(submod "."  id)]。

@;{Using @racket[".."] as an @racket[element] cancels one submodule step, effectively
referring to the enclosing module. For example, @racket[(submod "..")]
refers to the enclosing module of the submodule in which the path
appears.}
使用@racket[".."]作为一种@racket[element]取消一个子模块的步骤，有效指定外围模块。例如，@racket[(submod "..")]是指封闭的子模块的模块，路径出现在其中。

@examples[
  (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George"))
    (module crocodile-house racket
      (require (submod ".." monkey-house))
      (provide dinner)
      (define dinner monkey)))
  (require (submod 'zoo crocodile-house))
  dinner
]}
