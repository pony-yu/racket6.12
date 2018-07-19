#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-paths"]{Module Paths}}
@title[#:tag "module-paths"]{模块路径}

@;{A @deftech{module path} is a reference to a module, as used with
@racket[require] or as the @racket[_initial-module-path] in a
@racket[module] form. It can be any of several forms:}
一个@deftech{模块路径（module path）}是对一个模块的一个引用，作为@racket[require]的使用，或者作为一个@racket[module]表中的@racket[_initial-module-path]。它可以是几种表中的任意一种：

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (quote) (#,(racket quote) id)]{

@;{A @tech{module path} that is a quoted identifier refers to a non-file
@racket[module] declaration using the identifier. This form of module
reference makes the most sense in a @tech{REPL}.}
一个引用标识符的一个@tech{模块路径（module path）}指的是使用这个标识符的一个非文件@racket[module]声明。模块引用的这种表在一个@tech{REPL}中具有更多意义。

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
一个字符串@tech{模块路径}是使用UNIX样式约定的一个相对路径：@litchar{/}是路径分隔符，@litchar{..}指父目录，@litchar{.}指同一目录。@racket[rel-string]不必以一个路径分隔符开始或结束。如果路径没有后缀，@filepath{.rkt}会自动添加。

@;{The path is relative to the enclosing file, if any, or it is relative
to the current directory. (More precisely, the path is relative to the
value of @racket[(current-load-relative-directory)], which is set
while loading a file.)}
这个路径是相对于封闭文件，如果有的话，或者是相对于当前目录。（更确切地说，路径是相对于 @racket[(current-load-relative-directory)]的值），这是在加载一个文件时设置的。

@;{@secref["module-basics"] shows examples using relative paths.}
《@secref["module-basics"]》使用相对路径显示示例。

@;{If a relative path ends with a @filepath{.ss} suffix, it is converted
to @filepath{.rkt}. If the file that implements the referenced module
actually ends in @filepath{.ss}, the suffix will be changed back when
attempting to load the file (but a @filepath{.rkt} suffix takes
precedence). This two-way conversion provides compatibility with older
versions of Racket.}
如果一个相对路径以一个@filepath{.ss}后缀结尾，它会被转换成@filepath{.rkt}。如果实现引用模块的文件实际上以@filepath{.ss}结束，当试图加载这个文件（但一个@filepath{.rkt}后缀优先）时后缀将被改回来。这种双向转换提供了与Racket旧版本的兼容。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

@;{A @tech{module path} that is an unquoted identifier refers to an
installed library. The @racket[id] is constrained to contain only
ASCII letters, ASCII numbers, @litchar{+}, @litchar{-}, @litchar{_},
and @litchar{/}, where @litchar{/} separates path elements within the
identifier. The elements refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories.}
作为一个引用标识符的一个@tech{模块路径}引用一个已经安装的库。这个@racket[id]约束只包含ASCII字母、ASCII数字、@litchar{+}、@litchar{-}、@litchar{_}和@litchar{/}，这里@litchar{/}分隔标识符内的路径元素。这个元素指的是@tech{集合（collection）}和@tech{子集合（sub-collection）}，而不是目录和子目录。

@;{An example of this form is @racket[racket/date]. It refers to the
module whose source is the @filepath{date.rkt} file in the
@filepath{racket} collection, which is installed as part of
Racket. The @filepath{.rkt} suffix is added automatically.}
这种表的一个例子是@racket[racket/date]。它是指模块的源是@filepath{racket}集合中的@filepath{date.rkt}文件，它被安装为Racket的一部分。@filepath{.rkt}后缀被自动添加。

@;{Another example of this form is @racketmodname[racket], which is commonly
used at the initial import. The path @racketmodname[racket] is shorthand for
@racket[racket/main]; when an @racket[id] has no @litchar{/}, then
@racket[/main] is automatically added to the end. Thus,
@racketmodname[racket] or @racket[racket/main] refers to the module whose
source is the @filepath{main.rkt} file in the @filepath{racket}
collection.}
这种表的另一个例子是@racketmodname[racket]，它通常被使用在初始输入时。这个路径@racketmodname[racket]是对@racket[racket/main]的简写；当一个@racket[id]没有@litchar{/}，那么@racket[/main]自动被添加到结尾。因此，@racketmodname[racket]或@racket[racket/main]指的是其源是@filepath{racket}集合里的@filepath{main.rkt}文件的模块。

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
当一个模块的完整路径以@filepath{.rkt}结束时，如果没有这样的文件存在但有一个@filepath{.ss}后缀的文件存在，那么这个@filepath{.ss}后缀自动被替代。这种转换提供了与Racket旧版本的兼容。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (lib)
             (lib rel-string)]{

@;{Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the @racket[rel-string] can end with a file
suffix, in which case @filepath{.rkt} is not automatically added.}
像一个非引号标识符路径，但表示为一个字符串而不是一个标识符。另外，@racket[rel-string]可以以一个文件后缀结束，在这种情况下，@filepath{.rkt}不被自动添加。

@;{Example of this form include @racket[(lib "racket/date.rkt")] and
@racket[(lib "racket/date")], which are equivalent to
@racket[racket/date]. Other examples include @racket[(lib "racket")],
@racket[(lib "racket/main")], and @racket[(lib "racket/main.rkt")],
which are all equivalent to @racketmodname[racket].}
这种表的例子包括@racket[(lib "racket/date.rkt")]和@racket[(lib "racket/date")]，这等效于@racket[racket/date]。其它的例子包括@racket[(lib "racket")]、@racket[(lib "racket/main")]和@racket[(lib "racket/main.rkt")]，都等效于@racketmodname[racket]。

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
通过@|PLaneT|服务器访问一个被分发的第三方库。这个库在其被需要的第一时间被下载，然后使用这个本地副本。

@;{The @racket[id] encodes several pieces of information separated by a
@litchar{/}: the package owner, then package name with optional
version information, and an optional path to a specific library with
the package. Like @racket[id] as shorthand for a @racket[lib] path, a
@filepath{.rkt} suffix is added automatically, and @racketidfont{/main}
is used as the path if no sub-path element is supplied.}
这个@racket[id]编码了被一个@litchar{/}分隔的几条信息：包所有者，然后是带可选版本信息的包名称，以及对一个带包的特定库的一个可选路径。就像@racket[id]作为一个 @racket[lib]路径、一个被自动添加的@filepath{.rkt}后缀以及在没有子路径提供时被用作路径的@racketidfont{/main}的简写。

@examples[
(eval:alts
 (module m (lib "racket")
   (code:comment @#,t{@;{Use @filepath{schematics}'s @filepath{random.plt} 1.0, file @filepath{random.rkt}:}使用@filepath{schematics}的@filepath{random.plt} 1.0, 文件@filepath{random.rkt}:})
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
如同其它表，如果没有以@filepath{.rkt}结尾的执行文件存在，一个以@filepath{.ss}结尾的实现文件可以自动被取代。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet package-string)]{

@;{Like the symbol form of a @racket[planet], but using a string instead
of an identifier. Also, the @racket[package-string] can end with a
file suffix, in which case @filepath{.rkt} is not added.}
就像一个@racket[planet]的符号表，但使用一个字符串而不是一个标识符。同样，@racket[package-string]可以以一个文件后缀结束，在这种情况下，@filepath{.rkt}不被添加。

@;{As with other forms, an @filepath{.ss} extension is converted to
@filepath{.rkt}, while an implementation file ending with
@filepath{.ss} can be substituted automatically if no implementation
file ending with @filepath{.rkt} exists.}
与其它表一样，在如果没有以@filepath{.rkt}结束的的执行文件存在而一个以@filepath{.ss}结束的实现文件可以自动被替代时，一个@filepath{.ss}扩展名转换为@filepath{.rkt}。
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
一般更通用的表去访问来自于@|PLaneT|服务器的一个库。在这种通用表中，一个@|PLaneT|引用像一个有一个相对路径的@racket[lib]引用那样开始，但这个路径后面跟随关于库的制造者、包和版本的信息。指定的包被按需下载和安装。

@;{The @racket[vers]es specify a constraint on the acceptable version of
the package, where a version number is a sequence of non-negative
integers, and the constraints determine the allowable values for each
element in the sequence. If no constraint is provided for a particular
element, then any version is allowed; in particular, omitting all
@racket[vers]es means that any version is acceptable. Specifying at
least one @racket[vers] is strongly recommended.}
这个@racket[vers]在这个包的可接受版本上指定一个约束，这里一个版本号是一个非负整数序列，并且这个约束确定序列中的每个元素的允许值。如果没有为一个特定元素提供约束，则任何版本被允许；特别是，省略所有@racket[vers]意味着任何版本都被接受。强烈推荐至少指定一个@racket[vers]。

@;{For a version constraint, a plain @racket[nat] is the same as
@racket[(+ nat)], which matches @racket[nat] or higher for the
corresponding element of the version number.  A @racket[(_start-nat
_end-nat)] matches any number in the range @racket[_start-nat] to
@racket[_end-nat], inclusive. A @racket[(= nat)] matches only exactly
@racket[nat]. A @racket[(- nat)] matches @racket[nat] or lower.}
对于一个版本约束，一个单纯的@racket[nat]与@racket[(+ nat)]相同，其匹配@racket[nat]或高于这个版本号的相应元素。一个@racket[(_start-nat
_end-nat)]匹配包括在@racket[_start-nat]到@racket[_end-nat]范围内的任何数值。一个@racket[(= nat)]恰好匹配@racket[nat]。一个@racket[(- nat)]匹配@racket[nat]或更低的。

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
自动的@filepath{.ss}和@filepath{.rkt}转换应用为其它表。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (file)
             (file string)]{

@;{Refers to a file, where @racket[string] is a relative or absolute path
using the current platform's conventions. This form is not portable,
and it should @italic{not} be used when a plain, portable
@racket[rel-string] suffices.}
指定一个文件，其@racket[string]是一个使用当前平台的约定的相对或绝对路径。这个表不是轻量的，并且当一个单纯的、轻量的@racket[rel-string]足够时，它应该@italic{不（not）}被使用。

@;{The automatic @filepath{.ss} and @filepath{.rkt} conversions apply as
with other forms.}
  这个自动的@filepath{.ss}和@filepath{.rkt}转换应用为其它表。
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
是指@racket[base]的一个子模块。在@racket[submod]中的@racket[element]的序列指定一个子模块名称的路径以到达最终的子模块。

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
使用@racket["."]作为在@racket[submod]中的@racket[base]代表外围模块。使用@racket[".."]作为@racket[base]等效于使用@racket["."]后跟一个额外的@racket[".."]。当一个表@racket[(#,(racket quote) id)]的路径指一个子模块时，它等效于@racket[(submod "."  id)]。

@;{Using @racket[".."] as an @racket[element] cancels one submodule step, effectively
referring to the enclosing module. For example, @racket[(submod "..")]
refers to the enclosing module of the submodule in which the path
appears.}
使用@racket[".."]作为一个@racket[element]取消一个子模块步骤，实际上指定外围模块。例如，@racket[(submod "..")]指路径出现在其中的子模块的封闭模块。

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
