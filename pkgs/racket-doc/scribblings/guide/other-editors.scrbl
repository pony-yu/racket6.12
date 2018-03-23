#lang scribble/manual
@(require "guide-utils.rkt")

@;{@title[#:tag "other-editors" #:style 'toc]{Command-Line Tools and Your Editor of Choice}}
@title[#:tag "other-editors" #:style 'toc]{命令行工具和你的编辑器选择}
@; author["Vincent St-Amour" "Asumu Takikawa" "Jon Rafkind"]

@;{Although DrRacket is the easiest way for most people to start with
Racket, many Racketeers prefer command-line tools and other text
editors.  The Racket distribution includes several command-line tools,
and popular editors include or support packages to make them work well
with Racket.}
虽然DrRacket是大多数人用Racket开始的最简单的方法，许多Racket使用者喜欢命令行工具和其它文本编辑器。Racket分配包括几个命令行工具，流行的编辑器包括或支持包以使它们能很好地配合Racket。

@local-table-of-contents[]

@; ------------------------------------------------------------
@include-section["cmdline.scrbl"]

@; ------------------------------------------------------------
@;{@section{Emacs}}
@section{Emacs}

@;{Emacs has long been a favorite among Lispers and Schemers, and is
popular among Racketeers as well.}
Emacs一直是一个在Lisp使用者和Scheme使用者中特别受欢迎的，并且也是在Racket使用者中流行的。

@;{@subsection{Major Modes}}
@subsection{主要模式}

@itemlist[

 @item{
  @;{@hyperlink["https://github.com/greghendershott/racket-mode"]{Racket mode}
       provides thorough syntax highlighting and DrRacket-style REPL
       and buffer execution support for Emacs.}
@hyperlink["https://github.com/greghendershott/racket-mode"]{Racket模式}通过语法高亮和DrRacket风格REPL及Emacs缓冲区执行对Emacs支持。

       @;{Racket mode can be installed via @hyperlink["http://melpa.milkbox.net"]{MELPA}
       or manually from the Github repository.}
Racket模式可以通过@hyperlink["http://melpa.milkbox.net"]{MELPA}或安装melpa或手动从GitHub库安装。
}

 @item{
  @;{@hyperlink["http://www.neilvandyke.org/quack/"]{Quack} is an
       extension of Emacs's @tt{scheme-mode} that provides enhanced
       support for Racket, including highlighting and indentation of
       Racket-specific forms, and documentation integration.}
@hyperlink["http://www.neilvandyke.org/quack/"]{Quack}是一个为Racket提供更有力的支持的Emacs的@tt{scheme模式（scheme-mode）}的扩展，包括高亮和Racket特定形式的缩进，以及文档一体化。

       @;{Quack is included in the Debian and Ubuntu repositories as part
       of the @tt{emacs-goodies-el} package. A Gentoo port is also
       available (under the name @tt{app-emacs/quack}).}
Quack是包含在Debian和Ubuntu库里作为@tt{emacs-goodies-el}包的一部分。一个Gentoo端口也可获取的（在名字@tt{app-emacs/quack}下）。
}

 @item{
  @;{@hyperlink["http://www.nongnu.org/geiser/"]{Geiser} provides a
       programming environment where the editor is tightly integrated
       with the Racket REPL. Programmers accustomed to environments
       such as Slime or Squeak should feel at home using
       Geiser. Geiser requires GNU Emacs 23.2 or better.}
@hyperlink["http://www.nongnu.org/geiser/"]{Geiser}提供了一个编程环境，编辑器和Racket的REPL紧密集成。习惯用Slime或Squeak环境的程序员使用Geiser应该有宾至如归的感觉。Geiser要求GNU Emacs 23.2或更高的版本。

       @;{Quack and Geiser can be used together, and complement each
       other nicely. More information is available in the
       @hyperlink["http://www.nongnu.org/geiser/"]{Geiser manual}.}
Quack和Geiser可以一起使用，并且相辅相成。更多信息见@hyperlink["http://www.nongnu.org/geiser/"]{Geiser手册}。

       @;{Debian and Ubuntu packages for Geiser are available under the
       name @tt{geiser}.}
为Geiser提供的Debian和Ubuntu软件包在名称@tt{geiser}下适可获取的。
 }

 @item{
  @;{Emacs ships with a major mode for Scheme, @tt{scheme-mode},
       that while not as featureful as the above options, works
       reasonably well for editing Racket code. However, this mode
       does not provide support for Racket-specific forms.}
Emacs用一个为Scheme的主要模式传递，Scheme模式，而不是与上面的选项一样的特性，合理地编辑Racket代码。然而，这种模式并不能为Racket特定形式提供支持。
}

 @item{
  @;{No Racket program is complete without documentation. Scribble
       support for emacs is available with Neil Van Dyke's
       @hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{Scribble
       Mode}.}
没有文件，Racket项目是不完整的。Scribble支持emacs可用Neil Van Dyke的@hyperlink["http://www.neilvandyke.org/scribble-emacs/"]{Scribble模式}获取。。

       @;{In addition, @tt{texinfo-mode} (included with GNU Emacs) and
        plain text modes work well when editing Scribble
        documents. The Racket major modes above are not really suited
        to this task, given how different Scribble's syntax is from
        Racket's.}
此外，当编辑Scribble文件的时候，@tt{texinfo模式}（包括用GNU Emacs）和纯文本模式工作会非常好。鉴于与Racket相比Scribble语法是如此不同，上边的Racket主要模式不是真正的适合这种任务。
 }

]

@;{@subsection{Minor Modes}}
@subsection{小模式}

@itemlist[

 @item{
  @;{@hyperlink["http://mumble.net/~campbell/emacs/paredit.el"]{Paredit}
       is a minor mode for pseudo-structurally editing programs in
       Lisp-like languages. In addition to providing high-level
       S-expression editing commands, it prevents you from
       accidentally unbalancing parentheses.}
@hyperlink["http://mumble.net/~campbell/emacs/paredit.el"]{Paredit}是在LISP类似语言中伪结构编辑程序的一个小模式。除了提供高阶S表达式编辑命令外，它可以帮你防止意外的不平衡括号。

       @;{Debian and Ubuntu packages for Paredit are available under the
       name @tt{paredit-el}.}
对Paredit的Debian和Ubuntu软件包在名字@tt{paredit-el}下可以获取。
 }

 @item{
  @;{@hyperlink["https://github.com/Fuco1/smartparens"]{Smartparens}
       is a minor mode for editing s-expressions, keeping parentheses
       balanced, etc.  Similar to Paredit.}
Smartparen对编辑S表达式是一个小模式，保持括号平衡、类似于Paredit等等。
 }

 @item{
  @;{Alex Shinn's
       @hyperlink["http://synthcode.com/wiki/scheme-complete"]{scheme-complete}
       provides intelligent, context-sensitive code completion. It
       also integrates with Emacs's @tt{eldoc} mode to provide live
       documentation in the minibuffer.}
Alex Shinn的@hyperlink["http://synthcode.com/wiki/scheme-complete"]{scheme-complete}提供了智能的、上下文敏感的代码完成。它还用Emacs的@tt{eldoc}模式集成以在小缓冲区中提供现场文档。

       @;{While this mode was designed for @seclink["r5rs"]{@|r5rs|}, it
       can still be useful for Racket development. That the tool is
       unaware of large portions of the Racket standard library, and
       there may be some discrepancies in the live documentation in
       cases where Scheme and Racket have diverged.}
而这种模式是专为@seclink["r5rs"]{@|r5rs|}设计，它仍能用于Racket的开发。该工具不知道Racket标准库的大部分，而且在Scheme和Racket有分歧的情况下，现场文档可能有一些出入。
 }

 @item{
  @;{The
       @hyperlink["http://www.emacswiki.org/emacs/RainbowDelimiters"]{RainbowDelimiters}
       mode colors parentheses and other delimiters according to their
       nesting depth. Coloring by nesting depth makes it easier to
       know, at a glance, which parentheses match.}
@hyperlink["http://www.emacswiki.org/emacs/RainbowDelimiters"]{RainbowDelimiters}模式颜色括号和其它分隔符根据嵌套深度确定。通过嵌套深度着色使人们一目了然地知道哪些圆括号匹配。
}

 @item{
  @;{@hyperlink["http://www.emacswiki.org/emacs/ParenFace"]{ParenFace}
       lets you choose in which face (font, color, etc.) parentheses
       should be displayed. Choosing an alternate face makes it
       possible to make ``tone down'' parentheses.}
@hyperlink["http://www.emacswiki.org/emacs/ParenFace"]{ParenFace}让你选择在哪面（字体，颜色，等等）的括号应显示。选择一个交替的面可以使“tone down（按下）”括号。 
}
 
]

@;{@subsection{Packages specific to Evil Mode}}
@subsection{Evil模式的专有包}

@itemlist[

 @item{
  @;{@hyperlink["https://github.com/willghatch/emacs-on-parens"]{on-parens}
       is a wrapper for smartparens motions to work better with
       evil-mode's normal state.}
on-parens是对smartparens行为用evil模式的通常状态去更好工作的一个包装。
 }

 @item{
  @;{@hyperlink["https://github.com/timcharper/evil-surround"]{evil-surround}
       provides commands to add, remove, and change parentheses and
       other delimiters.}
@hyperlink["https://github.com/timcharper/evil-surround"]{evil-surround}提供命令去添加、删除和改变括号和其它分隔符。
  }

 @item{
@;{@hyperlink["https://github.com/noctuid/evil-textobj-anyblock"]{evil-textobj-anyblock}
       adds a text-object that matches the closest of any
       parenthesis or other delimiter pair.}
@hyperlink["https://github.com/noctuid/evil-textobj-anyblock"]{evil-textobj-anyblock}添加一个文本对象相匹配最接近的任何括号或其它分隔符配对。
}

]

@; ------------------------------------------------------------

@;{@section{Vim}}
@section{Vim}

@;{Many distributions of Vim ship with support for Scheme, which will
mostly work for Racket. You can enable filetype detection of Racket
files as Scheme with the following:}
带Scheme支持的Vim运送的许多分配，它们将更多地用于Racket工作。你可以像Scheme一样用以下方式激活Racket文件的文件类型检查：

@verbatim[#:indent 2]|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=scheme
endif
}|

@;{Alternatively, you can use the
@hyperlink["https://github.com/wlangstroth/vim-racket"]{vim-racket}
plugin to enable auto-detection, indentation, and syntax highlighting
specifically for Racket files. Using the plugin is the easiest method, but if you
would like to roll your own settings or override settings from the plugin, add
something like the following to your @filepath{.vimrc} file:}
或者，你可以使用@hyperlink["https://github.com/wlangstroth/vim-racket"]{vim-racket}插件来实现自动检测、缩进和专门针对Racket文件的语法高亮显示。使用插件是最简单的方法，但是如果你想把你自己的设置或重写插件设置，添加类似于下面的内容到你的@filepath{.vimrc}文件：

@verbatim[#:indent 2]|{
if has("autocmd")
  au BufReadPost *.rkt,*.rktl set filetype=racket
  au filetype racket set lisp
  au filetype racket set autoindent
endif
}|

@;{However, if you take this path you may need to do more work when installing
plugins because many Lisp-related plugins and scripts for vim are not aware of
Racket. You can also set these conditional commands in a @filepath{scheme.vim} or
@filepath{racket.vim} file in the @filepath{ftplugin} subdirectory of your vim folder.}
然而，如果您采取这一路径，你可能需要在安装插件时做更多的工作，因为很多与Lisp相关的插件和vim脚本都不知道Racket。你也可以在一个@filepath{scheme.vim}中或在vim文件夹的@filepath{ftplugin}子文件夹中的@filepath{racket.vim}文件中设置这些条件命令。

@;{Most installations of vim will automatically have useful defaults enabled,
but if your installation does not, you will want to set at least the following
in your @filepath{.vimrc} file:}
vim的大多数安装会自动具有有用的默认启用，但如果你的安装没有，你会希望至少在你的@filepath{.vimrc}文件里去设置：

@verbatim[#:indent 2]|{
" Syntax highlighting
syntax on

" These lines make vim load various plugins
filetype on
filetype indent on
filetype plugin on

" No tabs!
set expandtab
}|

@;{@subsubsub*section{Indentation}}
@subsubsub*section{缩格}

@;{You can enable indentation for Racket by setting both the @tt{lisp} and
@tt{autoindent} options in Vim. However, the indentation is limited and not as
complete as what you can get in Emacs. You can also use Dorai Sitaram's
@hyperlink["https://github.com/ds26gte/scmindent"]{scmindent} for
better indentation of Racket code. The instructions on how to
use the indenter are available on the website.}
你可以通过在Vim里设置@tt{lisp}和@tt{autoindent（自动缩格）}选项启用Racket的缩格。然而，缩格是有限的也不是和你在Emacs中能得到的一样完整。你也可以用Dorai Sitaram的@hyperlink["https://github.com/ds26gte/scmindent"]{scmindent}达到Racket代码的更好缩格。有关如何使用缩格器的说明可在网站上查阅。

@;{If you use the built-in indenter, you can customize it by setting how to
indent certain keywords. The vim-racket plugin mentioned above sets
some default keywords for you. You can add keywords yourself in your
@filepath{.vimrc} file like this:}
如果使用内置的缩格器，可以通过设置如何缩进某些关键字来定制它。上面提到的vim-racket插件为你设置了一些默认关键字。你可以在你的@filepath{.vimrc}文件里添加你自己的关键字，像这样：

@verbatim[#:indent 2]|{
" By default vim will indent arguments after the function name
" but sometimes you want to only indent by 2 spaces similar to
" how DrRacket indents define. Set the `lispwords' variable to
" add function names that should have this type of indenting.

set lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules
set lispwords+=..more..
}|

@;{@subsubsub*section{Highlighting}}
@subsubsub*section{突出}

@;{The @hyperlink["http://www.vim.org/scripts/script.php?script_id=1230"]{Rainbow
Parenthesis} script for vim can be useful for more visible parenthesis
matching. Syntax highlighting for Scheme is shipped with vim on many platforms,
which will work for the most part with Racket. The vim-racket script
provides good default highlighting settings for you.}
用于可视化的彩虹括号（Rainbow
Parenthesis}）脚本可以用于更可见的括号匹配。在许多平台上，有很多功能都是通过高亮显示来实现的。为你提供了良好的默认高亮显示设置。

@;{@subsubsub*section{Structured Editing}}
@subsubsub*section{结构化的编辑}

@;{The @hyperlink["http://www.vim.org/scripts/script.php?script_id=2531"]{Slimv}
plugin has a paredit mode that works like paredit in Emacs. However, the plugin
is not aware of Racket. You can either set vim to treat Racket as Scheme files
or you can modify the paredit script to load on @filepath{.rkt} files.}
@hyperlink["http://www.vim.org/scripts/script.php?script_id=2531"]{Slimv}插件有一paredit模式，就像Emacs里的paredit工作方式。然而，插件不知道Racket。你可以设置Vim去把Racket作为Scheme文件，也可以修改paredit脚本以加载@filepath{.rkt}文件。

@;{@subsubsub*section{Scribble}}
@subsubsub*section{Scribble}

Vim support for writing scribble documents is provided by the
@hyperlink["http://www.vim.org/scripts/script.php?script_id=3756"]{scribble.vim}
plugin.
对书写scribble文件，Vim通过@hyperlink["http://www.vim.org/scripts/script.php?script_id=3756"]{scribble.vim}插件被支持。

@;{@subsubsub*section{Miscellaneous}}
@subsubsub*section{混杂的}

@;{If you are installing many vim plugins (not necessary specific to Racket), we
recommend using a plugin that will make loading other plugins easier.
@hyperlink["http://www.vim.org/scripts/script.php?script_id=2332"]{Pathogen} is
one plugin that does this; using it, you can install new plugins by extracting
them to subdirectories in the @filepath{bundle} folder of your Vim installation.}
如果你安装了很多Vim插件（不需要特别针对Racket），我们建议使用一个插件，让其它插件更容易加载。@hyperlink["http://www.vim.org/scripts/script.php?script_id=2332"]{Pathogen}是一个这样做的插件；使用它，你可以通过在你Vim安装的@filepath{bundle}文件夹里提取它们到子目录来安装新插件。

@; ------------------------------------------------------------

@;{@section{Sublime Text}}
@section{Sublime Text}

@;{The @hyperlink["https://sublime.wbond.net/packages/Racket"]{Racket package}
provides support for syntax highlighting and building for Sublime Text.}
@hyperlink["https://sublime.wbond.net/packages/Racket"]{Racket package}支持语法高亮显示和构建 Sublime Text。