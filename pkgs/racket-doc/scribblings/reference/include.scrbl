#lang scribble/doc
@(require "mz.rkt")

@;{@title[#:tag "include"]{File Inclusion}}
@title[#:tag "include"]{文件包含}

@note-lib[racket/include]

本节所记录的绑定是由racket/include和racket库提供的，但racket/base库不提供。

@defform/subs[#:literals (file lib)
              (include path-spec)
              ([path-spec string
                          (file string)
                          (lib string ...+)])]{

@;{Inlines the syntax in the file designated by @racket[path-spec] in
place of the @racket[include] expression.}
文件中的内联语法通过在@racket[include]表达式这个地方里的@racket[path-spec]指定。
 
@;{A @racket[path-spec] resembles a subset of the @racket[_mod-path]
forms for @racket[require], but it specifies a file whose content need
not be a module. That is, @racket[string] refers to a file using a
platform-independent relative path, @racket[(file string)] refers to a
file using platform-specific notation, and @racket[(lib string ...)]
refers to a file within a collection.}
一个@racket[path-spec]类似于一个对@racket[require]的@racket[_mod-path]表子集，但它指定一个内容不需要是模块的一个文件。也就是说，@racket[string]是指一个使用一个不依赖于平台的相对路径的文件，@racket[(file string)]指一个使用特定于平台的符号的文件，而@racket[(lib string ...)]指的是一个集合中的一个文件。

@;{If @racket[path-spec] specifies a relative path, the path is resolved
relative to the source for the @racket[include] expression, if that
source is a complete path string. If the source is not a complete path
string, then @racket[path-spec] is resolved relative to
@racket[(current-load-relative-directory)] if it is not @racket[#f],
or relative to @racket[(current-directory)] otherwise.}
如果@racket[path-spec]指定一个相对路径，并且如果该源是一个完整的路径字符串，则该路径相对于@racket[include]表达式的源解析。如果源是一个不完整的路径字符串，并且如果不是#f，那么@racket[path-spec]相对@racket[(current-load-relative-directory)]解析，否则，也可能相对@racket[(current-directory)]解析。

@;{The included syntax is given the lexical context of the
@racket[include] expression, while the included syntax's source
location refers to its actual source.}
所包含的语法给出了@racket[include]表达式的词法上下文，而包含的语法的源位置是指其实际的源。
}

@defform[(include-at/relative-to context source path-spec)]{

@;{Like @racket[include], except that the lexical context of
@racket[context] is used for the included syntax, and a relative
@racket[path-spec] is resolved with respect to the source of
@racket[source]. The @racket[context] and @racket[source] elements are
otherwise discarded by expansion.}
类似于@racket[include]，除了@racket[context]的词法上下文被用于包含的语法，并且一个相对的@racket[path-spec]用某方面解析到@racket[source]的源。否则@racket[context]和@racket[source]元素将被扩展丢弃。
}

@defform[(include/reader path-spec reader-expr)]{

@;{Like @racket[include], except that the procedure produced by the
expression @racket[reader-expr] is used to read the included file,
instead of @racket[read-syntax].}
和@racket[include]同样地，除了被表达式@racket[reader-expr]产生的程序是用于读取包含文件，而不是@racket[read-syntax]。

@;{The @racket[reader-expr] is evaluated at expansion time in the
@tech{transformer environment}. Since it serves as a replacement for
@racket[read-syntax], the expression's value should be a procedure
that consumes two inputs---a string representing the source and an
input port---and produces a syntax object or @racket[eof]. The
procedure will be called repeatedly until it produces @racket[eof].}
@racket[reader-expr]在@tech{转换器环境}里的扩展时间求值。因为它作为@racket[read-syntax]的一个替换提供服务，这个表达式的值应该是一个过程，它接受两个输入——一个代表源的字符串和一个输入端口——同时产生一个语法对象或@racket[eof]。该过程将反复调用，直到生成@racket[eof]为止。

@;{The syntax objects returned by the procedure should have source
location information, but usually no lexical context; any lexical
context in the syntax objects will be ignored.}
过程返回的语法对象应该有源位置信息，但通常没有词法上下文；语法对象中的任何词法上下文都将被忽略。
}

@defform[(include-at/relative-to/reader context source path-spec reader-expr)]{

@;{Combines @racket[include-at/relative-to] and @racket[include/reader].}
合并@racket[include-at/relative-to]和@racket[include/reader]。
}
