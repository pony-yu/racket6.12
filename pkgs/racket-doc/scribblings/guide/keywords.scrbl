#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "keywords"]{Keywords}}
@title[#:tag "keywords"]{关键字（Keyword）}

@;{A @deftech{keyword} value is similar to a symbol (see
@secref["symbols"]), but its printed form is prefixed with
@litchar{#:}.}
一个@deftech{关键字（keyword）}值类似于一个符号（见@secref["symbols"]），但它的打印形式是用前缀@litchar{#:}。

@;{@refdetails/gory["parse-keyword"]{the syntax of keywords}}
@refdetails/gory["parse-keyword"]{关键字语法}

@examples[
(string->keyword "apple")
'#:apple
(eq? '#:apple (string->keyword "apple"))
]

@;{More precisely, a keyword is analogous to an identifier; in the same
way that an identifier can be quoted to produce a symbol, a keyword
can be quoted to produce a value. The same term ``keyword'' is used in
both cases, but we sometimes use @defterm{keyword value} to refer more
specifically to the result of a quote-keyword expression or of
@racket[string->keyword]. An unquoted keyword is not an expression,
just as an unquoted identifier does not produce a symbol:}
更确切地说，关键字类似于标识符；以同样的方式，可以引用标识符来生成符号，可以引用关键字来生成值。在这两种情况下都使用同一术语“关键字”，但有时我们使用@defterm{关键字值（keyword value）}更具体地引用引号关键字表达式或使用@racket[string->keyword]过程的结果。一个不带引号的关键字不是表达式，只是作为一个不带引号的标识符不产生符号：

@examples[
not-a-symbol-expression
#:not-a-keyword-expression
]

@;{Despite their similarities, keywords are used in a different way than
identifiers or symbols. Keywords are intended for use (unquoted) as
special markers in argument lists and in certain syntactic forms.  For
run-time flags and enumerations, use symbols instead of keywords.  The
example below illustrates the distinct roles of keywords and symbols.}
尽管它们有相似之处，但关键字的使用方式不同于标识符或符号。关键字是为了使用（不带引号）作为参数列表和在特定的句法形式的特殊标记。运行时的标记和枚举，而不是关键字用符号。下面的示例说明了关键字和符号的不同角色。

@examples[
(code:line (define dir (find-system-path 'temp-dir)) (code:comment @#,t{not @racket['#:temp-dir]}))
(with-output-to-file (build-path dir "stuff.txt")
  (lambda () (printf "example\n"))
  (code:comment @#,t{@;{optional @racket[#:mode] argument can be @racket['text] or @racket['binary]}可选的@racket[#:mode]参数可以是@racket['text]或@racket['binary]})
  #:mode 'text
  (code:comment @#,t{@;{optional @racket[#:exists] argument can be @racket['replace], @racket['truncate], ...}可选的@racket[#:exists]参数可以是@racket['replace]、@racket['truncate]、...})
  #:exists 'replace)
]

@interaction-eval[(delete-file (build-path (find-system-path 'temp-dir) "stuff.txt"))]
