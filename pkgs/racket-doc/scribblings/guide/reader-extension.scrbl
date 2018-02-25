#lang scribble/doc
@(require scribble/manual scribble/bnf scribble/eval
          "guide-utils.rkt" "modfile.rkt"
          (for-label racket/match syntax/readerr))

@;{@title[#:tag "hash-reader"]{Reader Extensions}}
@title[#:tag "hash-reader"]{读取器扩展}

@;{@refdetails["parse-reader"]{reader extensions}}
@refdetails["parse-reader"]{读取器扩展}

@;{The @tech{reader} layer of the Racket language can be extended through
the @racketmetafont{#reader} form. A reader extension is implemented
as a module that is named after @racketmetafont{#reader}. The module
exports functions that parse raw characters into a form to be consumed
by the @tech{expander} layer.}
Racket语言@tech{读取器（reader）}层可以通过@racketmetafont{#reader}表扩展。一个读取器扩展作为一个模块实现，该模块在@racketmetafont{#reader}之后命名。模块导出的函数将原始字符解析成一个被@tech{扩展器（expander）}层接受的表。

@;{The syntax of @racketmetafont{#reader} is}
@racketmetafont{#reader}的语法是

@racketblock[@#,(BNF-seq @litchar{#reader} @nonterm{module-path} @nonterm{reader-specific})]

@;{where @nonterm{module-path} names a module that provides
@racketidfont{read} and @racketidfont{read-syntax} functions. The
@nonterm{reader-specific} part is a sequence of characters that is
parsed as determined by the @racketidfont{read} and
@racketidfont{read-syntax} functions from @nonterm{module-path}.}
在@nonterm{module-path}命名一个模块，它提供@racketidfont{read}和@racketidfont{read-syntax}函数。@nonterm{reader-specific}部分是一个字符序列，它被解析为确定——被来自@nonterm{module-path}的@racketidfont{read}和@racketidfont{read-syntax}函数。

@;{For example, suppose that file @filepath{five.rkt} contains}
例如，假设文件@filepath{five.rkt}包含

@racketmodfile["five.rkt"]

@;{Then, the program}
那么，程序

@racketmod[
racket/base

'(1 @#,(elem @racketmetafont{#reader} @racket["five.rkt"] @tt{23456} @racket[7]) 8)
]

@;{is equivalent to}
相当于

@racketmod[
racket/base

'(1 ("23456") 7 8)
]

@;{because the @racketidfont{read} and @racketidfont{read-syntax}
functions of @filepath{five.rkt} both read five characters from the
input stream and put them into a string and then a list. The reader
functions from @filepath{five.rkt} are not obliged to follow Racket
lexical conventions and treat the continuous sequence @litchar{234567}
as a single number. Since only the @litchar{23456} part is consumed by
@racketidfont{read} or @racketidfont{read-syntax}, the @litchar{7}
remains to be parsed in the usual Racket way. Similarly, the reader
functions from @filepath{five.rkt} are not obliged to ignore
whitespace, and}
因为@filepath{five.rkt}的@racketidfont{read}和@racketidfont{read-syntax}函数既从输入流中读取五个字符又把它们放入一个字符串进而成为一个列表。来自@filepath{five.rkt}的读取器函数没有义务遵循Racket词法约定并作为一个单一的数处理连续序列@litchar{234567}。由于只有@litchar{23456}部分被@racketidfont{read}或@racketidfont{read-syntax}所接受，@litchar{7}仍然需要用通常的Racket方式进行分析。同样，来自@filepath{five.rkt}的读取器函数没有义务忽略空白，以及

@racketmod[
racket/base

'(1 @#,(elem @racketmetafont{#reader} @racket["five.rkt"] @hspace[1] @tt{2345} @racket[67]) 8)
]

@;{is equivalent to}
相当于

@racketmod[
racket/base

'(1 (" 2345") 67 8)
]

@;{since the first character immediately after @racket["five.rkt"] is a
space.}
由于@racket["five.rkt"]后的第一个字符立即是一个空格。

@;{A @racketmetafont{#reader} form can be used in the @tech{REPL}, too:}
一个@racketmetafont{#reader}表也能够在@tech{REPL}中使用：

@interaction[
(eval:alts '@#,(elem @racketmetafont{#reader}@racket["five.rkt"]@tt{abcde}) '#reader"five.rkt"abcde)
]

@; ----------------------------------------------------------------------

@;{@section{Source Locations}}
@section{源位置}

@;{The difference between @racketidfont{read} and
@racketidfont{read-syntax} is that @racketidfont{read} is meant to be
used for data while @racketidfont{read-syntax} is meant to be used to
parse programs. More precisely, the @racketidfont{read} function will
be used when the enclosing stream is being parsed by the Racket
@racket[read], and @racketidfont{read-syntax} is used when the
enclosing stream is being parsed by the Racket @racket[read-syntax]
function. Nothing requires @racketidfont{read} and
@racketidfont{read-syntax} to parse input in the same way, but making
them different would confuse programmers and tools.}
@racketidfont{read}和@racketidfont{read-syntax}的区别在于@racketidfont{read}是用于数据，而 @racketidfont{read-syntax}是用来解析程序的。更确切地说，当通过Racket的@racket[read]解析封闭流时，将使用@racketidfont{read}函数，当封闭流由Racket的@racket[read-syntax]函数解析时，使用@racketidfont{read-syntax}。没有什么需要@racketidfont{read}和@racketidfont{read-syntax}用同样的方法解析输入，但是使它们不同会混淆程序员和工具。

@;{The @racketidfont{read-syntax} function can return the same kind of
value as @racketidfont{read}, but it should normally return a
@tech{syntax object} that connects the parsed expression with source
locations. Unlike the @filepath{five.rkt} example, the
@racketidfont{read-syntax} function is typically implemented directly
to produce @tech{syntax objects}, and then @racketidfont{read} can use
@racketidfont{read-syntax} and strip away @tech{syntax object}
wrappers to produce a raw result.}
@racketidfont{read-syntax}函数可以返回与@racketidfont{read}相同的值，但它通常应该返回一个@tech{语法对象（syntax object）}，它将解析表达式与源位置连接起来。与@filepath{five.rkt}的例子不同的是，@racketidfont{read-syntax}函数通常是直接实现生成@tech{语法对象（syntax object）}，然后@racketidfont{read}可以使用@racketidfont{read-syntax}和揭开@tech{语法对象（syntax object）}封装器以产生一个原生的结果。

@;{The following @filepath{arith.rkt} module implements a reader to
parse simple infix arithmetic expressions into Racket forms. For
example, @litchar{1*2+3} parses into the Racket form @racket[(+ (* 1
2) 3)]. The supported operators are @litchar{+}, @litchar{-},
@litchar{*}, and @litchar{/}, while operands can be unsigned integers
or single-letter variables. The implementation uses
@racket[port-next-location] to obtain the current source location, and
it uses @racket[datum->syntax] to turn raw values into @tech{syntax
objects}.}
下面的@filepath{arith.rkt}模块实现了一个读取器以解析简单的中缀算术表达式为Racket表。例如，@litchar{1*2+3}解析成Racket表@racket[(+ (* 1
2) 3)]。支持的运算符是@litchar{+}、@litchar{-}、@litchar{*}和@litchar{/}，而操作数可以是无符号整数或单字母变量。该实现使用@racket[port-next-location]获取当前源位置，并使用 @racket[datum->syntax]将原始值转换为@tech{语法对象（syntax object）}。

@racketmodfile["arith.rkt"]

@;{If the @filepath{arith.rkt} reader is used in an expression position,
then its parse result will be treated as a Racket expression. If it is
used in a quoted form, however, then it just produces a number or a
list:}
如果@filepath{arith.rkt}读取器在一个表达式位置中使用，那么它的解析结果将被视为一个Racket表达。但是，如果它被用在引用的表中，那么它只生成一个数字或一个列表：

@interaction[
(eval:alts @#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) #reader"arith.rkt" 1*2+3 )
(eval:alts '@#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) '#reader"arith.rkt" 1*2+3 )
]

@;{The @filepath{arith.rkt} reader could also be used in positions that
make no sense. Since the @racketidfont{read-syntax} implementation
tracks source locations, syntax errors can at least refer to parts of
the input in terms of their original locations (at the beginning of
the error message):}
@filepath{arith.rkt}读取器也可以在毫无意义的位置中使用。由于@racketidfont{read-syntax}实现跟踪源位置，语法错误至少可以根据原始位置（在错误消息的开头）引用输入部分：

@interaction[
(eval:alts (let @#,(elem @racketmetafont{#reader}@racket["arith.rkt"]@hspace[1]@tt{1*2+3}) 8)
           (eval (parameterize ([read-accept-reader #t])
                   (read-syntax 'repl (let ([p @open-input-string{(let #reader"arith.rkt" 1*2+3 8)}])
                                        (port-count-lines! p)
                                        p)))))
]

@; ----------------------------------------------------------------------

@;{@section[#:tag "readtable"]{Readtables}}
@section[#:tag "readtable"]{readtable（读取表格）}

@;{A reader extension's ability to parse input characters in an arbitrary
way can be powerful, but many cases of lexical extension call for a
less general but more composable approach. In much the same way that
the @tech{expander} level of Racket syntax can be extended through
@tech{macros}, the @tech{reader} level of Racket syntax can be
composably extended through a @deftech{readtable}.}
一个读取器扩展对以任意方式解析输入字符的能力可以是强大的，但很多情况下，词汇扩展需要一个不那么一般但更可组合的方法。同样，Racket语法的@tech{扩展器（expander）}等级可以通过@tech{宏（macros）}来扩展，Racket语法的@tech{reader}等级可以通过一个@deftech{readtable}来复合扩展。

@;{The Racket reader is a recursive-descent parser, and the
@tech{readtable} maps characters to parsing handlers. For example, the
default readtable maps @litchar{(} to a handler that recursively
parses subforms until it finds a @litchar{)}. The
@racket[current-readtable] @tech{parameter} determines the
@tech{readtable} that is used by @racket[read] or
@racket[read-syntax]. Rather than parsing raw characters directly, a
reader extension can install an extended @tech{readtable} and then
chain to @racket[read] or @racket[read-syntax].}
Racket读取器是一个递归下降解析器，以及@tech{readtable}映射特征到解析处理器。例如，默认readtable映射@litchar{(}到一个处理器，它递归解析子表直到找到一个@litchar{)}。@racket[current-readtable]@tech{参数（parameter）}决定@tech{readtable}由@racket[read]或@racket[read-syntax]使用。而不是直接解析原始特征，一个读取器扩展可以安装一个扩展的@tech{readtable}然后链接@racket[read]或@racket[read-syntax]。

@;{@guideother{See @secref["parameterize"] for an introduction to
@tech{parameters}.}}
@guideother{参见《@secref["parameterize"]》——对《@tech{parameters}》的介绍。}

@;{The @racket[make-readtable] function constructs a new @tech{readtable}
as an extension of an existing one. It accepts a sequence of
specifications in terms of a character, a type of mapping for the
character, and (for certain types of mappings) a parsing
procedure. For example, to extend the readtable so that @litchar{$}
can be used to start and end infix expressions, implement a
@racket[read-dollar] function and use:}
@racket[make-readtable]函数构建一个新的@tech{readtable}作为一个现有的一个扩展。它根据一个字符、一个字符映射类型和（对于的某些映射的类型）一个解析程序接受一系列规范。例如，扩展readtable使@litchar{$}可以用来开始和结束中缀表达式，实现@racket[read-dollar]函数并使用：

@racketblock[
(make-readtable (current-readtable)
                #\$ 'terminating-macro read-dollar)
]

@;{The protocol for @racket[read-dollar] requires the function to accept
different numbers of arguments depending on whether it is being used
in @racket[read] or @racket[read-syntax] mode. In @racket[read] mode,
the parser function is given two arguments: the character that
triggered the parser function and the input port that is being
read. In @racket[read-syntax] mode, the function must accept four
additional arguments that provide the source location of the
character.}
@racket[read-dollar]协议要求函数接受不同参数的数值，这取决于它是否被用于@racket[read]或@racket[read-syntax]模式。在@racket[read]模式中，解析器函数给出两个参数：触发解析器函数的字符和正在读取的输入端口。在@racket[read-syntax]模式中，函数必须接受提供字符源位置的四个附加参数。

@;{The following @filepath{dollar.rkt} module defines a
@racket[read-dollar] function in terms of the @racketidfont{read} and
@racketidfont{read-syntax} functions provided by @filepath{arith.rkt},
and it puts @racket[read-dollar] together with new @racketidfont{read} and
@racketidfont{read-syntax} functions that install the readtable and
chain to Racket's @racket[read] or @racket[read-syntax]:}
下面的@filepath{dollar.rkt}模块根据被@filepath{arith.rkt}提供的@racketidfont{read}和@racketidfont{read-syntax}函数定义了一个@racket[read-dollar]函数，并将@racket[read-dollar]连同新的@racketidfont{read}和@racketidfont{read-syntax}函数放在一起，它安装readtable并链接Racket的@racket[read]或@racket[read-syntax]：

@racketmodfile["dollar.rkt"]

@;{With this reader extension, a single @racketmetafont{#reader} can be
used at the beginning of an expression to enable multiple uses of
@litchar{$} that switch to infix arithmetic:}
这个读取器扩展，一个单一的@racketmetafont{#reader}可用于一个表达式的开始以使交换中缀算法的@litchar{$}的多重使用成为可能：

@interaction[
(eval:alts @#,(elem @racketmetafont{#reader}@racket["dollar.rkt"]@hspace[1]
                    @racket[(let ([a @#,tt{$1*2+3$}] [b @#,tt{$5/6$}]) $a+b$)])
           #reader"dollar.rkt" (let ([a $1*2+3$] [b $5/6$]) $a+b$))
]
