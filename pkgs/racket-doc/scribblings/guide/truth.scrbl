#lang scribble/doc
@(require scribble/manual scribble/eval racket/list "guide-utils.rkt"

          (for-label racket/list
                     (only-in racket/class is-a?)))

@(define list-eval (make-base-eval))
@(interaction-eval #:eval list-eval (require racket/list))

@;{@title{Pairs, Lists, and Racket Syntax}}
@title[#:tag "Pairs-Lists-and-Racket-Syntax"]{pair、list和Racket的语法}

@;{The @racket[cons] function actually accepts any two values, not just
a list for the second argument. When the second argument is not
@racket[empty] and not itself produced by @racket[cons], the result prints
in a special way. The two values joined with @racket[cons] are printed
between parentheses, but with a dot (i.e., a period surrounded by
whitespace) in between:}
@racket[cons]函数实际上接受任意两个值，而不只是一个给第二个参数的列表。当第二个参数不是@racket[empty]且不是自己通过@racket[cons]产生的时，结果以一种特殊的方式打印出来。两个值用@racket[cons]凑在一起被打印在括号之间，但在两者之间有一个点（即，一个被空格环绕的句点）：

@interaction[(cons 1 2) (cons "banana" "split")]

@;{Thus, a value produced by @racket[cons] is not always a list. In
general, the result of @racket[cons] is a @defterm{pair}. The more
traditional name for the @racket[cons?] function is @racket[pair?],
and we'll use the traditional name from now on.}
因此，由@racket[cons]产生的一个值并不总是一个列表。一般来说，@racket[cons]的结果是一个 @defterm{点对（pair）}。更符合惯例的@racket[cons?]函数名字是@racket[pair?]，那我们从现在开始使用这个符合惯例的名字。

@;{The name @racket[rest] also makes less sense for non-list pairs; the
more traditional names for @racket[first] and @racket[rest] are
@racket[car] and @racket[cdr], respectively. (Granted, the traditional
names are also nonsense. Just remember that ``a'' comes before ``d,''
and @racket[cdr] is pronounced ``could-er.'')}
名字@racket[rest]对非列表点对也意义不大；对@racket[first]和@racket[rest]更符合惯例的名字分别是@racket[car]和@racket[cdr]。（当然，符合惯例的名字也是没有意义的。请记住，“a”出现在“d”之前，并且@racket[cdr]被声明为“could-er（可以）”。

@examples[
#:eval list-eval
(car (cons 1 2))
(cdr (cons 1 2))
(pair? empty)
(pair? (cons 1 2))
(pair? (list 1 2 3))
]

@close-eval[list-eval]

@;{Racket's pair datatype and its relation to lists is essentially a
historical curiosity, along with the dot notation for printing and the
funny names @racket[car] and @racket[cdr]. Pairs are deeply wired into
to the culture, specification, and implementation of Racket, however,
so they survive in the language.}
Racket的点对数据类型和它对表的关系，连同打印的点符号和滑稽的名字@racket[car]及@racket[cdr]本质上是一个历史上的奇特事物。然而，点对深深地被连接进了Racket的文化、详述和实现上，因此它们在语言中得以存在下来。

@;{You are perhaps most likely to encounter a non-list pair when making a
mistake, such as accidentally reversing the arguments to
@racket[cons]:}
在你犯一个错误时，你很可能会遇到一个非列表点对，比如不小心给cons把参数颠倒过来：

@interaction[(cons (list 2 3) 1) (cons 1 (list 2 3))]

@;{Non-list pairs are used intentionally, sometimes. For example, the
@racket[make-hash] function takes a list of pairs, where the
@racket[car] of each pair is a key and the @racket[cdr] is an
arbitrary value.}
非列表点对有时被有意使用。例如，@racket[make-hash]函数取得了一个点对的列表，其中每个点对的@racket[car]是一个键同时@racket[cdr]是一个任意值。

@;{The only thing more confusing to new Racketeers than non-list pairs is
the printing convention for pairs where the second element @italic{is}
a pair, but  a list:}
对新的Racket程序员唯一更困惑的情况莫过于非列表点对是对点对的打印习惯，其第二个元素@italic{是}一个点对而@italic{不是}一个列表：

@interaction[(cons 0 (cons 1 2))]

@;{In general, the rule for printing a pair is as follows: use the dot
notation unless the dot is immediately followed by an open
parenthesis. In that case, remove the dot, the open parenthesis, and the
matching close parenthesis. Thus, @racketresultfont[#:decode? #f]{'(0 . (1 . 2))}
becomes @racketresult['(0 1 . 2)], and
@racketresultfont[#:decode? #f]{'(1 . (2 . (3 . ())))} becomes @racketresult['(1 2 3)].}
一般来说，打印一个点对的规则如下：除非该点紧接着是一个开括号，否则使用点表示法。在这种情况下，去掉点、开括号和匹配的闭括号。由此，@racketresultfont[#:decode? #f]{'(0 . (1 . 2))}变成@racketresult['(0 1 . 2)]，@racketresultfont[#:decode? #f]{'(1 . (2 . (3 . ())))}变成@racketresult['(1 2 3)]。

@;------------------------------------------------------------------------
@;{@section[#:tag "quoting-lists"]{Quoting Pairs and Symbols with @racket[quote]}}
@section[#:tag "quoting-lists"]{用@racket[quote]引用pair和symbol}

@;{A list prints with a quote mark before it, but if an element of a list
is itself a list, then no quote mark is printed for the inner list:}
一个列表在前面打印一个引号标记，但是如果一个列表的一个元素本身是一个列表，那么就不会为内部列表打印引号标记：

@interaction[
(list (list 1) (list 2 3) (list 4))
]

@;{For nested lists, especially, the @racket[quote] form lets you write a
list as an expression in essentially the same way that the list
prints:}
对于嵌套列表，尤其是@racket[quote]表，你可以将列表作为一个表达式来写，基本上与列表打印的方式相同：

@interaction[
(eval:alts (@#,racket[quote] ("red" "green" "blue")) '("red" "green" "blue"))
(eval:alts (@#,racket[quote] ((1) (2 3) (4))) '((1) (2 3) (4)))
(eval:alts (@#,racket[quote] ()) '())
]

@;{The @racket[quote] form works with the dot notation, too, whether the
quoted form is normalized by the dot-parenthesis elimination rule or
not:}
无论引用表是否由点括号消除规则规范，@racket[quote]表都要包含点符号：

@interaction[
(eval:alts (@#,racket[quote] (1 . 2)) '(1 . 2))
(eval:alts (@#,racket[quote] (0 @#,racketparenfont{.} (1 . 2))) '(0 . (1 . 2)))
]

@;{Naturally, lists of any kind can be nested:}
当然，任何种类的列表都可以嵌套：

@interaction[
(list (list 1 2 3) 5 (list "a" "b" "c"))
(eval:alts (@#,racket[quote] ((1 2 3) 5 ("a" "b" "c"))) '((1 2 3) 5 ("a" "b" "c")))
]

@;{If you wrap an identifier with @racket[quote], then you get output
that looks like an identifier, but with a @litchar{'} prefix:}
如果用@racket[quote]包裹标识符，则得到看起来像一个标识符的输出，但带有一个@litchar{'}前缀：

@interaction[
(eval:alts (@#,racket[quote] jane-doe) 'jane-doe)
]

@;{A value that prints like a quoted identifier is a @defterm{symbol}. In the
same way that parenthesized output should not be confused with
expressions, a printed symbol should not be confused with an
identifier. In particular, the symbol @racket[(@#,racket[quote]
@#,racketidfont{map})] has nothing to do with the @racketidfont{map}
identifier or the predefined function that is bound to
@racket[map], except that the symbol and the identifier happen
to be made up of the same letters.}
像一个引用标识符那样打印的一个值是一个@defterm{symbol（符号）}。同样，括号输出不应该和表达式混淆，一个打印符号不应与一个标识符混淆。特别是，除了符号和标识符碰巧由相同的字母组成外，符号@racket[(@#,racket[quote]
@#,racketidfont{map})]与@racketidfont{map}标识符或绑定到@racket[map]的预定义函数无关。

@;{Indeed, the intrinsic value of a symbol is nothing more than its
character content. In this sense, symbols and strings are almost the
same thing, and the main difference is how they print. The functions
@racket[symbol->string] and @racket[string->symbol] convert between
them.}
的确，一个符号固有的值不过是它的字符内容。从这个意义上说，符号和字符串几乎是一样的东西，主要区别在于它们是如何打印的。函数@racket[symbol->string]和@racket[string->symbol]在它们之间转换。

@examples[
map
(eval:alts (@#,racket[quote] @#,racketidfont{map}) 'map)
(eval:alts (symbol? (@#,racket[quote] @#,racketidfont{map})) (symbol? 'map))
(symbol? map)
(procedure? map)
(string->symbol "map")
(eval:alts (symbol->string (@#,racket[quote] @#,racketidfont{map})) (symbol->string 'map))
]

@;{In the same way that @racket[quote] for a list automatically applies
itself to nested lists, @racket[quote] on a parenthesized sequence of
identifiers automatically applies itself to the identifiers to create
a list of symbols:}
同样，对一个列表@racket[quote]会自己自动作用于嵌套列表，在标识符的一个括号序列上的@racket[quote]会自己自动应用到标识符上以创建一个符号列表：

@interaction[
(eval:alts (car (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map}))) (car '(road map)))
(eval:alts (symbol? (car (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map})))) (symbol? (car '(road map))))
]

@;{When a symbol is inside a list that is printed with
@litchar{'}, the @litchar{'} on the symbol is omitted, since
@litchar{'} is doing the job already:}
当一个符号在一个打印有@litchar{'}的列表中时，在符号上的这个@litchar{'}被省略了，因为@litchar{'}已经在做这项工作了：

@interaction[
(eval:alts (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map})) '(road map))
]

@;{The @racket[quote] form has no effect on a literal expression such as
a number or string:}
@racket[quote]表对一个文字表达式，像一个数字或一个字符串这样的，没有影响：

@interaction[
(eval:alts (@#,racket[quote] 42) 42)
(eval:alts (@#,racket[quote] "on the record") "on the record")
]

@;------------------------------------------------------------------------
@;{@section{Abbreviating @racket[quote] with @racketvalfont{@literal{'}}}}
@section[#:tag "Abbreviating-quote-with-quote"]{使用@racketvalfont{@literal{'}}缩写quote}

@;{As you may have guessed, you can abbreviate a use of
@racket[quote] by just putting @litchar{'} in front of a form to
quote:}
你可能已经猜到了，你可以通过仅放置一个@litchar{'}在一个表前面来缩写一个@racket[quote]的使用：

@interaction[
'(1 2 3)
'road
'((1 2 3) road ("a" "b" "c"))
]

@;{In the documentation, @litchar{'} within an expression is printed in green along with the
form after it, since the combination is an expression that is a
constant. In DrRacket, only the @litchar{'} is colored green. DrRacket
is more precisely correct, because the meaning of @racket[quote] can
vary depending on the context of an expression. In the documentation,
however, we routinely assume that standard bindings are in scope, and
so we paint quoted forms in green for extra clarity.}
在文档中，在一个表达式中的@litchar{'}和后面的表单一起被打印成绿色，因为这个组合是一个表达式，它是一个常量。在DrRacket，只有@litchar{'}被渲染成绿色。DrRacket更加精确校正，因为@racket[quote]的意义可以根据一个表达式的上下文而变化。然而，在文档中，我们经常假定标准绑定是在范围内的，因此我们为了更清晰用绿色绘制引用表。

@;{A @litchar{'} expands to a @racket[quote] form in quite a literal
way. You can see this if you put a @litchar{'} in front of a form that has a
@litchar{'}:}
一个@litchar{'}以字面相当的方式扩展成一个@racket[quote]表。你够明白如果你在一个有一个@litchar{'}的表前面放置一个@litchar{'}的这种情况：

@interaction[
(car ''road)
(eval:alts (car '(@#,racketvalfont{quote} @#,racketvalfont{road})) 'quote)
]

@;{The @litchar{'} abbreviation works in output as well as input. The
@tech{REPL}'s printer recognizes the symbol @racket['quote] as the
first element of a two-element list when printing output, in which
case it uses @racketidfont{'} to print the output:}
@litchar{'}缩写在输出和输入中起作用。在打印输出时，@tech{REPL}打印机识别符号@racket['quote]的两元素列表的第一个元素，在这种情况下，它使用@racketidfont{'}打印输出：

@interaction[
(eval:alts (@#,racketvalfont{quote} (@#,racketvalfont{quote} @#,racketvalfont{road})) ''road)
(eval:alts '(@#,racketvalfont{quote} @#,racketvalfont{road}) ''road)
''road
]

@; FIXME:
@; warning about how "quote" creates constant data, which is subtly
@; different than what "list" creates

@;------------------------------------------------------------------------
@;{@section[#:tag "lists-and-syntax"]{Lists and Racket Syntax}}
@section[#:tag "lists-and-syntax"]{列表和Racket语法}

@;{Now that you know the truth about pairs and lists, and now that you've
seen @racket[quote], you're ready to understand the main way in which
we have been simplifying Racket's true syntax.}
现在你已经知道了关于点对和列表的真相，而且现在你已经明白了@racket[quote]，你已经准备好理解我们一直在简化Racket真实语法的主要方法。

@;{The syntax of Racket is not defined directly in terms of character
streams. Instead, the syntax is determined by two layers:}
Racket的语法并不是直接在字符流中定义的。相反，语法是由两个层确定的：

@itemize[

 @item{
  @;{a @deftech{reader} layer, which turns a sequence of characters
       into lists, symbols, and other constants; and}
    一个@deftech{读取器（reader）}层，将字符序列转换成列表、符号和其它常量；
 }

 @item{
  @;{an @deftech{expander} layer, which processes the lists, symbols,
       and other constants to parse them as an expression.}
    一个@deftech{扩展器（expander）}层，它处理列表、符号和其它常量，并将它们解析为表达式。
 }

]

@;{The rules for printing and reading go together. For example, a list is
printed with parentheses, and reading a pair of parentheses produces a
list. Similarly, a non-list pair is printed with the dot notation, and
a dot on input effectively runs the dot-notation rules in reverse to
obtain a pair.}
打印和读取的规则是互相协调的。例如，一个列表用圆括号打印，读取一对圆括号生成一个列表。类似地，一个非列表点对用点表示法打印，同时在输入上的一个点有效地运行点标记规则从反向得到一个点对。

@;{One consequence of the read layer for expressions is that you can use
the dot notation in expressions that are not quoted forms:}
读取层给表达式的一个推论是你可以在不被引用的表的表达式中使用点标记：

@interaction[
(eval:alts (+ 1 . @#,racket[(2)]) (+ 1 2))
]

@;{This works because @racket[(+ 1 . @#,racket[(2)])] is just another
way of writing @racket[(+ 1 2)]. It is practically never a good idea
to write application expressions using this dot notation; it's just a
consequence of the way Racket's syntax is defined.}
这个操作因为@racket[(+ 1 . @#,racket[(2)])]只是编写@racket[(+ 1 2)]的另一种方法。用这种点表示法编写应用程序表达式实际上从来不是一个好主意，它只是Racket语法定义方法的一个推论。

@;{Normally, @litchar{.} is allowed by the reader only with a
parenthesized sequence, and only before the last element of the
sequence. However, a pair of @litchar{.}s can also appear around a
single element in a parenthesized sequence, as long as the element is
not first or last. Such a pair triggers a reader conversion that moves
the element between @litchar{.}s to the front of the list. The
conversion enables a kind of general infix notation:}
通常，@litchar{.}被仅只带一个括号序列的读取器允许，并且只有在序列的最后一个元素之前。然而，一对@litchar{.}也可以出现在一个括号序列的一个单个元素周围，只要这个元素不是第一个或最后一个。这样的一个点对触发一个阅读器转换，它将@litchar{.}之间的元素移动到列表的前面。这个转换使一种通用的中缀表示法成为可能：

@interaction[
(1 . < . 2)
'(1 . < . 2)
]

@;{This two-dot convention is non-traditional, and it has essentially
nothing to do with the dot notation for non-list pairs. Racket
programmers use the infix convention sparingly---mostly for asymmetric
binary operators such as @racket[<] and @racket[is-a?].}
这两个点转换是非传统的，并且它与非列表点对的点记法基本上没有关系。Racket程序员保守地使用中缀标记——大多用于非对称二元操作符，如@racket[<]和@racket[is-a?]。