#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define check-eval (make-base-eval))
@(interaction-eval #:eval check-eval (require (for-syntax racket/base)))

@(define-syntax-rule (racketblock/eval #:eval e body ...)
   (begin
     (interaction-eval #:eval e body) ...
     (racketblock body ...)))

@;{@title[#:tag "proc-macros" #:style 'toc]{General Macro Transformers}}
@title[#:tag "proc-macros" #:style 'toc]{通用宏转化器}

@;{The @racket[define-syntax] form creates a @deftech{transformer
binding} for an identifier, which is a binding that can be used at
compile time while expanding expressions to be evaluated at run time.
The compile-time value associated with a transformer binding can be
anything; if it is a procedure of one argument, then the binding is
used as a macro, and the procedure is the @deftech{macro transformer}.}
@racket[define-syntax]表为标识符创建一个@deftech{转换器绑定（transformer binding）}，这是一个可以在编译时使用的绑定，同时扩展表达式以在运行时进行求值。与转换器绑定相关联的编译时间值可以是任何东西；如果它是一个参数的过程，则绑定用作宏，而过程是 @deftech{宏转换器（macro transformer）}。

@local-table-of-contents[]

@; ----------------------------------------

@;{@section[#:tag "stx-obj"]{Syntax Objects}}
@section[#:tag "stx-obj"]{语法对象}

@;{The input and output of a macro transformer (i.e., source and
replacement forms) are represented as @deftech{syntax objects}. A
syntax object contains symbols, lists, and constant values (such as
numbers) that essentially correspond to the @racket[quote]d form of
the expression. For example, a representation of the expression
@racket[(+ 1 2)] contains the symbol @racket['+] and the numbers
@racket[1] and @racket[2], all in a list. In addition to this quoted
content, a syntax object associates source-location and
lexical-binding information with each part of the form. The
source-location information is used when reporting syntax errors (for
example), and the lexical-binding information allows the macro system
to maintain lexical scope. To accommodate this extra information, the
represention of the expression @racket[(+ 1 2)] is not merely
@racket['(+ 1 2)], but a packaging of @racket['(+ 1 2)] into a syntax
object.}
宏转换器（即源和替换表）的输入和输出被表示为@deftech{语法对象（syntax object）}。语法对象包含符号、列表和常量值（如数字），它们基本上与表达式的@racket[quote]（引用）表相对应。例如，表达式描述为@racket[(+ 1 2)]包含符号@racket['+]和数字@racket[1]和@racket[2]，都在列表中。除了引用的内容之外，语法对象还将源位置和词汇绑定信息与表的每个部分关联起来。在报告语法错误时使用源位置信息（例如），词汇绑定信息允许宏系统维护词法范围。为了适应这种额外的信息，表达式描述为@racket[(+ 1 2)]不仅是@racket['(+ 1 2)]，但@racket['(+ 1 2)]的封装成为了语法对象。

@;{To create a literal syntax object, use the @racket[syntax] form:}
要创建文字语法对象，请使用@racket[syntax]表：

@interaction[
(eval:alts (#,(racket syntax) (+ 1 2)) (syntax (+ 1 2)))
]

@;{In the same way that @litchar{'} abbreviates @racket[quote],
@litchar{#'} abbreviates @racket[syntax]:}
在同样的方式，@litchar{'}省略了@racket[quote]，@litchar{#'}省略了@racket[syntax]：

@interaction[
#'(+ 1 2)
]

@;{A syntax object that contains just a symbol is an @deftech{identifier
syntax object}. Racket provides some additional operations specific to
identifier syntax objects, including the @racket[identifier?]
operation to detect identifiers. Most notably,
@racket[free-identifier=?]  determines whether two identifiers refer
to the same binding:}
只包含符号的语法对象是@deftech{标识符语法对象（identifier syntax object）}。它提供了一些特定于标识符语法对象的附加操作，包括@racket[identifier?]操作以检查操作符。最值得注意的是，@racket[free-identifier=?]确定两个标识符是否引用相同的绑定：

@interaction[
(identifier? #'car)
(identifier? #'(+ 1 2))
(free-identifier=? #'car #'cdr)
(free-identifier=? #'car #'car)
(require (only-in racket/base [car also-car]))
(free-identifier=? #'car #'also-car)
]

@;{To see the lists, symbols, numbers, @|etc| within a syntax object, use
@racket[syntax->datum]:}
要在语法对象中看到列表、符号、数字、@|etc|等等，请使用@racket[syntax->datum]：

@interaction[
(syntax->datum #'(+ 1 2))
]

@;{The @racket[syntax-e] function is similar to @racket[syntax->datum],
but it unwraps a single layer of source-location and lexical-context
information, leaving sub-forms that have their own information wrapped
as syntax objects:}
@racket[syntax-e]函数类似于@racket[syntax->datum]，但它打开了一个单层的源位置和词汇上下文信息，离开有它们自己的信息作为语法的对象子表：

@interaction[
(syntax-e #'(+ 1 2))
]

@;{The @racket[syntax-e] function always leaves syntax-object wrappers
around sub-forms that are represented via symbols, numbers, and other
literal values. The only time it unwraps extra sub-forms is when
unwrapping a pair, in which case the @racket[cdr] of the pair may be
recursively unwrapped, depending on how the syntax object was
constructed.}
@racket[syntax-e]函数总是放弃语法对象包装器，它包围着子表，子表表示为通过符号、数值，和其它文本值。唯一的一次额外的子表打开时展开一个配对，在这种情况下，配对的@racket[cdr]可以根据语法构建的对象递归地展开。

@;{The opposite of @racket[syntax->datum] is, of course,
@racket[datum->syntax].  In addition to a datum like @racket['(+ 1
2)], @racket[datum->syntax] needs an existing syntax object to donate
its lexical context, and optionally another syntax object to donate
its source location:}
当然，与@racket[syntax->datum]相对立的是@racket[datum->syntax]。除了像@racket['(+ 1
2)]这样的数据外，@racket[datum->syntax]还需要一个现有的语法对象来贡献它的词法上下文，并且可以选择另一个语法对象来贡献它的源位置：

@interaction[
(datum->syntax #'lex
               '(+ 1 2)
               #'srcloc)
]

@;{In the above example, the lexical context of @racket[#'lex] is used
for the new syntax object, while the source location of
@racket[#'srcloc] is used.}
在上面的例子中，对@racket[#'lex]的词法上下文是用于新的语法对象，而@racket[#'srcloc]源位置被使用。

@;{When the second (i.e., the ``datum'') argument to
@racket[datum->syntax] includes syntax objects, those syntax objects
are preserved intact in the result. That is, deconstructing the result
with @racket[syntax-e] eventually produces the syntax objects that
were given to @racket[datum->syntax].}
当@racket[datum->syntax]的第二个（即，“datum”）参数包含语法对象时，这些语法对象将原封不动地保存在结果中。那就是，用@racket[syntax-e]解构的结果最终产生了给予@racket[datum->syntax]的这个语法对象。

@; ----------------------------------------

@;{@section[#:tag "macro-transformers"]{Macro Transformer Procedures}}
@section[#:tag "macro-transformers"]{宏转化器程序}

@;{Any procedure of one argument can be a @tech{macro transformer}.  As
it turns out, the @racket[syntax-rules] form is a macro that expands
to a procedure form. For example, if you evaluate a
@racket[syntax-rules] form directly (instead of placing on the
right-hand of a @racket[define-syntax] form), the result is a
procedure:}
任何一个参数的过程都可以是一个@tech{宏转换器（macro transformer）}。事实证明，@racket[syntax-rules]（语法规则）表是一个扩展为过程表的宏。例如，如果直接求值@racket[syntax-rules]表（而不是放在@racket[define-syntax]表的右侧），结果就是一个过程：


@interaction[
(syntax-rules () [(nothing) something])
]

@;{Instead of using @racket[syntax-rules], you can write your own macro
transformer procedure directly using @racket[lambda]. The argument to
the procedure is a @tech{syntax object} that represents the source form, and the
result of the procedure must be a @tech{syntax object} that represents the
replacement form:}
可以使用@racket[lambda]直接编写自己的宏转换器过程，而不是使用@racket[syntax-rules]。对过程的参数是表示源表的@tech{语法对象（syntax object）}，过程的结果必须是表示替换表的@tech{语法对象（syntax object）}：

@interaction[
#:eval check-eval
(define-syntax self-as-string
  (lambda (stx)
    (datum->syntax stx 
                   (format "~s" (syntax->datum stx)))))

(self-as-string (+ 1 2))
]

@;{The source form passed to a macro transformer represents an
expression in which its identifier is used in an application position
(i.e., after a parenthesis that starts an expression), or it
represents the identifier by itself if it is used as an expression
position and not in an application position.@margin-note*{The procedure produced by
@racket[syntax-rules] raises a syntax error if its argument
corresponds to a use of the identifier by itself, which is why
@racket[syntax-rules] does not implement an @tech{identifier macro}.}}
传递给宏转换器的源表表示一个表达式，其中在应用程序位置（即在启动表达式的括号之后）使用其标识符，或者如果它被用作表达式位置而不是应用程序位置，则它本身代表标识符。@margin-note*{如果程序的参数与标识符本身的使用相对应，则该程序将通过@racket[syntax-rules]产生一个语法错误，这就是为什么@racket[syntax-rules]不实现一个@tech{标识符宏（identifier macro）}的原因。}

@interaction[
#:eval check-eval
(self-as-string (+ 1 2))
self-as-string
]

@;{The @racket[define-syntax] form supports the same shortcut
syntax for functions as @racket[define], so that the following @racket[self-as-string]
definition is equivalent to the one that uses @racket[lambda]
explicitly:}
@racket[define-syntax]表支持与@racket[define]的函数一样的快捷语法，因此下面的@racket[self-as-string]定义等同于显式使用@racket[lambda]的那个定义：

@interaction[
#:eval check-eval
(define-syntax (self-as-string stx)
  (datum->syntax stx 
                 (format "~s" (syntax->datum stx))))

(self-as-string (+ 1 2))
]

@; ----------------------------------------

@;{@section[#:tag "syntax-case"]{Mixing Patterns and Expressions: @racket[syntax-case]}}
@section[#:tag "syntax-case"]{混合模式和表达式：@racket[syntax-case]}

@;{The procedure generated by @racket[syntax-rules] internally uses
@racket[syntax-e] to deconstruct the given syntax object, and it uses
@racket[datum->syntax] to construct the result. The
@racket[syntax-rules] form doesn't provide a way to escape from
pattern-matching and template-construction mode into an arbitrary
Racket expression.}
通过@racket[syntax-rules]生成的程序在内部使用@racket[syntax-e]解构了语法对象，并使用@racket[datum->syntax]以构造结果。@racket[syntax-rules]表没有提供一种方法从模式匹配和模板构建模式中跳转到任意的Racket表达式中。

@;{The @racket[syntax-case] form lets you mix pattern matching, template
construction, and arbitrary expressions:}
@racket[syntax-case]表允许混合模式匹配、模板构造和任意表达式：

@specform[(syntax-case stx-expr (literal-id ...)
            [pattern expr]
            ...)]

@;{Unlike @racket[syntax-rules], the @racket[syntax-case] form does not
produce a procedure. Instead, it starts with a @racket[_stx-expr]
expression that determines the syntax object to match against the
@racket[_pattern]s. Also, each @racket[syntax-case] clause has a
@racket[_pattern] and @racket[_expr], instead of a @racket[_pattern]
and @racket[_template]. Within an @racket[_expr], the @racket[syntax]
form---usually abbreviated with @litchar{#'}---shifts into
template-construction mode; if the @racket[_expr] of a clause starts
with @litchar{#'}, then we have something like a @racket[syntax-rules]
form:}
与@racket[syntax-rules]不同，@racket[syntax-case]表不产生过程。相反，它从一个@racket[_stx-expr]表达式决定的语法对象来匹配@racket[_pattern]。另外，每个@racket[syntax-case]有一个pattern和一个expr，而不是一个@racket[_pattern]和@racket[_template]。在一个@racket[_expr]里，@racket[syntax]表——通常用@litchar{#'}缩写——进入模板构造方式；如果一个从句的@racket[_expr]以@litchar{#'}开始，那么我们就会获得一些像@racket[syntax-rules]的表：

@interaction[
(syntax->datum
 (syntax-case #'(+ 1 2) ()
  [(op n1 n2) #'(- n1 n2)]))
]

@;{We could write the @racket[swap] macro using @racket[syntax-case]
instead of @racket[define-syntax-rule] or @racket[syntax-rules]:}
我们可以使用@racket[syntax-case]来编写@racket[swap]宏，以代替@racket[define-syntax-rule]或@racket[syntax-rules]：

@racketblock[
(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) #'(let ([tmp x])
                    (set! x y)
                    (set! y tmp))]))
]

@;{One advantage of using @racket[syntax-case] is that we can provide
better error reporting for @racket[swap]. For example, with the
@racket[define-syntax-rule] definition of @racket[swap], then
@racket[(swap x 2)] produces a syntax error in terms of @racket[set!],
because @racket[2] is not an identifier. We can refine our
@racket[syntax-case] implementation of @racket[swap] to explicitly
check the sub-forms:}
使用@racket[syntax-case]的一个优点是，我们可以为@racket[swap]提供更好的错误报告。例如，使用@racket[swap]的@racket[define-syntax-rule]定义，然后@racket[(swap x 2)]在@racket[set!]条件中产生语法错误！，因为@racket[2]不是一个标识符。我们可以改进@racket[swap]的@racket[syntax-case]实现来显式检查子表：

@racketblock[
(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) 
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             (if (identifier? #'x) 
                                 #'y 
                                 #'x)))]))
]

@;{With this definition, @racket[(swap x 2)] provides a syntax error
originating from @racket[swap] instead of @racket[set!].}
通过这个定义，@racket[(swap x 2)]提供了一个源自@racket[swap]而不是@racket[set!]的语法错误。

@;{In the above definition of @racket[swap], @racket[#'x] and
@racket[#'y] are templates, even though they are not used as the
result of the macro transformer. This example illustrates how
templates can be used to access pieces of the input syntax, in this
case for checking the form of the pieces. Also, the match for
@racket[#'x] or @racket[#'y] is used in the call to
@racket[raise-syntax-error], so that the syntax-error message can
point directly to the source location of the non-identifier.}
在互换上述@racket[swap]的定义里，@racket[#'x]和@racket[#'y]是模板，即使它们不是作为宏转换器的结果。这个例子说明了如何使用模板来访问输入语法的片段，在这种情况下可以检查碎片的表。同时，在对@racket[raise-syntax-error]的调用中对@racket[#'x]或@racket[#'y]的匹配被使用，所以语法错误信息可以直接指到非标识符的源位置。

@; ----------------------------------------

@;{@section[#:tag "with-syntax"]{@racket[with-syntax] and @racket[generate-temporaries]}}
@section[#:tag "with-syntax"]{@racket[with-syntax]和@racket[generate-temporaries]}

@;{Since @racket[syntax-case] lets us compute with arbitrary Racket
expressions, we can more simply solve a problem that we had in
writing @racket[define-for-cbr] (see
@secref["pattern-macro-example"]), where we needed to generate a
set of names based on a sequence @racket[id ...]:}
由于@racket[syntax-case]允许我们用任意的Racket表达式进行计算，我们可以更简单地解决我们在编写@racket[define-for-cbr]（参见@secref["pattern-macro-example"]）中的一个问题，在这里我们需要根据序列@racket[id ...]生成一组名称：

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     ....
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body) ....]))
]

@;{In place of the @racket[....]s above, we need to bind @racket[get
...] and @racket[put ...] to lists of generated identifiers. We
cannot use @racket[let] to bind @racket[get] and @racket[put],
because we need bindings that count as pattern variables, instead
of normal local variables. The @racket[with-syntax] form lets us
bind pattern variables:}
代替上面的@racket[....]我们需要绑定@racket[get
...]和@racket[put ...]到生成标识符的列表。我们不能使用@racket[let]绑定@racket[get]和@racket[put]，因为我们需要绑定那个计数作为模式变量，而不是普通的局部变量。@racket[with-syntax]表允许我们绑定模式变量：

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     (with-syntax ([(get ...) ....]
                   [(put ...) ....])
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body))]))
]

@;{Now we need an expression in place of @racket[....] that
generates as many identifiers as there are @racket[id] matches in
the original pattern. Since this is a common task, Racket
provides a helper function, @racket[generate-temporaries], that
takes a sequence of identifiers and returns a sequence of
generated identifiers:}
现在我们需要一个表达代替@racket[....]它生成与在原始模式中匹配@racket[id]一样多的标识符。由于这是一个常见的任务，Racket提供了一个辅助函数，@racket[generate-temporaries]，以一系列的标识符并返回一个序列生成的标识符：

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     (with-syntax ([(get ...) (generate-temporaries #'(id ...))]
                   [(put ...) (generate-temporaries #'(id ...))])
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body))]))
]

@;{This way of generating identifiers is normally easier to think
about than tricking the macro expander into generating names with
purely pattern-based macros.}
这种方式产生的标识符通常比欺骗宏扩展产生的纯粹基于模式的宏的名字更容易理解。

@;{In general, the left-hand side of a @racket[with-syntax]
binding is a pattern, just like in @racket[syntax-case]. In fact,
a @racket[with-syntax] form is just a @racket[syntax-case] form
turned partially inside-out.}
一般来说，一个@racket[with-syntax]绑定左边是一个模式，就像在@racket[syntax-case]中一样。事实上，一个@racket[with-syntax]表只是一个@racket[syntax-case]的部分转换。

@; ----------------------------------------

@;{@section[#:tag "stx-phases"]{Compile and Run-Time Phases}}
@section[#:tag "stx-phases"]{编译和运行时阶段}

@;{As sets of macros get more complicated, you might want to write
your own helper functions, like
@racket[generate-temporaries]. For example, to provide good
syntax error messsages, @racket[swap], @racket[rotate], and
@racket[define-cbr] all should check that certain sub-forms in
the source form are identifiers. We could use a
@racket[check-ids] function to perform this checking everywhere:}
随着宏集合越来越多复杂，你可能要写你自己的辅助函数，如@racket[generate-temporaries]。例如，提供良好的语法错误信息，@racket[swap]、@racket[rotate]和@racket[define-cbr]都应该检查在源表中的某一个子表是标识符。我们可以使用@racket[check-ids]函数在任何地方执行此检查：

@racketblock/eval[
#:eval check-eval
(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))

(define-syntax (rotate stx)
  (syntax-case stx ()
    [(rotate a c ...)
     (begin
       (check-ids stx #'(a c ...))
       #'(shift-to (c ... a) (a c ...)))]))
]

@;{The @racket[check-ids] function can use the @racket[syntax->list]
function to convert a syntax-object wrapping a list into a list
of syntax objects:}
@racket[check-ids]函数可以使用@racket[syntax->list]函数将一个语法对象转换成一个语法对象列表：

@racketblock[
(define (check-ids stx forms)
  (for-each
   (lambda (form)
     (unless (identifier? form)
       (raise-syntax-error #f
                           "not an identifier"
                           stx
                           form)))
   (syntax->list forms)))
]

@;{If you define @racket[swap] and @racket[check-ids] in this way,
however, it doesn't work:}
然而，如果以这种方式定义@racket[swap]和@racket[check-ids]，则它不会运行：

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b))
]

@;{The problem is that @racket[check-ids] is defined as a run-time
expression, but @racket[swap] is trying to use it at compile time. In
interactive mode, compile time and run time are interleaved, but they
are not interleaved within the body of a module, and they are not
interleaved across modules that are compiled ahead-of-time. To help
make all of these modes treat code consistently, Racket separates the
binding spaces for different phases.}
问题是@racket[check-ids]被定义为一个运行时表达式，但是@racket[swap]试图在编译时使用它。在交互模式中，编译时和运行时是交错的，但它们不是在模块的主体内交错的，它们不在预编译的模块之间进行交叉。为了帮助所有这些模式一致地对待代码，Racket将不同阶段的绑定空间分隔开来。

@;{To define a @racket[check-ids] function that can be referenced at
compile time, use @racket[begin-for-syntax]:}
要定义可在编译时引用的@racket[check-ids]函数，使用@racket[begin-for-syntax]：

@racketblock/eval[
#:eval check-eval
(begin-for-syntax
  (define (check-ids stx forms)
    (for-each
     (lambda (form)
       (unless (identifier? form)
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             form)))
     (syntax->list forms))))
]

@;{With this for-syntax definition, then @racket[swap] works:}
使用此语法定义，那么@racket[swap]就会运行：

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b) (list a b))
(swap a 1)
]

@;{When organizing a program into modules, you may want to put helper
functions in one module to be used by macros that reside on other
modules. In that case, you can write the helper function using
@racket[define]:}
当将程序组织成模块时，你也许希望将辅助函数放在一个模块中，以供驻留在其它模块上的宏使用。在这种情况下，你可以使用@racket[define]编写辅助函数：

@racketmod[#:file
"utils.rkt"
racket

(provide check-ids)

(define (check-ids stx forms)
  (for-each
   (lambda (form)
     (unless (identifier? form)
       (raise-syntax-error #f
                           "not an identifier"
                           stx
                           form)))
   (syntax->list forms)))
]

@;{Then, in the module that implements macros, import the helper function
using @racket[(require (for-syntax "utils.rkt"))] instead of
@racket[(require "utils.rkt")]:}
然后，在实现宏模块中，使用@racket[(require (for-syntax "utils.rkt"))]代替@racket[(require "utils.rkt")]导入辅助函数：

@racketmod[
racket

(require (for-syntax "utils.rkt"))

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))
]

@;{Since modules are separately compiled and cannot have circular
dependencies, the @filepath["utils.rkt"] module's run-time body can be
compiled before the compiling the module that implements
@racket[swap].  Thus, the run-time definitions in
@filepath["utils.rkt"] can be used to implement @racket[swap], as long
as they are explicitly shifted into compile time by @racket[(require
(for-syntax ....))].}
因为模块分别编译并没有循环依赖，@filepath["utils.rkt"]模块的运行时本体可以在编译的模块实现@racket[swap]编译。因此，在@filepath["utils.rkt"]可以用来实现@racket[swap]，只要他们通过@racket[(require
(for-syntax ....))]明确转移到编译时间。

@;{The @racketmodname[racket] module provides @racket[syntax-case],
@racket[generate-temporaries], @racket[lambda], @racket[if], and more
for use in both the run-time and compile-time phases. That is why we
can use @racket[syntax-case] in the @exec{racket} @tech{REPL} both
directly and in the right-hand side of a @racket[define-syntax]
form.}
@racketmodname[racket]模块提供了@racket[syntax-case]、@racket[generate-temporaries]、@racket[lambda]、@racket[if]以及更多以用在运行时阶段和编译时阶段。这就是为什么我们既可以直接在@exec{racket}的@tech{REPL}中也可以在一个@racket[define-syntax]表的右端使用@racket[syntax-case]的原因。

@;{The @racketmodname[racket/base] module, in contrast, exports those
bindings only in the run-time phase. If you change the module above
that defines @racket[swap] so that it uses the
@racketmodname[racket/base] language instead of
@racketmodname[racket], then it no longer works. Adding
@racket[(require (for-syntax racket/base))] imports
@racket[syntax-case] and more into the compile-time phase, so that the
module works again.}
与此相反，@racketmodname[racket/base]模块只在运行时阶段导出这些绑定。如果你改变上面定义@racket[swap]的模块，使它使用@racketmodname[racket/base]语言，而不是@racketmodname[racket]，那么它不再运行。添加@racket[(require (for-syntax racket/base))]导入@racket[syntax-case]和更多进入编译时阶段，使模块再次工作。

@;{Suppose that @racket[define-syntax] is used to define a local macro in
the right-hand side of a @racket[define-syntax] form. In that case,
the right-hand side of the inner @racket[define-syntax] is in the
@deftech{meta-compile phase level}, also known as @deftech{phase level
2}. To import @racket[syntax-case] into that phase level, you would
have to use @racket[(require (for-syntax (for-syntax racket/base)))]
or, equivalently, @racket[(require (for-meta 2 racket/base))].  For example,}
假设@racket[define-syntax]用于在@racket[define-syntax]表的右端定义一个局部宏。在这种情况下，内部@racket[define-syntax]的右端位于@deftech{元编译阶段等级（meta-compile phase level）}，也称为@deftech{阶段等级2（phase level 2）}。要将@racket[syntax-case]导入到该阶段等级，你必须使用@racket[(require (for-syntax (for-syntax racket/base)))]，或者等效地使用@racket[(require (for-meta 2 racket/base))]。例如,

@codeblock|{
#lang racket/base
(require  ;; This provides the bindings for the definition
          ;; of shell-game.
          (for-syntax racket/base)
 
          ;; And this for the definition of
          ;; swap.
          (for-syntax (for-syntax racket/base)))

(define-syntax (shell-game stx)

  (define-syntax (swap stx)
    (syntax-case stx ()
      [(_ a b)
       #'(let ([tmp a])
           (set! a b)
           (set! b tmp))]))
  
  (syntax-case stx ()
    [(_ a b c)
     (let ([a #'a] [b #'b] [c #'c])
       (when (= 0 (random 2)) (swap a b))
       (when (= 0 (random 2)) (swap b c))
       (when (= 0 (random 2)) (swap a c))
       #`(list #,a #,b #,c))]))

(shell-game 3 4 5)
(shell-game 3 4 5)
(shell-game 3 4 5)
}|

@;{Negative phase levels also exist. If a macro uses a helper function
that is imported @racket[for-syntax], and if the helper function
returns syntax-object constants generated by @racket[syntax], then
identifiers in the syntax will need bindings at @deftech{phase level
-1}, also known as the @deftech{template phase level}, to have any
binding at the run-time phase level relative to the module that
defines the macro.}
反向阶段等级也存在。如果一个宏使用了一个导入@racket[for-syntax]的辅助函数，如果辅助函数返回由@racket[syntax]生成的语法对象常量，那么语法中的标识符将需要在@deftech{阶段等级-1（phase level -1）}，也称为@deftech{模板阶段等级（template phase level）}，以便在运行时阶段等级相对于定义宏的模块有任何绑定。

@;{For instance, the @racket[swap-stx] helper function in the example below
is not a syntax transformer---it's just an ordinary function---but it 
produces syntax objects that get spliced into the result of 
@racket[shell-game]. Therefore, its containing @racket[helper] submodule 
needs to be imported at @racket[shell-game]'s phase 1 with 
@racket[(require (for-syntax 'helper))]. }
例如，在下面的例子中没有语法变换器的@racket[swap-stx]的辅助函数——它只是一个普通的函数——但它产生的语法对象得到拼接成@racket[shell-game]的结果。因此，其包含的辅助模块需要在@racket[shell-game]阶段1用@racket[(require (for-syntax 'helper))]导入。

@;{But from the perspective of @racket[swap-stx], its results will ultimately
be evaluated at phase level -1, when the syntax 
returned by @racket[shell-game] is evaluated. In other words, a negative phase
level is a positive phase level from the opposite direction: 
@racket[shell-game]'s phase 1 is @racket[swap-stx]'s phase 0, so 
@racket[shell-game]'s phase 0 is @racket[swap-stx]'s phase -1. 
And that's why this example won't work---the @racket['helper] submodule 
has no bindings at phase -1.}
但从@racket[swap-stx]的角度，当被@racket[shell-game]返回的语法求值时，其结果最终在阶段1求值。换句话说，一个负向阶段等级是一个从正方向来看的相反的方阶段等级：@racket[shell-game]的阶段1是@racket[swap-stx]的阶段0，所以@racket[shell-game]的阶段0是@racket[swap-stx]的阶段-1。这就是为什么这个例子不运行——@racket['helper]子模块在阶段-1没有绑定。

@codeblock|{
#lang racket/base
(require (for-syntax racket/base))

(module helper racket/base
  (provide swap-stx)
  (define (swap-stx a-stx b-stx)
    #`(let ([tmp #,a-stx])
          (set! #,a-stx #,b-stx)
          (set! #,b-stx tmp))))

(require (for-syntax 'helper))

(define-syntax (shell-game stx)
  (syntax-case stx ()
    [(_ a b c)
     #`(begin
         #,(swap-stx #'a #'b)
         #,(swap-stx #'b #'c)
         #,(swap-stx #'a #'c)
         (list a b c))]))

(define x 3)
(define y 4)
(define z 5)
(shell-game x y z)
}|

@;{To repair this example, we add @racket[(require (for-template racket/base))]
to the @racket['helper] submodule.}
修复这个例子，我们添加@racket[(require (for-template racket/base))]到@racket['helper]子模块。

@codeblock|{
#lang racket/base
(require (for-syntax racket/base))

(module helper racket/base
  (require (for-template racket/base)) ; binds `let` and `set!` at phase -1
  (provide swap-stx)
  (define (swap-stx a-stx b-stx)
    #`(let ([tmp #,a-stx])
          (set! #,a-stx #,b-stx)
          (set! #,b-stx tmp))))

(require (for-syntax 'helper))

(define-syntax (shell-game stx)
  (syntax-case stx ()
    [(_ a b c)
     #`(begin
         #,(swap-stx #'a #'b)
         #,(swap-stx #'b #'c)
         #,(swap-stx #'a #'c)
         (list a b c))]))

(define x 3)
(define y 4)
(define z 5)
(shell-game x y z)
(shell-game x y z)
(shell-game x y z)}|



@; ----------------------------------------

@include-section["phases.scrbl"]

@; ----------------------------------------

@include-section["syntax-taints.scrbl"]

@close-eval[check-eval]
