#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-require"]{Imports: @racket[require]}}
@title[#:tag "module-require"]{导入：@racket[require]}

@;{The @racket[require] form imports from another module. A
@racket[require] form can appear within a module, in which case it
introduces bindings from the specified module into importing module. A
@racket[require] form can also appear at the top level, in which case
it both imports bindings and @deftech{instantiates} the specified
module; that is, it evaluates the body definitions and expressions of
the specified module, if they have not been evaluated already.}
从另一个模块导入@racket[require]表。一个@racket[require]表可以出现在一个模块中，在这种情况下，它将指定模块的绑定引入到导入的模块中。一个@racket[require]表也可以出现在顶层，在这种情况下，既导入绑定也 @deftech{实例化（instantiates）}指定的模块；即，它对指定模块的主体和表达式求值，如果他们还没有被求值。

@;{A single @racket[require] can specify multiple imports at once:}
一个单个的@racket[require]可以同时指定多个导入：

@specform[(require require-spec ...)]{}

@;{Specifying multiple @racket[_require-spec]s in a single
@racket[require] is essentially the same as using multiple
@racket[require]s, each with a single @racket[_require-spec]. The
difference is minor, and confined to the top-level: a single
@racket[require] can import a given identifier at most once, whereas a
separate @racket[require] can replace the bindings of a previous
@racket[require] (both only at the top level, outside of a module).}
在一个单一的@racket[require]表里指定多个@racket[_require-spec]，从本质上与使用多个@racket[require]，每个单独包含一个单一的@racket[_require-spec]是相同的。区别很小，且局限于顶层：一个独立的@racket[require]可以导入一个给定标识符最多一次，而一个单独的@racket[require]可以代替以前@racket[require]的绑定（都是只局限于顶层，在一个模块之外）。

@;{The allowed shape of a @racket[_require-spec] is defined recursively:}
@racket[_require-spec]的允许形态是递归定义的：

@;------------------------------------------------------------------------
@specspecsubform[module-path]{

@;{In its simplest form, a @racket[_require-spec] is a
@racket[module-path] (as defined in the previous section,
@secref["module-paths"]). In this case, the bindings introduced
by @racket[require] are determined by @racket[provide] declarations
within each module referenced by each @racket[module-path].}
在最简单的形式中，一个@racket[_require-spec]是一个@racket[module-path]（如前一节《@secref["module-paths"]》（Module Paths）中定义的）。在这种情况下，@racket[require]所引入的绑定通过@racket[provide]声明来确定，其中在每个模块通过各个@racket[module-path]引用。

@examples[
(module m racket
  (provide color)
  (define color "blue"))
(module n racket
  (provide size)
  (define size 17))
(require 'm 'n)
(eval:alts (list color size) (eval '(list color size)))
]

}

@;------------------------------------------------------------------------
@specspecsubform/subs[#:literals (only-in)
                      (only-in require-spec id-maybe-renamed ...)
                      ([id-maybe-renamed id
                                         [orig-id bind-id]])]{

@;{An @racket[only-in] form limits the set of bindings that would be introduced
by a base @racket[require-spec]. Also, @racket[only-in] optionally
renames each binding that is preserved: in a @racket[[orig-id
bind-id]] form, the @racket[orig-id] refers to a binding implied by
@racket[require-spec], and @racket[bind-id] is the name that will be
bound in the importing context instead of @racket[orig-id].}
一个@racket[only-in]表限制绑定设置，它将通过@racket[require-spec]引入。此外，@racket[only-in]选择重命名每个绑定，它被保护：在@racket[[orig-id
bind-id]]表里，@racket[orig-id]是指一个被@racket[require-spec]隐含的绑定，并且@racket[bind-id]是这个在导入上下文中将被绑定的名称，以代替@racket[orig-id]。

@examples[
(module m (lib "racket")
  (provide tastes-great?
           less-filling?)
  (define tastes-great? #t)
  (define less-filling? #t))
(require (only-in 'm tastes-great?))
(eval:alts tastes-great? (eval 'tastes-great?))
less-filling?
(require (only-in 'm [less-filling? lite?]))
(eval:alts lite? (eval 'lite?))
]}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-in)
                 (except-in require-spec id ...)]{

@;{This form is the complement of @racket[only-in]: it excludes specific
bindings from the set specified by @racket[require-spec].}
这个表是 @racket[only-in]的补充：它从以@racket[require-spec]指定的集合中排除指定的绑定。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (rename-in)
                 (rename-in require-spec [orig-id bind-id] ...)]{

@;{This form supports renaming like @racket[only-in], but leaving alone
identifiers from @racket[require-spec] that are not mentioned as an
@racket[orig-id].}
这种形式支持类似于@racket[only-in]的重命名，但从@racket[require-spec]中分离单独的标识符，它们没有作为一个@racket[orig-id]提交。
}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-in)
                 (prefix-in prefix-id require-spec)]{

@;{This is a shorthand for renaming, where @racket[prefix-id] is added to
the front of each identifier specified by @racket[require-spec].}
  这是一个重命名的简写，@racket[prefix-id]添加到用@racket[require-spec]指定的每个标识符的前面。

}

@;{The @racket[only-in], @racket[except-in], @racket[rename-in], and
@racket[prefix-in] forms can be nested to implement more complex
manipulations of imported bindings. For example,}
除了@racket[only-in]、@racket[except-in]、@racket[rename-in]2和@racket[prefix-in]表可以嵌套以实现更复杂的导入绑定操作。例如,

@racketblock[(require (prefix-in m: (except-in 'm ghost)))]

@;{imports all bindings that @racket[m]
exports, except for the @racket[ghost] binding, and with local names
that are prefixed with @racket[m:].}
导入@racket[m]输出的所有绑定，除@racket[ghost]绑定之外，并带用@racket[m:]前缀的局部名字：

@;{Equivalently, the @racket[prefix-in] could be applied before
@racket[except-in], as long as the omission with @racket[except-in] is
specified using the @racket[m:] prefix:}
等价地，@racket[prefix-in]可以被应用在@racket[except-in]之前，只是带@racket[except-in]的省略是用@racket[m:]前缀指定：

@racketblock[(require (except-in (prefix-in m: 'm) m:ghost))]
