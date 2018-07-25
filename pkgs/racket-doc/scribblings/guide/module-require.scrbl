#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-require"]{Imports: @racket[require]}}
@title[#:tag "module-require"]{输入：@racket[require]}

@;{The @racket[require] form imports from another module. A
@racket[require] form can appear within a module, in which case it
introduces bindings from the specified module into importing module. A
@racket[require] form can also appear at the top level, in which case
it both imports bindings and @deftech{instantiates} the specified
module; that is, it evaluates the body definitions and expressions of
the specified module, if they have not been evaluated already.}
@racket[require]表从其它模块输入。一个@racket[require]表可以出现在一个模块中，在这种情况它将来自于指定模块的绑定引入到输入模块中。一个@racket[require]表也可以出现在顶层，在这种情况它既输入绑定也@deftech{实例化（instantiates）}指定的模块；更确切地说，如果主体定义和指定模块的表达式还没有被求值则对其求值。

@;{A single @racket[require] can specify multiple imports at once:}
一个单个的@racket[require]可以同时指定多个输入：

@specform[(require require-spec ...)]{}

@;{Specifying multiple @racket[_require-spec]s in a single
@racket[require] is essentially the same as using multiple
@racket[require]s, each with a single @racket[_require-spec]. The
difference is minor, and confined to the top-level: a single
@racket[require] can import a given identifier at most once, whereas a
separate @racket[require] can replace the bindings of a previous
@racket[require] (both only at the top level, outside of a module).}
在一个单一的@racket[require]表里指定多个@racket[_require-spec]与使用多个每个包含一个单一@racket[_require-spec]的@racket[require]本质上是一样的。其区别很小，且局限于顶层：一个单一的@racket[require]可以最多一次导入一个给定标识符，而一个单独的@racket[require]可以代替一个以前的@racket[require]（都是仅在顶层，在一个模块之外）的绑定。

@;{The allowed shape of a @racket[_require-spec] is defined recursively:}
一个@racket[_require-spec]的允许形态被递归地定义为：

@;------------------------------------------------------------------------
@specspecsubform[module-path]{

@;{In its simplest form, a @racket[_require-spec] is a
@racket[module-path] (as defined in the previous section,
@secref["module-paths"]). In this case, the bindings introduced
by @racket[require] are determined by @racket[provide] declarations
within each module referenced by each @racket[module-path].}
在其最简单的表中，一个@racket[_require-spec]是一个@racket[module-path]（如前一节《@secref["module-paths"]》中定义的）。在这种情况下，被@racket[require]引入的这个绑定通过@racket[provide]声明来确定，申明中的每个模块被各个@racket[module-path]引用。

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
一个@racket[only-in]表限制被一个低级的@racket[require-spec]引入的绑定的设置。此外，@racket[only-in]选择性地重命名每个可保存的绑定：在一个@racket[[orig-id
bind-id]]表里，@racket[orig-id]引用一个被@racket[require-spec]隐含的绑定，并且@racket[bind-id]是这个在输入上下文中将被绑定的名称，以代替@racket[orig-id]。

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
这个表是 @racket[only-in]的补充：它从被@racket[require-spec]指定的集合中排除指定的绑定。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (rename-in)
                 (rename-in require-spec [orig-id bind-id] ...)]{

@;{This form supports renaming like @racket[only-in], but leaving alone
identifiers from @racket[require-spec] that are not mentioned as an
@racket[orig-id].}
这个表支持像@racket[only-in]的重命名，但自不作为一个@racket[orig-id]提交的@racket[require-spec]中分离单独的标识符。
}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-in)
                 (prefix-in prefix-id require-spec)]{

@;{This is a shorthand for renaming, where @racket[prefix-id] is added to
the front of each identifier specified by @racket[require-spec].}
这是一个重命名的简写，这里@racket[prefix-id]被添加到被@racket[require-spec]指定的每个标识符的前面。

}

@;{The @racket[only-in], @racket[except-in], @racket[rename-in], and
@racket[prefix-in] forms can be nested to implement more complex
manipulations of imported bindings. For example,}
除了@racket[only-in]、@racket[except-in]、@racket[rename-in]2和@racket[prefix-in]表可以被嵌套以实现输入绑定的更复杂的操作。例如,

@racketblock[(require (prefix-in m: (except-in 'm ghost)))]

@;{imports all bindings that @racket[m]
exports, except for the @racket[ghost] binding, and with local names
that are prefixed with @racket[m:].}
输入@racket[m]输出的所有绑定，除了@racket[ghost]绑定之外，并带用@racket[m:]前缀的局部名称：

@;{Equivalently, the @racket[prefix-in] could be applied before
@racket[except-in], as long as the omission with @racket[except-in] is
specified using the @racket[m:] prefix:}
等价地，这个@racket[prefix-in]可以被应用在@racket[except-in]之前，只是带@racket[except-in]的省略是被使用@racket[m:]前缀所指定：

@racketblock[(require (except-in (prefix-in m: 'm) m:ghost))]
