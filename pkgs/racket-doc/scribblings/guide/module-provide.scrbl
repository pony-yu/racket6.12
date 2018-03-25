#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-provide"]{Exports: @racket[provide]}}
@title[#:tag "module-provide"]{导出：@racket[provide]}

@;{By default, all of a module's definitions are private to the
module. The @racket[provide] form specifies definitions to be made
available where the module is @racket[require]d.}
默认情况下，一个模块的所有定义对模块都是私有的。@racket[provide]表指定定义，以使在模块@racket[require]的地方可获取。

@specform[(provide provide-spec ...)]{}

@;{A @racket[provide] form can only appear at module level (i.e., in the
immediate body of a @racket[module]).  Specifying multiple
@racket[_provide-spec]s in a single @racket[provide] is exactly the
same as using multiple @racket[provide]s each with a single
@racket[_provide-spec].}
一个@racket[provide]表只能出现在模块级（即一个@racket[module]的当前主体）中。在一个单一的@racket[provide]中指定多个@racket[_provide-spec]，那和使用多个@racket[provide]，其每一个有单一的@racket[_provide-spec]，明显是一样的。

@;{Each identifier can be exported at most once from a module across all
@racket[provide]s within the module. More precisely, the external name
for each export must be distinct; the same internal binding can be
exported multiple times with different external names.}
每个标识符最多可以从模块中导出一次，遍及模块中的所有@racket[provide]。更确切地说，每个导出的外部名称必须是不同的；相同的内部绑定可以用不同的外部名称多次导出。

@;{The allowed shape of a @racket[_provide-spec] is defined recursively:}
允许的@racket[_provide-spec]形式是递归定义的：

@;------------------------------------------------------------------------
@specspecsubform[identifier]{

@;{In its simplest form, a @racket[_provide-spec] indicates a binding
within its module to be exported. The binding can be from either a
local definition, or from an import.}
  在最简单的形式中，@racket[_provide-spec]标明一个绑定，它在被导出的模块内。绑定可以来自于局部定义，也可以来自于一个导入。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(rename-out)
                 (rename-out [orig-id export-id] ...)]{

@;{A @racket[rename-out] form is similar to just specifying an identifier,
but the exported binding @racket[orig-id] is given a different name,
@racket[export-id], to importing modules.}
  一个@racket[rename-out]表类似于只指定一个标识符，但这个导出绑定@racket[orig-id]是给定一个不同的名称，@racket[export-id]，给导入模块。

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(struct-out)
                 (struct-out struct-id)]{

@;{A @racket[struct-out] form exports the bindings created by
@racket[(struct struct-id ....)].}
  一个@racket[struct-out]表导出由@racket[(struct struct-id ....)]创建的绑定。

@guideother{
  @;{See @secref["define-struct"] for information on
@racket[define-struct].}
    参见@secref["define-struct"]以获取@racket[define-struct]的更多信息。
 }

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-defined-out)
                 (all-defined-out)]{

@;{The @racket[all-defined-out] shorthand exports all bindings that are
defined within the exporting module (as opposed to imported).}
  @racket[all-defined-out]简写导出所有的绑定，其定义在导出模块中（与导入相反）。

@;{Use of the @racket[all-defined-out] shorthand is generally
discouraged, because it makes less clear the actual exports for a
module, and because Racket programmers get into the habit of
thinking that definitions can be added freely to a module without
affecting its public interface (which is not the case when
@racket[all-defined-out] is used).}
@racket[all-defined-out]简写的使用通常是被阻止的，因为它不太清楚模块的实际导出，而且因为Racket程序员习惯于认为可以自由地将定义添加到模块，而不影响其公共接口（在@racket[all-defined-out]被使用时候都不是这样）。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-from-out)
                 (all-from-out module-path)]{

@;{The @racket[all-from-out] shorthand exports all bindings in the module
that were imported using a @racket[_require-spec] that is based on
@racket[module-path].}
@racket[all-from-out]简写输出模块中的所有绑定，该模块使用一个基于@racket[module-path]的@racket[_require-spec]导入。

@;{Although different @racket[module-path]s could refer to the same
file-based module, re-exporting with @racket[all-from-out] is based
specifically on the @racket[module-path] reference, and not the module
that is actually referenced.}
尽管不同的@racket[module-path]可以引用同一个基于文件的模块，但是带@racket[all-from-out]的重复导出是明确基于@racket[module-path]引用，而不是实际引用的模块。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-out)
                 (except-out provide-spec id ...)]{

@;{Like @racket[provide-spec], but omitting the export of each
@racket[id], where @racket[id] is the external name of the binding to
omit.}
就像@racket[provide-spec]，但省略每个@racket[id]的导出，其中@racket[id]是要省略的绑定的外部名称。

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-out)
                 (prefix-out prefix-id provide-spec)]{

@;{Like @racket[provide-spec], but adding @racket[prefix-id] to the
beginning of the external name for each exported binding.}
就像@racket[provide-spec]，但为每个导出的绑定添加@racket[prefix-id]到外部名称的开头。

}

