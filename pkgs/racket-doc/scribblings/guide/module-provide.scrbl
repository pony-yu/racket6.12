#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "module-provide"]{Exports: @racket[provide]}}
@title[#:tag "module-provide"]{输出：@racket[provide]}

@;{By default, all of a module's definitions are private to the
module. The @racket[provide] form specifies definitions to be made
available where the module is @racket[require]d.}
默认情况下，一个模块的所有定义对这个模块是私有的。@racket[provide]表指定定义以造成在模块被@racket[require]的地方可获取。

@specform[(provide provide-spec ...)]{}

@;{A @racket[provide] form can only appear at module level (i.e., in the
immediate body of a @racket[module]).  Specifying multiple
@racket[_provide-spec]s in a single @racket[provide] is exactly the
same as using multiple @racket[provide]s each with a single
@racket[_provide-spec].}
一个@racket[provide]表只能出现在模块级（即在一个@racket[module]的当前主体中）。在一个单一的@racket[provide]中指定多个@racket[_provide-spec]和使用每个带有一个单一的@racket[_provide-spec]的多个@racket[provide]明显是一样的。

@;{Each identifier can be exported at most once from a module across all
@racket[provide]s within the module. More precisely, the external name
for each export must be distinct; the same internal binding can be
exported multiple times with different external names.}
每个标识符遍及这个模块中的所有@racket[provide]最多可以从一个模块中输出一次。更确切地说，用于每个输出的外部名称必须是不同的；相同的内部绑定可以用不同的外部名称输出多次。

@;{The allowed shape of a @racket[_provide-spec] is defined recursively:}
一个@racket[_provide-spec]的允许形态被递归定义为：

@;------------------------------------------------------------------------
@specspecsubform[identifier]{

@;{In its simplest form, a @racket[_provide-spec] indicates a binding
within its module to be exported. The binding can be from either a
local definition, or from an import.}  在最简单表中，一个@racket[_provide-spec]标明一个在被输出的模块内的绑定。这个绑定既可以来自于一个局部定义，也可以来自于一个输入。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals(rename-out)
                 (rename-out [orig-id export-id] ...)]{

@;{A @racket[rename-out] form is similar to just specifying an identifier,
but the exported binding @racket[orig-id] is given a different name,
@racket[export-id], to importing modules.}
  一个@racket[rename-out]表类似于只指定一个标识符，但这个输出绑定@racket[orig-id]是给定一个不同的名称，@racket[export-id]，到输入模块。

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals(struct-out)
                 (struct-out struct-id)]{

@;{A @racket[struct-out] form exports the bindings created by
@racket[(struct struct-id ....)].}
  一个@racket[struct-out]表输出被@racket[(struct struct-id ....)]创建的绑定。

@guideother{
  @;{See @secref["define-struct"] for information on
@racket[define-struct].}
    参见《@secref["define-struct"]》以获取@racket[define-struct]的更多信息。
 }

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-defined-out)
                 (all-defined-out)]{

@;{The @racket[all-defined-out] shorthand exports all bindings that are
defined within the exporting module (as opposed to imported).}
  @racket[all-defined-out]简写输出所有在输出模块中（与输入相反）被定义的绑定。

@;{Use of the @racket[all-defined-out] shorthand is generally
discouraged, because it makes less clear the actual exports for a
module, and because Racket programmers get into the habit of
thinking that definitions can be added freely to a module without
affecting its public interface (which is not the case when
@racket[all-defined-out] is used).}
@racket[all-defined-out]简写的使用通常被阻止，因为它导致对一个模块的实际导出不太明确，并且因为Racket程序员习惯于认为定义可以自由地添加到一个模块而不影响其公共接口（在@racket[all-defined-out]被使用时候不是这样）。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (all-from-out)
                 (all-from-out module-path)]{

@;{The @racket[all-from-out] shorthand exports all bindings in the module
that were imported using a @racket[_require-spec] that is based on
@racket[module-path].}
@racket[all-from-out]简写输出在使用一个基于@racket[module-path]的@racket[_require-spec]输入的模块中的所有绑定。

@;{Although different @racket[module-path]s could refer to the same
file-based module, re-exporting with @racket[all-from-out] is based
specifically on the @racket[module-path] reference, and not the module
that is actually referenced.}
尽管不同的@racket[module-path]可以适用于同一个基于文件的模块，但带@racket[all-from-out]的重复输出是明确基于@racket[module-path]引用，而不是被实际引用的模块。

}

@;------------------------------------------------------------------------
@specspecsubform[#:literals (except-out)
                 (except-out provide-spec id ...)]{

@;{Like @racket[provide-spec], but omitting the export of each
@racket[id], where @racket[id] is the external name of the binding to
omit.}
就像@racket[provide-spec]，但省略每个@racket[id]的输出，其中@racket[id]是绑定到省略的外部名称。

}


@;------------------------------------------------------------------------
@specspecsubform[#:literals (prefix-out)
                 (prefix-out prefix-id provide-spec)]{

@;{Like @racket[provide-spec], but adding @racket[prefix-id] to the
beginning of the external name for each exported binding.}
就像@racket[provide-spec]，但为每个输出的绑定添加@racket[prefix-id]到外部名称的开头。

}

