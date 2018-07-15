#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt"
          (for-label racket/dict racket/serialize))

@(define posn-eval (make-base-eval))

@;{@title[#:tag "define-struct"]{Programmer-Defined Datatypes}}
@title[#:tag "define-struct"]{程序员定义的数据类型}

@;{@refalso["structures"]{structure types}}
@margin-note{在《Racket参考》中的“（structures）”部分也有关于数据结构类型的文档。}

@;{New datatypes are normally created with the @racket[struct]
form, which is the topic of this chapter. The class-based object
system, which we defer to @secref["classes"], offers an alternate
mechanism for creating new datatypes, but even classes and objects are
implemented in terms of structure types.}
新的数据类型通常用@racket[struct]表来创造，这是本章的主题。基于类的对象系统，我们参照《@secref["classes"]》，为创建新的数据类型提供了一种替换机制，但即使是类和对象也是根据结构类型实现。

@; ------------------------------------------------------------
@;{@section{Simple Structure Types: @racket[struct]}}
@section[#:tag "Simple-Structure-Types-struct"]{简单的结构类型：@racket[struct]}

@;{@refalso["define-struct"]{@racket[struct]}}
@margin-note{在《Racket参考》中的“（define-struct）”部分也有关于@racket[struct]的文档。}

@;{To a first approximation, the syntax of @racket[struct] is}
作为一个最接近的，@racket[struct]的语法是

@specform[
(struct struct-id (field-id ...))
]{}

@as-examples[@racketblock+eval[
#:eval posn-eval
(struct posn (x y))
]]

@;{The @racket[struct] form binds @racket[_struct-id] and a number of
identifiers that are built from @racket[_struct-id] and the
@racket[_field-id]s:}
@racket[struct]表绑定@racket[_struct-id]和一个标识符的数值，它构建于@racket[_struct-id]和@racket[_field-id]：

@itemize[

 @item{
  @;{@racket[_struct-id] : a @deftech{constructor} function that
       takes as many arguments as the number of @racket[_field-id]s,
       and returns an instance of the structure type.}
@racket[_struct-id]：一个@deftech{构造器（constructor）}函数，带有和acket[_field-id]的数值一样多的参数，并返回这个结构类型的一个实例。

       @examples[#:eval posn-eval (posn 1 2)]}

 @item{
  @;{@racket[_struct-id]@racketidfont{?} : a @deftech{predicate}
       function that takes a single argument and returns @racket[#t]
       if it is an instance of the structure type, @racket[#f]
       otherwise.}
@racket[_struct-id]@racketidfont{?}：一个@deftech{判断（predicate）}函数，它带一个单个参数，同时如果它是这个结构类型的一个实例则返回@racket[#t]，否则返回@racket[#f]。

       @examples[#:eval posn-eval (posn? 3) (posn? (posn 1 2))]}

 @item{
  @;{@racket[_struct-id]@racketidfont{-}@racket[_field-id] : for
       each @racket[_field-id], an @deftech{accessor} that extracts
       the value of the corresponding field from an instance of the
       structure type.}
@racket[_struct-id]@racketidfont{-}@racket[_field-id]：对于每个@racket[_field-id]，一个@deftech{访问器（accessor）}从这个结构类型的一个实例中解析相应字段的值。

       @examples[#:eval posn-eval 
                 (posn-x (posn 1 2)) (posn-y (posn 1 2))]}

 @item{
  @;{@racketidfont{struct:}@racket[_struct-id] : a
       @deftech{structure type descriptor}, which is a value that
       represents the structure type as a first-class value (with
       @racket[#:super], as discussed later in
       @secref["struct-options"]).}
    @racketidfont{struct:}@racket[_struct-id]：一个@deftech{结构类型描述符（structure type descriptor）}，它是一个值，体现结构类型作为一个第一类值（与@racket[#:super]，在《@secref["struct-options"]》中作为后续讨论）。
    }

]

@;{A @racket[struct] form places no constraints on the kinds of
values that can appear for fields in an instance of the structure
type. For example, @racket[(posn "apple" #f)] produces an
instance of @racket[posn], even though @racket["apple"] and
@racket[#f] are not valid coordinates for the obvious uses of
@racket[posn] instances. Enforcing constraints on field values, such
as requiring them to be numbers, is normally the job of a contract, as
discussed later in @secref["contracts"].}
一个@racket[struct]表对值的种类不设置约束条件，它可表现为这个结构类型的一个实例中的字段。例如，@racket[(posn "apple" #f)]过程产生一个@racket[posn]实例，即使@racket["apple"]和@racket[#f]对@racket[posn]实例的显性使用是无效的配套。执行字段值上的约束，比如要求它们是数值，通常是一个合约的工作，在《@secref["contracts"]》中作为后续讨论。

@; ------------------------------------------------------------
@;{@section[#:tag "struct-copy"]{Copying and Update}}
@section[#:tag "struct-copy"]{复制和更新}

@;{The @racket[struct-copy] form clones a structure and optionally
updates specified fields in the clone. This process is sometimes
called a @deftech{functional update}, because the result is a
structure with updated field values. but the original structure is not
modified.}
@racket[struct-copy]表克隆一个结构并可选地更新克隆中的指定字段。这个过程有时称为一个@deftech{功能性更新（functional update）}，因为这个结果是一个带有更新字段值的结构。但原始的结构没有被修改。

@specform[
(struct-copy struct-id struct-expr [field-id expr] ...)
]

@;{The @racket[_struct-id] that appears after @racket[struct-copy] must
be a structure type name bound by @racket[struct] (i.e., the
name that cannot be used directly as an expression). The
@racket[_struct-expr] must produce an instance of the structure type.
The result is a new instance of the structure type that is like the old
one, except that the field indicated by each @racket[_field-id] gets
the value of the corresponding @racket[_expr].}
出现在@racket[struct-copy]后面的@racket[_struct-id]必须是由@racket[struct]绑定的结构类型名称（即这个名称不能作为一个表达式直接被使用）。@racket[_struct-expr]必须产生结构类型的一个实例。结果是一个新实例，就像旧的结构类型一样，除这个被每个@racket[_field-id]标明的字段得到相应的@racket[_expr]的值之外。

@examples[
#:eval posn-eval 
(define p1 (posn 1 2))
(define p2 (struct-copy posn p1 [x 3]))
(list (posn-x p2) (posn-y p2))
(list (posn-x p1) (posn-x p2))
]

@; ------------------------------------------------------------
@;{@section[#:tag "struct-subtypes"]{Structure Subtypes}}
@section[#:tag "struct-subtypes"]{结构子类型}

@;{An extended form of @racket[struct] can be used to define a
@defterm{structure subtype}, which is a structure type that extends an
existing structure type:}
@racket[struct]的一个扩展表可以用来定义一个@defterm{结构子类型（structure subtype）}，它是一种扩展一个现有结构类型的结构类型：

@specform[
(struct struct-id super-id (field-id ...))
]

@;{The @racket[_super-id] must be a structure type name bound by
@racket[struct] (i.e., the name that cannot be used directly as
an expression).}
这个@racket[_super-id]必须是一个由@racket[struct]绑定的结构类型名称（即名称不能被作为一个表达式直接使用）。

@as-examples[@racketblock+eval[
#:eval posn-eval 
(struct posn (x y))
(struct 3d-posn posn (z))
]]

@;{A structure subtype inherits the fields of its supertype, and the
subtype constructor accepts the values for the subtype fields after
values for the supertype fields. An instance of a structure subtype
can be used with the predicate and accessors of the
supertype.}
一个结构子类型继承其超类型的字段，并且子类型构造器接受在超类型字段的值之后的子类型字段的值。一个结构子类型的一个实例可以被用作这个超类型的判断和访问器。

@examples[
#:eval posn-eval 
(define p (3d-posn 1 2 3))
p
(posn? p)
(3d-posn-z p)
(code:comment @;{"a 3d-posn has an x field, but there is no 3d-posn-x selector:"}"3d-posn有一个x字段，但是这里却没有3d-posn-x选择器：")
(3d-posn-x p)
(code:comment @;{"use the supertype's posn-x selector to access the x field:"}"使用基类型的posn-x选择器去访问x字段：")
(posn-x p)
]

@; ------------------------------------------------------------
@;{@section[#:tag "trans-struct"]{Opaque versus Transparent Structure Types}}
@section[#:tag "trans-struct"]{不透明结构类型与透明结构类型对比}

@;{With a structure type definition like}
用一个结构类型定义如下：

@racketblock[
(struct posn (x y))
]

@;{an instance of the structure type prints in a way that does not show
any information about the fields' values. That is, structure types by
default are @deftech{opaque}. If the accessors and mutators of a
structure type are kept private to a module, then no other module can
rely on the representation of the type's instances.}
结构类型的一个实例以不显示关于字段值的任何信息的方式打印。也就是说，默认的结构类型是@deftech{不透明的（opaque）}。如果一个结构类型的访问器和修改器对一个模块保持私有，那么没有其它的模块可以依赖这个类型实例的表示。

@;{To make a structure type @deftech{transparent}, use the
@racket[#:transparent] keyword after the field-name sequence:}
让一个结构类型@deftech{透明（transparent）}，在字段名序列后面使用@racket[#:transparent]关键字：

@def+int[
#:eval posn-eval
(struct posn (x y)
        #:transparent)
(posn 1 2)
]

@;{An instance of a transparent structure type prints like a call to the
constructor, so that it shows the structures field values. A
transparent structure type also allows reflective operations, such as
@racket[struct?] and @racket[struct-info], to be used on its instances
(see @secref["reflection"]).}
一个透明结构类型的一个实例像一个对构造器的调用一样打印，因此它显示这个结构字段值。一个透明结构类型也允许反射操作，比如@racket[struct?]和@racket[struct-info]，在其实例中被使用（参见《@secref["reflection"]》）。

@;{Structure types are opaque by default, because opaque structure
instances provide more encapsulation guarantees. That is, a library
can use an opaque structure to encapsulate data, and clients of the
library cannot manipulate the data in the structure except as allowed
by the library.}
默认情况下，结构类型是不透明的，因为不透明的结构实例提供了更多的封装保证。也就是说，一个库可以使用不透明的结构来封装数据，而库中的客户机除了在库中被允许之外，也不能操纵结构中的数据。

@; ------------------------------------------------------------
@;{@section[#:tag "struct-equal"]{Structure Comparisons}}
@section[#:tag "struct-equal"]{结构的比较}

@;{A generic @racket[equal?] comparison automatically recurs on the
fields of a transparent structure type, but @racket[equal?] defaults
to mere instance identity for opaque structure types:}
一个通用的@racket[equal?]比较自动出现在一个透明的结构类型的字段上，但是@racket[equal?]默认仅针对不透明结构类型的实例标识：

@def+int[
#:eval posn-eval
(struct glass (width height) #:transparent)
(equal? (glass 1 2) (glass 1 2))
]
@def+int[
#:eval posn-eval
(struct lead (width height))
(define slab (lead 1 2))
(equal? slab slab)
(equal? slab (lead 1 2))
]

@;{To support instances comparisons via @racket[equal?] without making
the structure type transparent, you can use the @racket[#:methods]
keyword, @racket[gen:equal+hash], and implement three methods:}
通过@racket[equal?]支持实例比较而不需要使结构型透明，你可以使用@racket[#:methods]关键字、@racket[gen:equal+hash]并执行三个方法：

@def+int[
#:eval posn-eval
(struct lead (width height)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (code:comment @#,t{@;{compare @racket[a] and @racket[b]}比较@racket[a]和@racket[b]})
     (and (equal?-recur (lead-width a) (lead-width b))
          (equal?-recur (lead-height a) (lead-height b))))
   (define (hash-proc a hash-recur)
     (code:comment @#,t{@;{compute primary hash code of @racket[a]}计算首要的@racket[a]哈希代码。})
     (+ (hash-recur (lead-width a))
        (* 3 (hash-recur (lead-height a)))))
   (define (hash2-proc a hash2-recur)
     (code:comment @#,t{@;{compute secondary hash code of @racket[a]}计算次重要的@racket[a]哈希代码。})
     (+ (hash2-recur (lead-width a))
             (hash2-recur (lead-height a))))])
(equal? (lead 1 2) (lead 1 2))
]

@;{The first function in the list implements the @racket[equal?] test on
two @racket[lead]s; the third argument to the function is used instead
of @racket[equal?] for recursive equality testing, so that data cycles
can be handled correctly. The other two functions compute primary and
secondary hash codes for use with @tech{hash tables}:}
列表中的第一个函数实现对两个@racket[lead]的@racket[equal?]测试；函数的第三个参数是用来代替@racket[equal?]实现递归的相等测试，以便这个数据循环可以被正确处理。其它两个函数计算以@tech{哈希表（hash tables）}使用的首要的和次重要的哈希代码：

@interaction[
#:eval posn-eval
(define h (make-hash))
(hash-set! h (lead 1 2) 3)
(hash-ref h (lead 1 2))
(hash-ref h (lead 2 1))
]

@;{The first function provided with @racket[gen:equal+hash] is not
required to recursively compare the fields of the structure. For
example, a structure type representing a set might implement equality
by checking that the members of the set are the same, independent of
the order of elements in the internal representation. Just take care
that the hash functions produce the same value for any two structure
types that are supposed to be equivalent.}
拥有@racket[gen:equal+hash]的第一个函数不需要递归比较结构的字段。例如，表示一个集合的一个结构类型可以通过检查这个集合的成员是相同的来执行相等，独立于内部表示的元素顺序。只是要注意哈希函数对任何两个假定相等的结构类型产生相同的值。

@; ------------------------------------------------------------
@;{@section{Structure Type Generativity}}
@section[#:tag "Structure-Type-Generativity"]{结构类型的生成性}

@;{Each time that a @racket[struct] form is evaluated, it
generates a structure type that is distinct from all existing
structure types, even if some other structure type has the same name
and fields.}
每次对一个@racket[struct]表求值时，它就生成一个与所有现有结构类型不同的结构类型，即使某些其它结构类型具有相同的名称和字段。

@;{This generativity is useful for enforcing abstractions and
implementing programs such as interpreters, but beware of placing a
@racket[struct] form in positions that are evaluated multiple
times.}
这种生成性对强制抽象和执行程序（比如口译员）是有用的，但小心放置一个@racket[struct]表到被多次求值的位置。

@defexamples[
(define (add-bigger-fish lst)
  (struct fish (size) #:transparent) (code:comment #,(t "new every time"))
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))

(add-bigger-fish null)
(add-bigger-fish (add-bigger-fish null))
]
@defs+int[
[(struct fish (size) #:transparent)
 (define (add-bigger-fish lst)
   (cond
    [(null? lst) (list (fish 1))]
    [else (cons (fish (* 2 (fish-size (car lst))))
                lst)]))]
(add-bigger-fish (add-bigger-fish null))
]

@; ------------------------------------------------------------
@;{@section[#:tag "prefab-struct"]{Prefab Structure Types}}
@section[#:tag "prefab-struct"]{预制结构类型}

@;{Although a @tech{transparent} structure type prints in a way that
shows its content, the printed form of the structure cannot be used in
an expression to get the structure back, unlike the printed form of a
number, string, symbol, or list.}
虽然一个@tech{透明}结构类型以显示内容的方式打印，但不像一个数值、字符串、符号或列表的打印表，结构的打印表不能用在一个表达式中以找回结构。

@;{A @deftech{prefab} (``previously fabricated'') structure type is a
built-in type that is known to the Racket printer and expression
reader. Infinitely many such types exist, and they are indexed by
name, field count, supertype, and other such details. The printed form
of a prefab structure is similar to a vector, but it starts
@litchar{#s} instead of just @litchar{#}, and the first element in the
printed form is the prefab structure type's name.}
一个@deftech{预制（prefab）}（“被预先制造”）结构类型是一个内置的类型，它是已知的Racket打印机和表达式阅读器。有无限多这样的类型存在，并且它们通过名字、字段计数、超类型以及其它细节来索引。一个预制结构的打印表类似于一个向量，但它以@litchar{#s}开始而不是仅以@litchar{#}开始，而且打印表的第一个元素是预制结构类型的名称。

@;{The following examples show instances of the @racketidfont{sprout}
prefab structure type that has one field. The first instance has a
field value @racket['bean], and the second has field value
@racket['alfalfa]:}
下面的示例显示具有一个字段的@racketidfont{sprout}预置结构类型的实例。第一个实例具有一个字段值@racket['bean]，以及第二个具有字段值@racket['alfalfa]：

@interaction[
'#s(sprout bean)
'#s(sprout alfalfa)
]

@;{Like numbers and strings, prefab structures are ``self-quoting,'' so
the quotes above are optional:}
像数字和字符串一样，预置结构是“自引用”，所以上面的引号是可选的：

@interaction[
#s(sprout bean)
]

@;{When you use the @racket[#:prefab] keyword with
@racket[struct], instead of generating a new structure type,
you obtain bindings that work with the existing prefab structure type:}
当你用@racket[struct]使用@racket[#:prefab]关键字，而不是生成一个新的结构类型，你获得与现有的预制结构类型的绑定：

@interaction[
#:eval posn-eval
(define lunch '#s(sprout bean))
(struct sprout (kind) #:prefab)
(sprout? lunch)
(sprout-kind lunch)
(sprout 'garlic)
]

@;{The field name @racketidfont{kind} above does not matter for finding
the prefab structure type; only the name @racketidfont{sprout} and the
number of fields matters. At the same time, the prefab structure type
@racketidfont{sprout} with three fields is a different structure type
than the one with a single field:}
上面的字段名@racketidfont{kind}对查找预置结构类型无关紧要，仅名称@racketidfont{sprout}和字段数量是紧要的。同时，具有三个字段的预制结构类型@racketidfont{sprout}是一种不同于一个单个字段的结构类型：

@interaction[
#:eval posn-eval
(sprout? #s(sprout bean #f 17))
(code:line (struct sprout (kind yummy? count) #:prefab) (code:comment @#,t{redefine}))
(sprout? #s(sprout bean #f 17))
(sprout? lunch)
]

@;{A prefab structure type can have another prefab structure type as its
supertype, it can have mutable fields, and it can have auto
fields. Variations in any of these dimensions correspond to different
prefab structure types, and the printed form of the structure type's
name encodes all of the relevant details.}
一个预制结构类型可以有另一种预制结构类型作为它的超类型，它具有可变的字段，并它可以有自动字段。这些维度中的任何变化都对应于不同的预置结构类型，而且结构类型名称的打印表编码所有的相关细节。

@interaction[
(struct building (rooms [location #:mutable]) #:prefab)
(struct house building ([occupied #:auto]) #:prefab
  #:auto-value 'no)
(house 5 'factory)
]

@;{Every @tech{prefab} structure type is @tech{transparent}---but even
less abstract than a @tech{transparent} type, because instances can be
created without any access to a particular structure-type declaration
or existing examples. Overall, the different options for structure
types offer a spectrum of possibilities from more abstract to more
convenient:}
每个@tech{预制}结构类型都是@tech{透明}的——但甚至比一个@tech{透明}类型更抽象，因为可以创建实例而不必访问一个特定的结构类型声明或现有示例。总体而言，结构类型的不同选项提供了从更抽象到更方便的一连串可能性：

@itemize[

 @item{
  @;{@tech{Opaque} (the default) : Instances cannot be inspected or
       forged without access to the structure-type declaration. As
       discussed in the next section, @tech{constructor guards} and
       @tech{properties} can be attached to the structure type to
       further protect or to specialize the behavior of its
       instances.}
@tech{不透明的（Opaque）}（默认）：没有访问结构类型声明，就不能检查或创造实例。正如下一节所讨论的，@tech{构造器看守（constructor guards）}和@tech{属性（properties）}可以附加到结构类型上以进一步保护或专门化其实例的行为。
    }

 @item{
  @;{@tech{Transparent} : Anyone can inspect or create an instance
       without access to the structure-type declaration, which means
       that the value printer can show the content of an instance. All
       instance creation passes through a @tech{constructor guard},
       however, so that the content of an instance can be controlled,
       and the behavior of instances can be specialized through
       @tech{properties}. Since the structure type is generated by its
       definition, instances cannot be manufactured simply through the
       name of the structure type, and therefore cannot be generated
       automatically by the expression reader. }
@tech{透明的（Transparent）}：任何人都可以检查或创建一个没有访问结构类型声明的实例，这意味着这个值打印机可以显示一个实例的内容。然而，所有实例创建都经过一个@tech{构造器看守}，这样可以控制一个实例的内容，并且实例的行为可以通过@tech{属性（properties）}进行特例化。由于结构类型由其定义生成，实例不能简单地通过结构类型的名称来制造，因此不能由表达式读取器自动生成。
    }

 @item{
  @;{@tech{Prefab} : Anyone can inspect or create an instance at any
       time, without prior access to a structure-type declaration or
       an example instance. Consequently, the expression reader can
       manufacture instances directly. The instance cannot have a
       @tech{constructor guard} or @tech{properties}.}
@tech{预制的（Prefab）}：任何人都可以在任何时候检查或创建一个实例，而不必事先访问一个结构类型声明或一个实例。因此，表达式读取器可以直接制造实例。实例不能具有一个@tech{构造器看守}或@tech{属性}。
    }

]

@;{Since the expression reader can generate @tech{prefab} instances, they
are useful when convenient @tech{serialization} is more important than
abstraction. @tech{Opaque} and @tech{transparent} structures also can
be serialized, however, if they are defined with
@racket[serializable-struct] as described in
@secref["serialization"].}
由于表达式读取器可以生成@tech{预制}实例，所以在便利的@tech{序列化（serialization）}比抽象更重要时它们是有用的。然而，如果他们如《@secref["serialization"]》所描述那样被用@racket[serializable-struct]定义，@tech{不透明}和@tech{透明}的结构也可以被序列化。

@; ------------------------------------------------------------
@;{@section[#:tag "struct-options"]{More Structure Type Options}}
@section[#:tag "struct-options"]{更多的结构类型选项}

@;{The full syntax of @racket[struct] supports many options, both
at the structure-type level and at the level of individual fields:}
无论是在结构类型级还是在个别字段级上，@racket[struct]的完整语法支持许多选项：

@specform/subs[(struct struct-id maybe-super (field ...)
                       struct-option ...)
               ([maybe-super code:blank
                             super-id]
                [field field-id
                       [field-id field-option ...]])]

@;{A @racket[_struct-option] always starts with a keyword:}
一个 @racket[_struct-option]总是以一个关键字开头：

 @specspecsubform[#:mutable]{

    @;{Causes all fields of the structure to be mutable, and introduces
    for each @racket[_field-id] a @deftech{mutator}
@racketidfont{set-}@racket[_struct-id]@racketidfont{-}@racket[_field-id]@racketidfont{!}
    that sets the value of the corresponding field in an instance of
    the structure type.}
会导致结构的所有字段是可变的，并且给每个@racket[_field-id]产生一个@deftech{设置方式}@racketidfont{set-}@racket[_struct-id]@racketidfont{-}@racket[_field-id]@racketidfont{!}，其在结构类型的一个实例中设置对应字段的值。

     @defexamples[(struct dot (x y) #:mutable)
                  (define d (dot 1 2))
                  (dot-x d)
                  (set-dot-x! d 10)
                  (dot-x d)]

   @;{The @racket[#:mutable] option can also be used as a
   @racket[_field-option], in which case it makes an individual field
   mutable.}
@racket[#:mutable]选项也可以被用来作为一个@racket[_field-option]，在这种情况下，它使一个个别字段可变。
       
   @defexamples[
   (struct person (name [age #:mutable]))
   (define friend (person "Barney" 5))
   (set-person-age! friend 6)
   (set-person-name! friend "Mary")]}

 @specspecsubform[(code:line #:transparent)]{
  @;{Controls reflective access to structure instances, as discussed
  in a previous section, @secref["trans-struct"].}
对结构实例的控制反射访问，如前面一节《@secref["trans-struct"]》所讨论的那样。
}

 @specspecsubform[(code:line #:inspector inspector-expr)]{
@;{Generalizes @racket[#:transparent] to support more controlled access
  to reflective operations.}
推广@racket[#:transparent]以支持更多的控制访问或反射操作。
  }

 @specspecsubform[(code:line #:prefab)]{
  @;{Accesses a built-in structure type, as discussed
  in a previous section, @secref["prefab-struct"].}
    访问内置结构类型，如前一节《@secref["prefab-struct"]》所讨论的。
    }

 @specspecsubform[(code:line #:auto-value auto-expr)]{
  @;{Specifies a value to be used for all automatic fields in the
  structure type, where an automatic field is indicated by the
  @racket[#:auto] field option. The constructor procedure does not
  accept arguments for automatic fields. Automatic fields are
  implicitly mutable (via reflective operations), but mutator
  functions are bound only if @racket[#:mutable] is also specified.}
指定一个被用于所有在结构类型里的自动字段值，这里一个自动字段被@racket[#:auto]字段被标示。这个构造函数不接受给自动字段的参数。自动字段无疑是可变的（通过反射操作），但设置函数仅在@racket[#:mutable]也被指定的时候被绑定。

  @defexamples[
    (struct posn (x y [z #:auto])
                 #:transparent
                 #:auto-value 0)
    (posn 1 2)
  ]}

@;-- FIXME:
@;-- Explain when to use guards instead of contracts, and vice versa

 @specspecsubform[(code:line #:guard guard-expr)]{
 @;{Specifies a
  @deftech{constructor guard} procedure to be called whenever an
  instance of the structure type is created. The guard takes as many
  arguments as non-automatic fields in the structure type, plus one
  more for the name of the instantiated type (in case a sub-type is
  instantiated, in which case it's best to report an error using the
  sub-type's name). The guard should return the same number of values
  as given, minus the name argument. The guard can raise an exception
  if one of the given arguments is unacceptable, or it can convert an
  argument.}
每当一个结构类型的实例被创建，都指定一个@deftech{构造器看守（constructor guard）}过程以供调用。在结构类型中这个看守获取与结构类型中的非自动字段相同数量的参数，再加上一个实例化类型的名称（如果一个子类型被实例化，在这种情况下最好使用子类型的名称报告一个错误）。看守应该返回与给定值相同数量的值，减去名称参数。如果某个参数不可接受，或者它可以转换一个参数，则这个看守可以引发一个异常。

 @defexamples[
   #:eval posn-eval
   (struct thing (name)
           #:transparent
           #:guard (lambda (name type-name)
                     (cond
                       [(string? name) name]
                       [(symbol? name) (symbol->string name)]
                       [else (error type-name 
                                    "bad name: ~e" 
                                    name)])))
   (thing "apple")
   (thing 'apple)
   (thing 1/2)
  ]

  @;{The guard is called even when subtype instances are created. In that
  case, only the fields accepted by the constructor are provided to
  the guard (but the subtype's guard gets both the original fields and
  fields added by the subtype).}
即使子类型实例被创建，这个看守也会被调用。在这种情况下，只有被构造器接受的字段被提供给看守（但是子类型的看守同时获得子类型添加的原始字段和现有字段）。

 @defexamples[
  #:eval posn-eval
  (struct person thing (age)
          #:transparent
          #:guard (lambda (name age type-name)
                    (if (negative? age)
                        (error type-name "bad age: ~e" age)
                        (values name age))))
  (person "John" 10)
  (person "Mary" -1)
  (person 10 10)]}

 @specspecsubform[(code:line #:methods interface-expr [body ...])]{
  @;{Associates method definitions for the structure type that correspond
  to a @defterm{generic interface}.  For example, implementing the
  methods for @racket[gen:dict] allows instances of a structure
  type to be used as dictionaries. Implementing
  the methods for @racket[gen:custom-write] allows the customization
  of how an instance of a structure type is @racket[display]ed.}
使方法定义与关联到一个@defterm{通用接口（generic interface）}的结构类型关联。例如，执行@racket[gen:dict]方法允许一个结构类型的实例用作字典。执行@racket[gen:custom-write]方法允许一个如何被@racket[显示（display）]的结构类型的一个实例的定制。

  @defexamples[
    (struct cake (candles)
            #:methods gen:custom-write
            [(define (write-proc cake port mode)
               (define n (cake-candles cake))
               (show "   ~a   ~n" n #\. port)
               (show " .-~a-. ~n" n #\| port)
               (show " | ~a | ~n" n #\space port)
               (show "---~a---~n" n #\- port))
             (define (show fmt n ch port)
               (fprintf port fmt (make-string n ch)))])
    (display (cake 5))]}

 @specspecsubform[(code:line #:property prop-expr val-expr)]{
   @;{Associates a @deftech{property} and value with the structure type.
   For example, the @racket[prop:procedure] property allows a
   structure instance to be used as a function; the property value
   determines how a call is implemented when using the structure as a
   function.}
使一个@deftech{属性（property）}和值与结构类型相关联。例如，@racket[prop:procedure]属性允许一个结构实例用作一个函数；属性值决定当使用这个结构作为一个函数时一个调用如何被执行。

 @defexamples[
   (struct greeter (name)
           #:property prop:procedure
                      (lambda (self other)
                        (string-append
                         "Hi " other
                         ", I'm " (greeter-name self))))
   (define joe-greet (greeter "Joe"))
   (greeter-name joe-greet)
   (joe-greet "Mary")
   (joe-greet "John")]}

 @specspecsubform[(code:line #:super super-expr)]{
  @;{An alternative to supplying a @racket[super-id] next to
  @racket[struct-id]. Instead of the name of a structure type (which is
  not an expression), @racket[super-expr] should produce a
  @tech{structure type descriptor} value. An advantage of
  @racket[#:super] is that structure type descriptors are values, so
  they can be passed to procedures.}
用于一个与@racket[struct-id]紧邻的@racket[super-id]的一个替代者。代替一个结构类型的这个名字（它是一个表达式），@racket[super-expr]应该产生一个@tech{结构类型的描述符（structure type descriptor）}值。@racket[#:super]的一个优点是结构类型的描述符是值，所以可以被传递给过程。

  @defexamples[
    #:eval posn-eval
    (define (raven-constructor super-type)
      (struct raven ()
              #:super super-type
              #:transparent
              #:property prop:procedure (lambda (self)
                                          'nevermore))
      raven)
    (let ([r ((raven-constructor struct:posn) 1 2)])
      (list r (r)))
    (let ([r ((raven-constructor struct:thing) "apple")])
      (list r (r)))]}

@; ----------------------------------------

@;{@refdetails["structures"]{structure types}}
@margin-note{在《Racket参考》中的“（structures）”里提供有更多关于数据结构类型的内容。}

@close-eval[posn-eval]
