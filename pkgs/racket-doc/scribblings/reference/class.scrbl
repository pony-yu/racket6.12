#lang scribble/doc
@(require "mz.rkt"
          racket/class
          (for-syntax racket/base racket/serialize racket/trait))

@(begin

(define-syntax sees
  (syntax-rules ()
    [(_) ""]
    [(_ s) (elem @;{" and "}"》和《" (secref s))]
    [(_ s ... s0) (elem (elem @;{", "}"，《" (secref s)) ... @;{", and "}"》和《" (secref s0) "》")]))

(define-syntax (defclassforms stx)
  (syntax-case stx (*)
    [(_ [* (form ...) (also ...) more ...])
     @;{#'(defform* (form  ...)
         "See " @racket[class*] (sees also ...) "; use"
         " outside the body of a " @racket[class*] " form is a syntax error."
         more ...)}
     #'(defform* (form  ...)
         "参见《" @racket[class*] (sees also ...) "》；使用"
         "一个"@racket[class*]"表的外部主体是一个语法错误。"
         more ...)]
    [(_ [form (also ...) more ...])
     #'(defclassforms [* (form) (also ...) more ...])]
    [(_ form ...)
     #'(begin (defclassforms form) ...)]))

(define-syntax (defstarshorthands stx)
  (syntax-case stx ()
    [(_ form)
     (with-syntax ([name (string->symbol
                           (let ([s (symbol->string (syntax-e #'form))])
                             (substring s 0 (sub1 (string-length s)))))]
                   [tmpl (let ([s #'(... (thing (id expr) ...))])
                           (datum->syntax s
                                          (cons (datum->syntax
                                                 #'form
                                                 (syntax-e #'form)
                                                 (car (syntax-e s)))
                                                (cdr (syntax-e s)))
                                          s))])
       #'(...
          (defform tmpl
            @;{"Shorthand for "}"简写，原型为" (racket (begin (#,(racket name) id) ... (define id _expr) ...)) ".")))]
     [(_ form ...)
      #'(begin (defstarshorthands form) ...)]))

(define-syntax (defdefshorthands stx)
  (syntax-case stx ()
    [(_ form)
     (with-syntax ([name (string->symbol
                          (let ([s (symbol->string (syntax-e #'form))])
                            (string-append "define/" s)))])
       (with-syntax ([tmpl1 (let ([s #'(define id expr)])
                              (datum->syntax s
                                             (cons (datum->syntax
                                                    #'form
                                                    (syntax-e #'name)
                                                    (car (syntax-e s)))
                                                   (cdr (syntax-e s)))
                                             s))])
         #'(...
            (defform* [tmpl1 (#,(racket name) (id . formals) body ...+)]
              @;{"Shorthand for "}"简写，原型为"
              (racket (begin (#,(racket form) id) (define id expr)))
              @;{" or "}"或"
              (racket (begin (#,(racket form) id) (define (id . formals) body ...+)))))))]
     [(_ form ...)
      #'(begin (defdefshorthands form) ...)]))

(define class-eval (make-base-eval))
(define class-ctc-eval (make-base-eval))

)

@examples[#:hidden #:eval class-eval
          (require racket/class racket/contract)]
@examples[#:hidden #:eval class-ctc-eval
          (require racket/class racket/contract)]

@;{@title[#:tag "mzlib:class" #:style 'toc]{Classes and Objects}}
@title[#:tag "mzlib:class" #:style 'toc]{类和对象}

@;{@guideintro["classes"]{classes and objects}}
@guideintro["classes"]{类和对象}

@note-lib[racket/class #:use-sources (racket/private/class-internal)]

@;{A @deftech{class} specifies}
一个类指定

@itemize[
 
 @item{@;{a collection of fields;}
  一个字段集合；}

 @item{@;{a collection of methods;}
  一个方法集合；}

 @item{@;{initial value expressions for the fields;  and}
 字段的初始值表达式；}

 @item{@;{initialization variables that are bound to initialization
 arguments.}
 绑定到初始化参数的初始化变量。}

]

@;{In the context of the class system, an @defterm{object} is a
collection of bindings for fields that are instantiated according to a
class description.}
在类系统的上下文中，一个@defterm{对象（object）}是对一个类描述实例化的字段绑定的一个集合。

@;{The class system allows a program to define a new class (a
@deftech{derived class}) in terms of an existing class (the
@deftech{superclass}) using inheritance, overriding, and augmenting:}
这类系统允许一个程序定义一个新类（一个@deftech{派生类（derived class）}）在一个现有的类（@deftech{基类（superclass）}）使用继承、重写和增强：

@itemize[

 @item{
  @;{@deftech{inheritance}: An object of a derived class supports
 methods and instantiates fields declared by the derived class's
 superclass, as well as methods and fields declared in the derived
 class expression.}
@deftech{继承（inheritance）}：一个派生类对象支持的方法和实例字段由派生类的基类声明，以及方法和字段在派生类的表达式中声明。
    }

 @item{
  @;{@deftech{overriding}: Some methods declared in a superclass can
 be replaced in the derived class. References to the overridden method
 in the superclass use the implementation in the derived class.}
@deftech{重写（overriding）}：一些在基类中的方法中声明可以被派生类取代。对基类中的重写方法的引用使用在派生类中的实现。
 }

 @item{
  @;{@deftech{augmenting}: Some methods declared in a superclass can
 be merely extended in the derived class. The superclass method
 specifically delegates to the augmenting method in the derived class.}
@deftech{增强（augmenting）}：在基类中的一些方法申明可以在派生类中仅仅被扩展。基类方法具体代表派生类中的增强方法。
 }

]

@;{An @deftech{interface} is a collection of method names to be
implemented by a class, combined with a derivation requirement. A
class @deftech{implements} an interface when it}
一个@deftech{接口（interface）}是一个对一个类实现的方法名称的集合，并与派生需求相结合。在以下情况,一个类@deftech{实现（implements）}一个接口：

@itemize[

 @item{
  @;{declares (or inherits) a public method for each variable in the
 interface;}
    声明（或继承）接口中每个变量的一个公共方法；
    }

 @item{
  @;{is derived from the class required by the interface, if any; and}
    如果有的话，继承自接口所需的类；
    }

 @item{
  @;{specifically declares its intention to implement the interface.}
    特别声明了它实现接口的目的。
    }

]

@;{A class can implement any number of interfaces. A derived class
automatically implements any interface that its superclass
implements. Each class also implements an implicitly-defined interface
that is associated with the class. The implicitly-defined interface
contains all of the class's public method names, and it requires that
all other implementations of the interface are derived from the class.}
一个类可以实现任意数量的接口。一个派生类自动实现它的基类实现的任何接口。每个类还实现与类关联的一个隐式定义接口。该隐式定义接口包含所有类的公共方法名，并且它要求所有其它接口的实现都是从类派生的。

@;{A new interface can @deftech{extend} one or more interfaces with
additional method names; each class that implements the extended
interface also implements the original interfaces. The derivation
requirements of the original interface must be consistent, and the
extended interface inherits the most specific derivation requirement
from the original interfaces.}
一个新的接口可以用附加的方法名@deftech{扩展（extend）}一个或多个接口；实现扩展接口的每个类也实现原始接口。原始接口的派生要求必须是一致的，并且扩展接口继承了来自原始接口的最具体的派生需求。

@;{Classes, objects, and interfaces are all values. However, a class or
interface is not an object (i.e., there are no ``meta-classes'' or
``meta-interfaces'').}
类、对象和接口都是值。但是，一个类或接口不是一个对象（也就是说，没有“元类”或“元接口”）。

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@;{@section[#:tag "createinterface"]{Creating Interfaces}}
@section[#:tag "createinterface"]{创建接口}

@;{@guideintro["classes"]{classes, objects, and interfaces}}
@guideintro["classes"]{类、对象和接口}

@defform/subs[(interface (super-interface-expr ...) name-clause ...)
              ([name-clause
                id
                (id contract-expr)])]{

@;{Produces an interface. The @racket[id]s must be mutually distinct.}
生成一个接口。@racket[id]必须是互不相同的。 

@;{Each @racket[super-interface-expr] is evaluated (in order) when the
@racket[interface] expression is evaluated. The result of each
@racket[super-interface-expr] must be an interface value, otherwise
the @exnraise[exn:fail:object].  The interfaces returned by the
@racket[super-interface-expr]s are the new interface's
superinterfaces, which are all extended by the new interface. Any
class that implements the new interface also implements all of the
superinterfaces.}
在@racket[interface]表达式被求值时，每个@racket[super-interface-expr]被求值（按顺序）。每个@racket[super-interface-expr]的结果必须是一个接口值，否则@exnraise[exn:fail:object]。通过@racket[super-interface-expr]返回的接口是新接口的超级接口，它被新接口进行所有扩展。任何类实现新接口，也实现所有的超接口。

@;{The result of an @racket[interface] expression is an interface that
includes all of the specified @racket[id]s, plus all identifiers from
the superinterfaces. Duplicate identifier names among the
superinterfaces are ignored, but if a superinterface contains one of
the @racket[id]s in the @racket[interface] expression, the
@exnraise[exn:fail:object]. A given @racket[id] may be paired with
a corresponding @racket[contract-expr].}
一个@racket[interface]表达式的结果是一个接口，它包括所有指定的@racket[id]，加上来自于超级接口的标识符。在超接口之间的重复标识符名字被忽略，但如果一个超级接口包含@racket[interface]表达式中的@racket[id]之一，@exnraise[exn:fail:object]。一个给定的@racket[id]可以搭配一个相应的@racket[contract-expr]。

@;{If no @racket[super-interface-expr]s are provided, then the derivation
requirement of the resulting interface is trivial: any class that
implements the interface must be derived from @racket[object%].
Otherwise, the implementation requirement of the resulting interface
is the most specific requirement from its superinterfaces. If the
superinterfaces specify inconsistent derivation requirements, the
@exnraise[exn:fail:object].}
如果没有提供@racket[super-interface-expr]，那么结果作为结果的派生需求是微不足道的：任何实现该接口的类必须从@racket[object%]派生。否则，结果接口的实现需求是来自于它的超级接口的最具体的需求。如果超级接口不一致地指定派生需求，@exnraise[exn:fail:object]。

@examples[
#:eval class-ctc-eval
#:no-prompt
(define file-interface<%>
  (interface () open close read-byte write-byte))
(define directory-interface<%>
  (interface (file-interface<%>)
    [file-list (->m (listof (is-a?/c file-interface<%>)))]
    parent-directory))
]}

@defform/subs[(interface* (super-interface-expr ...)
                          ([property-expr val-expr] ...)
                name-clause ...)
              ([name-clause
                id
                (id contract-expr)])]{

@;{Like @racket[interface], but also associates to the interface the
structure-type properties produced by the @racket[property-expr]s with
the corresponding @racket[val-expr]s.}
就像@racket[interface]，但是也对接口联想到通过@racket[property-expr]用相应的@racket[val-expr]生成的结构类型属性。

@;{Whenever the resulting interface (or a sub-interface derived from it)
is explicitly implemented by a class through the @racket[class*] form,
each property is attached with its value to a structure type that
instantiated by instances of the class. Specifically, the property is
attached to a structure type with zero immediate fields, which is
extended to produce the internal structure type for instances of the
class (so that no information about fields is accessible to the
structure type property's guard, if any).}
每当作为结果的接口（或一个从它派生的子接口）通过@racket[class*]表被一个类单显式地实现时，每个属性都将其值附加到一个由类实例实例化的结构类型。具体来说，属性被附加到具有零立即字段的结构类型中，该字段被扩展为类的实例生成内部结构类型（因此，如果结构类型属性的守护者没有任何字段的信息，则可以访问）。

@examples[
#:eval class-eval
#:no-prompt
(define i<%> (interface* () ([prop:custom-write
                              (lambda (obj port mode) (void))])
               method1 method2 method3))
]}

@; ------------------------------------------------------------------------

@;{@section[#:tag "createclass"]{Creating Classes}}
@section[#:tag "createclass"]{创建类}

@;{@guideintro["classes"]{classes and objects}}
@guideintro["classes"]{类和对象}

@defthing[object% class?]{

@;{A built-in class that has no methods fields, implements only its own
interface @racket[(class->interface object%)], and is transparent
(i.e,. its inspector is @racket[#f], so all immediate instances are
@racket[equal?]). All other classes are derived from @racket[object%].}
一个内置类，没有方法字段，只实现自己的接口@racket[(class->interface object%)]，并且是透明的（即，其检查器是@racket[#f]，所以直接实例是@racket[equal?]）。所有其它类都是从@racket[object%]派生的。
}

@defform/subs[
#:literals (inspect init init-field field inherit-field init-rest init-rest
            public pubment public-final override override-final overment augment augride
            augment-final private abstract inherit inherit/super inherit/inner
            rename-super rename-inner begin lambda case-lambda let-values letrec-values
            define-values #%plain-lambda chaperone-procedure)
(class* superclass-expr (interface-expr ...)
  class-clause
  ...)
([class-clause
  (inspect inspector-expr)
  (init init-decl ...)
  (init-field init-decl ...)
  (field field-decl ...)
  (inherit-field maybe-renamed ...)
  (init-rest id)
  (init-rest)
  (public maybe-renamed ...)
  (pubment maybe-renamed ...)
  (public-final maybe-renamed ...)
  (override maybe-renamed ...)
  (overment maybe-renamed ...)
  (override-final maybe-renamed ...)
  (augment maybe-renamed ...)
  (augride maybe-renamed ...)
  (augment-final maybe-renamed ...)
  (private id ...)
  (abstract id ...)
  (inherit maybe-renamed ...)
  (inherit/super maybe-renamed ...)
  (inherit/inner maybe-renamed ...)
  (rename-super renamed ...)
  (rename-inner renamed ...)
  method-definition
  definition
  expr
  (begin class-clause ...)]

[init-decl
  id
  (renamed)
  (maybe-renamed default-value-expr)]

[field-decl
  (maybe-renamed default-value-expr)]

[maybe-renamed
  id
  renamed]

[renamed
  (internal-id external-id)]

[method-definition
  (define-values (id) method-procedure)]

[method-procedure
  (lambda kw-formals expr ...+)
  (case-lambda (formals expr ...+) ...)
  (#%plain-lambda formals expr ...+)
  (let-values ([(id) method-procedure] ...)
    method-procedure)
  (letrec-values ([(id) method-procedure] ...)
    method-procedure)
  (let-values ([(id) method-procedure] ...+) 
    id)
  (letrec-values ([(id) method-procedure] ...+) 
    id)
  (chaperone-procedure method-procedure wrapper-proc
                       other-arg-expr ...)])]{

@;{Produces a class value.}
生成一个类值。

@;{The @racket[superclass-expr] expression is evaluated when the
@racket[class*] expression is evaluated. The result must be a class
value (possibly @racket[object%]), otherwise the
@exnraise[exn:fail:object].  The result of the
@racket[superclass-expr] expression is the new class's superclass.}
当@racket[class*]表达式被求值时@racket[superclass-expr]表达式被求值。结果必须是类值（可能是@racket[object%]），否则@exnraise[exn:fail:object]。@racket[superclass-expr]表达式的结果是新的类的基类。

@;{The @racket[interface-expr] expressions are also evaluated when the
@racket[class*] expression is evaluated, after
@racket[superclass-expr] is evaluated. The result of each
@racket[interface-expr] must be an interface value, otherwise the
@exnraise[exn:fail:object].  The interfaces returned by the
@racket[interface-expr]s are all implemented by the class. For each
identifier in each interface, the class (or one of its ancestors) must
declare a public method with the same name, otherwise the
@exnraise[exn:fail:object]. The class's superclass must satisfy the
implementation requirement of each interface, otherwise the
@exnraise[exn:fail:object].}
@racket[superclass-expr]表达式被求值后，当@racket[class*]表达式被求值时@racket[interface-expr]表达式也被求值。每个@racket[interface-expr]的结果必须是一个接口的值，否则@exnraise[exn:fail:object]。通过@racket[interface-expr]返回的接口都是由类实现。对每个接口的每个标识符，类（或其原型之一）必须用相同的名称声明一个公共方法，否则@exnraise[exn:fail:object]。类的基类必须满足每一个接口的实现需求，否则@exnraise[exn:fail:object]。

@;{An @racket[inspect] @racket[class-clause] selects an inspector (see
@secref["inspectors"]) for the class extension. The
@racket[inspector-expr] must evaluate to an inspector or @racket[#f]
when the @racket[class*] form is evaluated. Just as for structure
types, an inspector controls access to the class's fields, including
private fields, and also affects comparisons using @racket[equal?]. If
no @racket[inspect] clause is provided, access to the class is
controlled by the parent of the current inspector (see
@secref["inspectors"]). A syntax error is reported if more than one
@racket[inspect] clause is specified.}
一个@racket[inspect] @racket[class-clause]为类表达式选择一个检查器（见@secref["inspectors"]）。@racket[inspector-expr]必须对一个检查器求值或或当@racket[class*]表被求值时为@racket[#f]。正如结构类型，检查器控制对类字段，包括私有字段的访问，同时也影响使用@racket[equal?]的比较。如果没有提供@racket[inspect]子句，则由当前检查器（请参阅@secref["inspectors"]）的父级控制对类的访问。如果指定了多个@racket[inspect]子句，则报告一个语法错误。

@;{The other @racket[class-clause]s define initialization arguments,
public and private fields, and public and private methods. For each
@racket[id] or @racket[maybe-renamed] in a @racket[public],
@racket[override], @racket[augment], @racket[pubment],
@racket[overment], @racket[augride], @racket[public-final],
@racket[override-final], @racket[augment-final], or @racket[private]
clause, there must be one @racket[method-definition]. All other
definition @racket[class-clause]s create private fields. All remaining
@racket[expr]s are initialization expressions to be evaluated when the
class is instantiated (see @secref["objcreation"]).}
其它@racket[class-clause]定义初始化参数、公共字段和私有字段，以及公共和私有方法。对在@racket[public]、@racket[override]、@racket[augment]、@racket[pubment]、@racket[overment]、@racket[augride]、@racket[public-final]、@racket[override-final]、@racket[augment-final]或者@racket[private]子句里的每个@racket[id]或@racket[maybe-renamed]，必须有一个@racket[method-definition]。所有其它定义@racket[class-clause]都创建私有字段。当类被实例化时（请参见@secref["objcreation"]），所有剩余的@racket[expr]是用于求值的初始化表达式。

@;{The result of a @racket[class*] expression is a new class, derived
from the specified superclass and implementing the specified
interfaces. Instances of the class are created with the
@racket[instantiate] form or @racket[make-object] procedure, as
described in @secref["objcreation"].}
一个@racket[class*]表达式的结果是一个新的类，从指定的基类派生并实现指定的接口。类的实例是用@racket[instantiate]表或@racket[make-object]过程创建的，如@secref["objcreation"]中所描述的那样。

@;{Each @racket[class-clause] is (partially) macro-expanded to reveal its
shapes. If a @racket[class-clause] is a @racket[begin] expression, its
sub-expressions are lifted out of the @racket[begin] and treated as
@racket[class-clause]s, in the same way that @racket[begin] is
flattened for top-level and embedded definitions.}
每个@racket[class-clause]是（部分地）宏展开以显示其形状。如果一个@racket[class-clause]是一个@racket[begin]表达式，它的子表达式将从@racket[begin]就被取消，并被当作@racket[class-clause]处理，同样地，对于顶级和嵌入的定义来说，@racket[begin]是扁平的。

@;{Within a @racket[class*] form for instances of the new class,
@racket[this] is bound to the object itself;
@racket[this%] is bound to the class of the object;
@racket[super-instantiate], @racket[super-make-object], and
@racket[super-new] are bound to forms to initialize fields in the
superclass (see @secref["objcreation"]); @racket[super] is
available for calling superclass methods (see
@secref["clmethoddefs"]); and @racket[inner] is available for
calling subclass augmentations of methods (see
@secref["clmethoddefs"]).}
在一个@racket[class*]表用于新类的实例中，@racket[this]被绑定到对象本身；@racket[this%]被绑定到对象的类；@racket[super-instantiate]、@racket[super-make-object]和@racket[super-new]被绑定到基类（参见@secref["objcreation"]）中的初始化字段的表；@racket[super]可用于调用基类方法（见@secref["clmethoddefs"]）；@racket[inner]可供调用方法（见@secref["clmethoddefs"]）的子类增强。
}

@defform[(class superclass-expr class-clause ...)]{

@;{Like @racket[class*], but omits the @racket[_interface-expr]s, for the case that none are needed.}
类似于@racket[class*]，但忽略@racket[_interface-expr]，因为不需要。

@examples[
#:eval class-eval
#:no-prompt
(define book-class%
  (class object%
    (field (pages 5))
    (define/public (letters)
      (* pages 500))
    (super-new)))
]}

@defidform[this]{

@;{@index['("self")]{Within} a @racket[class*] form, @racket[this] refers
to the current object (i.e., the object being initialized or whose
method was called). Use outside the body of a @racket[class*] form is
a syntax error.}
@index['("self")]{在}一个@racket[class*]表中，@racket[this]指当前对象（即，正在初始化的对象或其方法被调用）。在外面使用一个@racket[class*]表的主体是一个语法错误。

@examples[
#:eval class-eval
(eval:no-prompt
 (define (describe obj)
   (printf "Hello ~a\n" obj))
 (define table%
   (class object%
     (define/public (describe-self)
       (describe this))
     (super-new))))
(send (new table%) describe-self)
]}

@defidform[this%]{
                  
@;{Within a @racket[class*] form, @racket[this%] refers to the class
of the current object (i.e., the object being initialized or whose
method was called).  Use outside the body of a @racket[class*] form is
a syntax error.}
在@racket[class*]表中，@racket[this%]指当前对象（即，正在初始化的对象或其方法被调用）的类。在外面使用一个@racket[class*]表的主体是一个语法错误。

@examples[
#:eval class-eval
(eval:no-prompt
 (define account%
   (class object% 
     (super-new)
     (init-field balance)
     (define/public (add n)
       (new this% [balance (+ n balance)]))))
 (define savings%
   (class account%
     (super-new)
     (inherit-field balance)
     (define interest 0.04)
     (define/public (add-interest)
       (send this add (* interest balance))))))
(let* ([acct (new savings% [balance 500])]
       [acct (send acct add 500)]
       [acct (send acct add-interest)])
  (printf "Current balance: ~a\n" (get-field balance acct)))
]}

@defclassforms[
  [(inspect inspector-expr) ()]
  [(init init-decl ...) ("clinitvars")
   @examples[#:eval class-eval
     (class object%
       (super-new)
       (init turnip
             [(internal-potato potato)]
             [carrot 'good]
             [(internal-rutabaga rutabaga) 'okay]))]]
  [(init-field init-decl ...) ("clinitvars" "clfields")
   @examples[#:eval class-eval
     (class object%
       (super-new)
       (init-field turkey
                   [(internal-ostrich ostrich)]
                   [chicken 7]
                   [(internal-emu emu) 13]))]]
  [(field field-decl ...) ("clfields")
   @examples[#:eval class-eval
     (class object%
       (super-new)
       (field [minestrone 'ready]
              [(internal-coq-au-vin coq-au-vin) 'stewing]))]]
  [(inherit-field maybe-renamed ...) ("clfields")
   @examples[#:eval class-eval
     (eval:no-prompt
      (define cookbook%
        (class object%
          (super-new)
          (field [recipes '(caldo-verde oyakodon eggs-benedict)]
                 [pages 389]))))
     (class cookbook%
       (super-new)
       (inherit-field recipes
                      [internal-pages pages]))]]
  [* ((init-rest id) (init-rest)) ("clinitvars")
   @examples[#:eval class-eval
     (eval:no-prompt
      (define fruit-basket%
        (class object%
          (super-new)
          (init-rest fruits)
          (displayln fruits))))
     (make-object fruit-basket% 'kiwi 'lychee 'melon)]]
  [(public maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define jumper%
         (class object%
           (super-new)
           (define (skip) 'skip)
           (define (hop) 'hop)
           (public skip [hop jump]))))
      (send (new jumper%) skip)
      (send (new jumper%) jump)]]
  [(pubment maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define runner%
         (class object%
           (super-new)
           (define (run) 'run)
           (define (trot) 'trot)
           (pubment run [trot jog]))))
      (send (new runner%) run)
      (send (new runner%) jog)]]
  [(public-final maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define point%
         (class object%
           (super-new)
           (init-field [x 0] [y 0])
            (define (get-x) x)
           (define (do-get-y) y)
           (public-final get-x [do-get-y get-y]))))
      (send (new point% [x 1] [y 3]) get-y)
      (eval:error
       (class point%
         (super-new)
         (define (get-x) 3.14)
         (override get-x)))]]
  [(override maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define sheep%
         (class object%
           (super-new)
           (define/public (bleat)
             (displayln "baaaaaaaaah")))))
      (eval:no-prompt
       (define confused-sheep%
         (class sheep%
           (super-new)
           (define (bleat)
             (super bleat)
             (displayln "???"))
           (override bleat))))
      (send (new sheep%) bleat)
      (send (new confused-sheep%) bleat)]]
  [(overment maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define turkey%
         (class object%
           (super-new)
           (define/public (gobble)
             (displayln "gobble gobble")))))
      (eval:no-prompt
       (define extra-turkey%
         (class turkey%
           (super-new)
           (define (gobble)
             (super gobble)
             (displayln "gobble gobble gobble")
             (inner (void) gobble))
           (overment gobble))))
      (eval:no-prompt
       (define cyborg-turkey%
         (class extra-turkey%
           (super-new)
           (define/augment (gobble)
             (displayln "110011111011111100010110001011011001100101")))))
      (send (new extra-turkey%) gobble)
      (send (new cyborg-turkey%) gobble)]]
  [(override-final maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define meeper%
         (class object%
           (super-new)
           (define/public (meep)
             (displayln "meep")))))
      (eval:no-prompt
       (define final-meeper%
         (class meeper%
           (super-new)
           (define (meep)
             (super meep)
             (displayln "This meeping ends with me"))
           (override-final meep))))
      (send (new meeper%) meep)
      (send (new final-meeper%) meep)]]
  [(augment maybe-renamed ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define buzzer%
         (class object%
           (super-new)
           (define/pubment (buzz)
             (displayln "bzzzt")
             (inner (void) buzz)))))
      (eval:no-prompt
       (define loud-buzzer%
         (class buzzer%
           (super-new)
           (define (buzz)
             (displayln "BZZZZZZZZZT"))
           (augment buzz))))
      (send (new buzzer%) buzz)
      (send (new loud-buzzer%) buzz)]]
  [(augride maybe-renamed ...) ("clmethoddefs")]
  [(augment-final maybe-renamed ...) ("clmethoddefs")]
  [(private id ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define light%
         (class object%
           (super-new)
           (define on? #t)
           (define (toggle) (set! on? (not on?)))
           (private toggle)
           (define (flick) (toggle))
           (public flick))))
      (eval:error (send (new light%) toggle))
      (send (new light%) flick)]]
  [(abstract id ...) ("clmethoddefs")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define train%
         (class object%
           (super-new)
           (abstract get-speed)
           (init-field [position 0])
           (define/public (move)
             (new this% [position (+ position (get-speed))])))))
      (eval:no-prompt
       (define acela%
         (class train%
           (super-new)
           (define/override (get-speed) 241))))
      (eval:no-prompt
       (define talgo-350%
         (class train%
           (super-new)
           (define/override (get-speed) 330))))
      (eval:error (new train%))
      (send (new acela%) move)]]
  [(inherit maybe-renamed ...) ("classinherit")
    @examples[#:eval class-eval
      (eval:no-prompt
       (define alarm%
         (class object%
           (super-new)
           (define/public (alarm)
             (displayln "beeeeeeeep")))))
      (eval:no-prompt
       (define car-alarm%
         (class alarm%
           (super-new)
           (init-field proximity)
           (inherit alarm)
           (when (< proximity 10)
             (alarm)))))
      (new car-alarm% [proximity 5])]]
  [(inherit/super maybe-renamed ...)  ("classinherit")]
  [(inherit/inner maybe-renamed ...) ("classinherit")]
  [(rename-super renamed ...) ("classinherit")]
  [(rename-inner renamed ...) ("classinherit")]
]

@defstarshorthands[
 public* 
 pubment*
 public-final*
 override*
 overment*
 override-final*
 augment*
 augride*
 augment-final*
 private*
]

@defdefshorthands[
 public pubment public-final override
 overment override-final augment augride
 augment-final private
]


@defform[
(class/derived original-datum
  (name-id super-expr (interface-expr ...) deserialize-id-expr)
  class-clause
  ...)
]{

@;{Like @racket[class*], but includes a sub-expression to be used as the
source for all syntax errors within the class definition. For example,
@racket[define-serializable-class] expands to @racket[class/derived]
so that errors in the body of the class are reported in terms of
@racket[define-serializable-class] instead of @racket[class].}
类似于@racket[class*]，但包含一个子表达式，以用作类定义中所有语法错误的源。例如，@racket[define-serializable-class]扩展到@racket[class/derived]以便该类主体中的错误在@racket[define-serializable-class]术语中报告，而不是在@racket[class]中报告。

@;{The @racket[original-datum] is the original expression to use for
reporting errors.}
@racket[original-datum]是用于报告错误的原始表达式。

@;{The @racket[name-id] is used to name the resulting class; if it
is @racket[#f], the class name is inferred.}
@racket[name-id]是用于命名结果类；如果是@racket[#f]，类名被推断。

@;{The @racket[super-expr], @racket[interface-expr]s, and
@racket[class-clause]s are as for @racket[class*].}
@racket[super-expr]、@racket[interface-expr]和@racket[class-clause]是作为@racket[class*]。

@;{If the @racket[deserialize-id-expr] is not literally @racket[#f], then
a serializable class is generated, and the result is two values
instead of one: the class and a deserialize-info structure produced by
@racket[make-deserialize-info]. The @racket[deserialize-id-expr]
should produce a value suitable as the second argument to
@racket[make-serialize-info], and it should refer to an export whose
value is the deserialize-info structure.}
如果@racket[deserialize-id-expr]不是字面上的@racket[#f]，那么生成一个可序列化的类，并且结果是两个值而不是一个：类和通过@racket[make-deserialize-info]生成的反序列化信息结构。@racket[deserialize-id-expr]应该产生一个合适的值作为给@racket[make-serialize-info]的第二个参数，并且它应该指向一个导出，其值是反序列化信息结构。

@;{Future optional forms may be added to the sequence that currently ends
with @racket[deserialize-id-expr].}
未来可选的表可能会用@racket[deserialize-id-expr]添加到目前最后部分的序列中。
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@;@subsection[#:tag "clinitvars"]{Initialization Variables}
@subsection[#:tag "clinitvars"]{初始化变量}

@;{A class's initialization variables, declared with @racket[init],
@racket[init-field], and @racket[init-rest], are instantiated
for each object of a class. Initialization variables can be used in
the initial value expressions of fields, default value expressions
for initialization arguments, and in initialization expressions.  Only
initialization variables declared with @racket[init-field] can be
accessed from methods; accessing any other initialization variable
from a method is a syntax error.}
用@racket[init]、@racket[init-field]和@racket[init-rest]声明的一个类的初始化变量，被声明给一个类的每个对象。初始化变量可用于字段的初始值表达式、初始化参数的默认值表达式以及初始化表达式中。只有用@racket[init-field]声明的初始化变量可以从方法访问；从方法访问任何其它初始化变量都是一个语法错误。

@;{The values bound to initialization variables are}
绑定到初始化变量的值是

@itemize[

 @item{@;{the arguments provided with @racket[instantiate] or passed to
 @racket[make-object], if the object is created as a direct instance
 of the class; or,}
  如果对象是作为类的一个直接实例创建，参数用@racket[instantiate]提供或传递给@racket[make-object]；或，}

 @item{@;{the arguments passed to the superclass initialization form or
 procedure, if the object is created as an instance of a derived
 class.}
 如果对象被创建为一个派生类的一个实例，参数传递给基类的初始化表或程序。}

]

@;{If an initialization argument is not provided for an initialization
variable that has an associated @racket[_default-value-expr], then the
@racket[_default-value-expr] expression is evaluated to obtain a value
for the variable. A @racket[_default-value-expr] is only evaluated when
an argument is not provided for its variable. The environment of
@racket[_default-value-expr] includes all of the initialization
variables, all of the fields, and all of the methods of the class. If
multiple @racket[_default-value-expr]s are evaluated, they are
evaluated from left to right. Object creation and field initialization
are described in detail in @secref["objcreation"].}
如果一个初始化参数不提供一个初始化的变量，它有一个关联的@racket[_default-value-expr]，那么@racket[_default-value-expr]表达式被求值以获得一个给变量的值，一个@racket[_default-value-expr]仅在一个参数不提供给它的变量时被求值。@racket[_default-value-expr]的环境包括所有的初始化变量、所有的字段、所有的类方法。如果多个@racket[_default-value-expr]被求值，它们被从左到右。对象创建和字段初始化在@secref["objcreation"]中进行了详细描述。

@;{If an initialization variable has no @racket[_default-value-expr], then
the object creation or superclass initialization call must supply an
argument for the variable, otherwise the @exnraise[exn:fail:object].}
如果一个初始化的变量没有@racket[_default-value-expr]，那么创建对象或基类的初始化调用必须给这个变量提供一个可变参数，否则@exnraise[exn:fail:object]。

@;{Initialization arguments can be provided by name or by position.  The
external name of an initialization variable can be used with
@racket[instantiate] or with the superclass initialization form. Those
forms also accept by-position arguments. The @racket[make-object]
procedure and the superclass initialization procedure accept only
by-position arguments.}
初始化参数可以通过名称或位置提供。一个初始化变量的外部名称可用于@racket[instantiate]或基类的初始化表。这些表也接受通过位置提供的参数。@racket[make-object]程序和基类的初始化程序只接受通过位置提供的参数。

@;{Arguments provided by position are converted into by-name arguments
using the order of @racket[init] and @racket[init-field] clauses and
the order of variables within each clause. When an @racket[instantiate]
form provides both by-position and by-name arguments, the converted
arguments are placed before by-name arguments. (The order can be
significant; see also @secref["objcreation"].)}
通过位置提供的参数用@racket[init]和@racket[init-field]子句的顺序和每个子句中的变量顺序转换成通过名字提供的参数。当一个@racket[instantiate]表既提供按位置的参数也提供按名称的参数时，转换的参数将被放置在按名称的参数之前（顺序可能很重要，请参见@secref["objcreation"]。）

@;{Unless a class contains an @racket[init-rest] clause, when the number
of by-position arguments exceeds the number of declared initialization
variables, the order of variables in the superclass (and so on, up the
superclass chain) determines the by-name conversion.}
除非一个类包含一个@racket[init-rest]子句，当由位置提供的参数数量超过声明初始化变量的个数，基类中的变量顺序（等等，在基类上连续）确定通过名称的转换。

@;{If a class expression contains an @racket[init-rest] clause, there
must be only one, and it must be last. If it declares a variable, then
the variable receives extra by-position initialization arguments as a
list (similar to a dotted ``rest argument'' in a procedure).  An
@racket[init-rest] variable can receive by-position initialization
arguments that are left over from a by-name conversion for a derived
class. When a derived class's superclass initialization provides even
more by-position arguments, they are prefixed onto the by-position
arguments accumulated so far.}
如果一个类表达式包含一个@racket[init-rest]子句，那么必须只有一个，并且它必须是最后一个。如果它声明了一个变量，那么这个变量会接受额外的位置初始化参数作为一个列表（类似于一个过程中的一个带“rest argument”点缀的参数）。一个@racket[init-rest]变量可以接受位置初始化参数，它从一个名称参数转化给一个派生类。当一个派生类的基类初始化提供更多位置参数，它们被前缀上积累更多的位置参数。

@;{If too few or too many by-position initialization arguments are
provided to an object creation or superclass initialization, then the
@exnraise[exn:fail:object]. Similarly, if extra by-position arguments
are provided to a class with an @racket[init-rest] clause, the
@exnraise[exn:fail:object].}
如果太少或者太多的位置初始化参数被提供给一个对象创建或基类初始化，那么@exnraise[exn:fail:object]。同样，如果额外的位置参数提供给了一个带@racket[init-rest]子句的类，那么@exnraise[exn:fail:object]。

@;{Unused (by-name) arguments are to be propagated to the superclass, as
described in @secref["objcreation"].  Multiple initialization
arguments can use the same name if the class derivation contains
multiple declarations (in different classes) of initialization
variables with the name. See @secref["objcreation"] for further
details.}
未使用的（名字）参数是被传播给基类，作为描述参见@secref["objcreation"]。如果类派生包含多个带有名称的初始化变量的声明（在不同的类中），则多个初始化参数可以使用相同的名称。有关更详细的信息，请参见@secref["objcreation"]。

@;{See also @secref["extnames"] for information about internal and
external names.}
有关内部和外部名称的信息，也可以参阅@secref["extnames"]。

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@;{@subsection[#:tag "clfields"]{Fields}}
@subsection[#:tag "clfields"]{字段}

@;??????????????????????????????????????????????????????????????
Each @racket[field], @racket[init-field], and non-method
@racket[define-values] clause in a class declares one or more new
fields for the class. Fields declared with @racket[field] or
@racket[init-field] are public. Public fields can be accessed and
mutated by subclasses using @racket[inherit-field]. Public fields are
also accessible outside the class via @racket[class-field-accessor]
and mutable via @racket[class-field-mutator] (see
@secref["ivaraccess"]). Fields declared with @racket[define-values]
are accessible only within the class.

A field declared with @racket[init-field] is both a public field and
an initialization variable. See @secref["clinitvars"] for
information about initialization variables.

An @racket[inherit-field] declaration makes a public field defined by
a superclass directly accessible in the class expression. If the
indicated field is not defined in the superclass, the
@exnraise[exn:fail:object] when the class expression is evaluated.
Every field in a superclass is present in a derived class, even if it
is not declared with @racket[inherit-field] in the derived class. The
@racket[inherit-field] clause does not control inheritance, but merely
controls lexical scope within a class expression.

When an object is first created, all of its fields have the
@|undefined-const| value (see @secref["void"]). The fields of a
class are initialized at the same time that the class's initialization
expressions are evaluated; see @secref["objcreation"] for more
information.

See also @secref["extnames"] for information about internal and
external names.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsection[#:tag "clmethods"]{Methods}

@subsubsection[#:tag "clmethoddefs"]{Method Definitions}

Each @racket[public], @racket[override], @racket[augment],
@racket[pubment], @racket[overment], @racket[augride],
@racket[public-final], @racket[override-final],
@racket[augment-final], and @racket[private]
clause in a class declares one or more method names. Each method name
must have a corresponding @racket[_method-definition]. The order of
@racket[public], @|etc|, clauses and their corresponding definitions
(among themselves, and with respect to other clauses in the class)
does not matter.

As shown in the grammar for @racket[class*], a method definition is
syntactically restricted to certain procedure forms, as defined by the
grammar for @racket[_method-procedure]; in the last two forms of
@racket[_method-procedure], the body @racket[id] must be one of the
@racket[id]s bound by @racket[let-values] or @racket[letrec-values]. A
@racket[_method-procedure] expression is not evaluated
directly. Instead, for each method, a class-specific method procedure
is created; it takes an initial object argument, in addition to the
arguments the procedure would accept if the @racket[_method-procedure]
expression were evaluated directly. The body of the procedure is
transformed to access methods and fields through the object argument.

A method declared with @racket[public], @racket[pubment], or
@racket[public-final] introduces a new method into a class. The method
must not be present already in the superclass, otherwise the
@exnraise[exn:fail:object] when the class expression is evaluated. A
method declared with @racket[public] can be overridden in a subclass
that uses @racket[override], @racket[overment], or
@racket[override-final].  A method declared with @racket[pubment] can
be augmented in a subclass that uses @racket[augment],
@racket[augride], or @racket[augment-final]. A method declared with
@racket[public-final] cannot be overridden or augmented in a subclass.

A method declared with @racket[override], @racket[overment], or
@racket[override-final] overrides a definition already present in the
superclass. If the method is not already present, the
@exnraise[exn:fail:object] when the class expression is evaluated.  A
method declared with @racket[override] can be overridden again in a
subclass that uses @racket[override], @racket[overment], or
@racket[override-final].  A method declared with @racket[overment] can
be augmented in a subclass that uses @racket[augment],
@racket[augride], or @racket[augment-final]. A method declared with
@racket[override-final] cannot be overridden further or augmented in a
subclass.

A method declared with @racket[augment], @racket[augride], or
@racket[augment-final] augments a definition already present in the
superclass. If the method is not already present, the
@exnraise[exn:fail:object] when the class expression is evaluated.  A
method declared with @racket[augment] can be augmented further in a
subclass that uses @racket[augment], @racket[augride], or
@racket[augment-final]. A method declared with @racket[augride] can be
overridden in a subclass that uses @racket[override],
@racket[overment], or @racket[override-final]. (Such an override
merely replaces the augmentation, not the method that is augmented.)
A method declared with @racket[augment-final] cannot be overridden or
augmented further in a subclass.

A method declared with @racket[private] is not accessible outside the
class expression, cannot be overridden, and never overrides a method
in the superclass.

When a method is declared with @racket[override], @racket[overment],
or @racket[override-final], then the superclass implementation of the
method can be called using @racket[super] form.

When a method is declared with @racket[pubment], @racket[augment], or
@racket[overment], then a subclass augmenting method can be called
using the @racket[inner] form. The only difference between
@racket[public-final] and @racket[pubment] without a corresponding
@racket[inner] is that @racket[public-final] prevents the declaration
of augmenting methods that would be ignored.

A method declared with @racket[abstract] must be declared without
an implementation. Subclasses may implement abstract methods via the
@racket[override], @racket[overment], or @racket[override-final]
forms. Any class that contains or inherits any abstract methods is
considered abstract and cannot be instantiated.

@defform*[[(super id arg ...)
           (super id arg ... . arg-list-expr)]]{

Always accesses the superclass method, independent of whether the
method is overridden again in subclasses. Using the @racket[super]
form outside of @racket[class*] is a syntax error. Each @racket[arg]
is as for @racket[#%app]: either @racket[_arg-expr] or
@racket[_keyword _arg-expr].

The second form is analogous to using @racket[apply] with a procedure;
the @racket[arg-list-expr] must not be a parenthesized expression.}

@defform*[[(inner default-expr id arg ...)
           (inner default-expr id arg ... . arg-list-expr)]]{

If the object's class does not supply an augmenting method, then
@racket[default-expr] is evaluated, and the @racket[arg] expressions
are not evaluated. Otherwise, the augmenting method is called with the
@racket[arg] results as arguments, and @racket[default-expr] is not
evaluated. If no @racket[inner] call is evaluated for a particular
method, then augmenting methods supplied by subclasses are never
used. Using the @racket[inner] form outside of @racket[class*] is an
syntax error.

The second form is analogous to using @racket[apply] with a procedure;
the @racket[arg-list-expr] must not be a parenthesized expression.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsubsection[#:tag "classinherit"]{Inherited and Superclass Methods}

Each @racket[inherit], @racket[inherit/super], @racket[inherit/inner],
@racket[rename-super], and @racket[rename-inner] clause declares one
or more methods that are defined in the class, but must be present in
the superclass. The @racket[rename-super] and @racket[rename-inner]
declarations are rarely used, since @racket[inherit/super] and
@racket[inherit/inner] provide the same access. Also, superclass and
augmenting methods are typically accessed through @racket[super] and
@racket[inner] in a class that also declares the methods, instead of
through @racket[inherit/super], @racket[inherit/inner],
@racket[rename-super], or @racket[rename-inner].

Method names declared with @racket[inherit], @racket[inherit/super],
or @racket[inherit/inner] access overriding declarations, if any, at
run time. Method names declared with @racket[inherit/super] can also
be used with the @racket[super] form to access the superclass
implementation, and method names declared with @racket[inherit/inner]
can also be used with the @racket[inner] form to access an augmenting
method, if any.
 
Method names declared with @racket[rename-super] always access the
superclass's implementation at run-time. Methods declared with
@racket[rename-inner] access a subclass's augmenting method, if any,
and must be called with the form

@racketblock[
(_id (lambda () _default-expr) _arg ...)
]

so that a @racket[default-expr] is available to evaluate when no
augmenting method is available. In such a form, @racket[lambda] is a
literal identifier to separate the @racket[default-expr] from the
@racket[arg]. When an augmenting method is available, it receives the
results of the @racket[arg] expressions as arguments.

Methods that are present in the superclass but not declared with
@racket[inherit], @racket[inherit/super], or @racket[inherit/inner] or
@racket[rename-super] are not directly accessible in the class
(though they can be called with @racket[send]).  Every public method
in a superclass is present in a derived class, even if it is not
declared with @racket[inherit] in the derived class; the
@racket[inherit] clause does not control inheritance, but merely
controls lexical scope within a class expression.

If a method declared with @racket[inherit], @racket[inherit/super],
@racket[inherit/inner], @racket[rename-super], or
@racket[rename-inner] is not present in the superclass, the
@exnraise[exn:fail:object] when the class expression is evaluated.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsubsection[#:tag "extnames"]{Internal and External Names}

Each method declared with @racket[public], @racket[override],
@racket[augment], @racket[pubment], @racket[overment],
@racket[augride], @racket[public-final], @racket[override-final],
@racket[augment-final], @racket[inherit], @racket[inherit/super],
@racket[inherit/inner], @racket[rename-super], and
@racket[rename-inner] can have separate internal and external names
when @racket[(internal-id external-id)] is used for declaring the
method. The internal name is used to access the method directly within
the class expression (including within @racket[super] or
@racket[inner] forms), while the external name is used with
@racket[send] and @racket[generic] (see @secref["ivaraccess"]).  If
a single @racket[id] is provided for a method declaration, the
identifier is used for both the internal and external names.

Method inheritance, overriding, and augmentation are based on external
names only.  Separate internal and external names are required for
@racket[rename-super] and @racket[rename-inner] (for historical
reasons, mainly).

Each @racket[init], @racket[init-field], @racket[field], or
@racket[inherit-field] variable similarly has an internal and an
external name. The internal name is used within the class to access
the variable, while the external name is used outside the class when
providing initialization arguments (e.g., to @racket[instantiate]),
inheriting a field, or accessing a field externally (e.g., with
@racket[class-field-accessor]). As for methods, when inheriting a
field with @racket[inherit-field], the external name is matched to an
external field name in the superclass, while the internal name is
bound in the @racket[class] expression.

A single identifier can be used as an internal identifier and an
external identifier, and it is possible to use the same identifier as
internal and external identifiers for different bindings. Furthermore,
within a single class, a single name can be used as an external method
name, an external field name, and an external initialization argument
name. Overall, each internal identifier must be distinct from all
other internal identifiers, each external method name must be distinct
from all other method names, each external field name must be distinct
from all other field names, and each initialization argument name must
be distinct from all other initialization argument names.

By default, external names have no lexical scope, which means, for
example, that an external method name matches the same syntactic
symbol in all uses of @racket[send]. The
@racket[define-local-member-name] and @racket[define-member-name] forms
introduce scoped external names.

When a @racket[class] expression is compiled, identifiers used in
place of external names must be symbolically distinct (when the
corresponding external names are required to be distinct), otherwise a
syntax error is reported. When no external name is bound by
@racket[define-member-name], then the actual external names are
guaranteed to be distinct when @racket[class] expression is evaluated.
When any external name is bound by @racket[define-member-name], the
@exnraise[exn:fail:object] by @racket[class] if the actual external
names are not distinct.


@defform[(define-local-member-name id ...)]{

Unless it appears as the top-level definition, binds each @racket[id]
so that, within the scope of the definition, each use of each
@racket[id] as an external name is resolved to a hidden name generated
by the @racket[define-local-member-name] declaration. Thus, methods,
fields, and initialization arguments declared with such external-name
@racket[id]s are accessible only in the scope of the
@racket[define-local-member-name] declaration.  As a top-level
definition, @racket[define-local-member-name] binds @racket[id] to its
symbolic form.

The binding introduced by @racket[define-local-member-name] is a
syntax binding that can be exported and imported with
@racket[module]s. Each evaluation of a
@racket[define-local-member-name] declaration generates a distinct
hidden name (except as a top-level definition). The
@racket[interface->method-names] procedure does not expose hidden
names.

@examples[
#:eval class-eval
(eval:no-prompt
 (define-values (r o)
   (let ()
     (define-local-member-name m)
     (define c% (class object%
                  (define/public (m) 10)
                  (super-new)))
     (define o (new c%))
    
     (values (send o m)
             o))))

r
(eval:error (send o m))
]}


@defform[(define-member-name id key-expr)]{

Maps a single external name to an external name that is determined by
an expression. The value of @racket[key-expr] must be the result of either a
@racket[member-name-key] expression or a @racket[generate-member-key] call.}


@defform[(member-name-key identifier)]{

Produces a representation of the external name for @racket[id] in the
environment of the @racket[member-name-key] expression.}

@defproc[(generate-member-key) member-name-key?]{

Produces a hidden name, just like the binding for
@racket[define-local-member-name].}

@defproc[(member-name-key? [v any/c]) boolean?]{

Returns @racket[#t] for values produced by @racket[member-name-key]
and @racket[generate-member-key], @racket[#f]
otherwise.}

@defproc[(member-name-key=? [a-key member-name-key?] [b-key member-name-key?]) boolean?]{

Produces @racket[#t] if member-name keys @racket[a-key] and
@racket[b-key] represent the same external name, @racket[#f]
otherwise.}


@defproc[(member-name-key-hash-code [a-key member-name-key?]) integer?]{

Produces an integer hash code consistent with
@racket[member-name-key=?]  comparisons, analogous to
@racket[equal-hash-code].}

@examples[
#:eval class-eval
(eval:no-prompt
 (define (make-c% key)
   (define-member-name m key)
   (class object% 
     (define/public (m) 10)
     (super-new))))

(send (new (make-c% (member-name-key m))) m)
(eval:error (send (new (make-c% (member-name-key p))) m))
(send (new (make-c% (member-name-key p))) p)

(eval:no-prompt
 (define (fresh-c%)
   (let ([key (generate-member-key)])
     (values (make-c% key) key)))

 (define-values (fc% key) (fresh-c%)))

(eval:error (send (new fc%) m))
(let ()
  (define-member-name p key)
  (send (new fc%) p))
]


@; ------------------------------------------------------------------------

@section[#:tag "objcreation"]{Creating Objects}

The @racket[make-object] procedure creates a new object with
by-position initialization arguments, the @racket[new] form
creates a new object with by-name initialization arguments, and
the @racket[instantiate] form creates a new object with both
by-position and by-name initialization arguments.


All fields in the newly created object are initially bound to the
special @|undefined-const| value (see
@secref["void"]). Initialization variables with default value
expressions (and no provided value) are also initialized to
@|undefined-const|. After argument values are assigned to
initialization variables, expressions in @racket[field] clauses,
@racket[init-field] clauses with no provided argument,
@racket[init] clauses with no provided argument, private field
definitions, and other expressions are evaluated. Those
expressions are evaluated as they appear in the class expression,
from left to right.

Sometime during the evaluation of the expressions,
superclass-declared initializations must be evaluated once by
using the @racket[super-make-object] procedure,
@racket[super-new] form, or @racket[super-instantiate] form.

By-name initialization arguments to a class that have no matching
initialization variable are implicitly added as by-name arguments
to a @racket[super-make-object], @racket[super-new], or
@racket[super-instantiate] invocation, after the explicit
arguments.  If multiple initialization arguments are provided for
the same name, the first (if any) is used, and the unused
arguments are propagated to the superclass. (Note that converted
by-position arguments are always placed before explicit by-name
arguments.)  The initialization procedure for the
@racket[object%] class accepts zero initialization arguments; if
it receives any by-name initialization arguments, then
@exnraise[exn:fail:object].

If the end of initialization is reached for any class in the
hierarchy without invoking the superclass's initialization, the
@exnraise[exn:fail:object]. Also, if superclass initialization is
invoked more than once, the @exnraise[exn:fail:object].

Fields inherited from a superclass are not initialized until the
superclass's initialization procedure is invoked. In contrast,
all methods are available for an object as soon as the object is
created; the overriding of methods is not affected by
initialization (unlike objects in C++).



@defproc[(make-object [class class?] [init-v any/c] ...) object?]{

Creates an instance of @racket[class]. The @racket[init-v]s are
passed as initialization arguments, bound to the initialization
variables of @racket[class] for the newly created object as
described in @secref["clinitvars"]. If @racket[class] is not a
class, the @exnraise[exn:fail:contract].}

@defform[(new class-expr (id by-name-expr) ...)]{

Creates an instance of the value of @racket[class-expr] (which
must be a class), and the value of each @racket[by-name-expr] is
provided as a by-name argument for the corresponding
@racket[id].}

@defform[(instantiate class-expr (by-pos-expr ...) (id by-name-expr) ...)]{

Creates an instance of the value of @racket[class-expr] (which
must be a class), and the values of the @racket[by-pos-expr]s are
provided as by-position initialization arguments. In addition,
the value of each @racket[by-name-expr] is provided as a by-name
argument for the corresponding @racket[id].}

@defidform[super-make-object]{

Produces a procedure that takes by-position arguments an invokes
superclass initialization. See @secref["objcreation"] for more
information.}


@defform[(super-instantiate (by-pos-expr ...) (id by-expr ...) ...)]{


Invokes superclass initialization with the specified by-position and
by-name arguments. See @secref["objcreation"] for more
information.}


@defform[(super-new (id by-name-expr ...) ...)]{

Invokes superclass initialization with the specified by-name
arguments. See @secref["objcreation"] for more information.}

@; ------------------------------------------------------------------------

@section[#:tag "ivaraccess"]{Field and Method Access}

In expressions within a class definition, the initialization
variables, fields, and methods of the class are all part of the
environment. Within a method body, only the fields and other methods
of the class can be referenced; a reference to any other
class-introduced identifier is a syntax error.  Elsewhere within the
class, all class-introduced identifiers are available, and fields and
initialization variables can be mutated with @racket[set!].

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection[#:tag "methodcalls"]{Methods}

Method names used within a class can only be used in the procedure position
of an application expression; any other use is a syntax error.

To allow methods to be applied to lists of arguments, a method
application can have the following form:

@specsubform[
(method-id arg ... . arg-list-expr)
]

This form calls the method in a way analogous to @racket[(apply
_method-id _arg ... _arg-list-expr)]. The @racket[arg-list-expr]
must not be a parenthesized expression.

Methods are called from outside a class with the @racket[send],
@racket[send/apply], and @racket[send/keyword-apply] forms.

@defform*[[(send obj-expr method-id arg ...)
           (send obj-expr method-id arg ... . arg-list-expr)]]{

Evaluates @racket[obj-expr] to obtain an object, and calls the method
with (external) name @racket[method-id] on the object, providing the
@racket[arg] results as arguments. Each @racket[arg] is as for
@racket[#%app]: either @racket[_arg-expr] or @racket[_keyword
_arg-expr]. In the second form, @racket[arg-list-expr] cannot be a
parenthesized expression.

If @racket[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract]. If the object has no public method named
@racket[method-id], the @exnraise[exn:fail:object].}

@defform[(send/apply obj-expr method-id arg ... arg-list-expr)]{

Like the dotted form of @racket[send], but @racket[arg-list-expr] can
be any expression.}

@defform[(send/keyword-apply obj-expr method-id 
                             keyword-list-expr value-list-expr 
                             arg ... arg-list-expr)]{

Like @racket[send/apply], but with expressions for keyword and
argument lists like @racket[keyword-apply].}

@defproc[(dynamic-send [obj object?] 
                       [method-name symbol?]
                       [v any/c] ...
                       [#:<kw> kw-arg any/c] ...) any]{

Calls the method on @racket[obj] whose name matches
@racket[method-name], passing along all given @racket[v]s and
@racket[kw-arg]s.}


@defform/subs[(send* obj-expr msg ...+)
              ([msg (method-id arg ...)
                    (method-id arg ... . arg-list-expr)])]{

Calls multiple methods (in order) of the same object. Each
@racket[msg] corresponds to a use of @racket[send].

For example,

@racketblock[
(send* edit (begin-edit-sequence)
            (insert "Hello")
            (insert #\newline)
            (end-edit-sequence))
]

is the same as

@racketblock[
(let ([o edit])
  (send o begin-edit-sequence)
  (send o insert "Hello")
  (send o insert #\newline)
  (send o end-edit-sequence))
]}

@defform/subs[(send+ obj-expr msg ...)
              ([msg (method-id arg ...)
                    (method-id arg ... . arg-list-expr)])]{

Calls methods (in order) starting with the object produced by
@racket[obj-expr]. Each method call will be invoked on the result of
the last method call, which is expected to be an object. Each
@racket[msg] corresponds to a use of @racket[send].

This is the functional analogue of @racket[send*].

@examples[#:eval class-eval
(eval:no-prompt
 (define point%
   (class object%
     (super-new)
     (init-field [x 0] [y 0])
     (define/public (move-x dx)
       (new this% [x (+ x dx)]))
     (define/public (move-y dy)
       (new this% [y (+ y dy)])))))

(send+ (new point%)
       (move-x 5)
       (move-y 7)
       (move-x 12))
]}

@defform[(with-method ((id (obj-expr method-id)) ...)
           body ...+)]{

Extracts methods from an object and binds a local name that can be
applied directly (in the same way as declared methods within a class)
for each method. Each @racket[obj-expr] must produce an object,
which must have a public method named by the corresponding
@racket[method-id]. The corresponding @racket[id] is bound so that it
can be applied directly (see @secref["methodcalls"]).

Example:

@racketblock[
(let ([s (new stack%)])
  (with-method ([push (s push!)]
                [pop (s pop!)])
    (push 10)
    (push 9)
    (pop)))
]

is the same as

@racketblock[
(let ([s (new stack%)])
  (send s push! 10)
  (send s push! 9)
  (send s pop!))
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Fields}

@defform[(get-field id obj-expr)]{

Extracts the field with (external) name @racket[id] from the value of
@racket[obj-expr].

If @racket[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract]. If the object has no @racket[id] field,
the @exnraise[exn:fail:object].}

@defproc[(dynamic-get-field [field-name symbol?] [obj object?]) any/c]{

Extracts the field from @racket[obj] with the (external) name that
matches @racket[field-name]. If the object has no field matching @racket[field-name],
the @exnraise[exn:fail:object].}

@defform[(set-field! id obj-expr expr)]{

Sets the field with (external) name @racket[id] from the value of
@racket[obj-expr] to the value of @racket[expr].

If @racket[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract].  If the object has no @racket[id] field,
the @exnraise[exn:fail:object].}

@defproc[(dynamic-set-field! [field-name symbol?] [obj object?] [v any/c]) void?]{

Sets the field from @racket[obj] with the (external) name that
matches @racket[field-name] to @racket[v]. If the object has no field matching @racket[field-name],
the @exnraise[exn:fail:object].}

@defform[(field-bound? id obj-expr)]{

Produces @racket[#t] if the object result of @racket[obj-expr] has a
field with (external) name @racket[id], @racket[#f] otherwise.

If @racket[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract].}

@defform[(class-field-accessor class-expr field-id)]{

Returns an accessor procedure that takes an instance of the class
produced by @racket[class-expr] and returns the value of the object's
field with (external) name @racket[field-id].

If @racket[class-expr] does not produce a class, the
@exnraise[exn:fail:contract]. If the class has no @racket[field-id]
field, the @exnraise[exn:fail:object].}

@defform[(class-field-mutator class-expr field-id)]{

Returns a mutator procedure that takes an instance of the class
produced by @racket[class-expr] and a value, and sets the value of the
object's field with (external) name @racket[field-id] to the given
value. The result is @|void-const|.

If @racket[class-expr] does not produce a class, the
@exnraise[exn:fail:contract]. If the class has no @racket[field-id]
field, the @exnraise[exn:fail:object].}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Generics}

A @deftech{generic} can be used instead of a method name to avoid the
cost of relocating a method by name within a class.

@defform[(generic class-or-interface-expr id)]{

Produces a generic that works on instances of the class or interface
produced by @racket[class-or-interface-expr] (or an instance of a
class/interface derived from @racket[class-or-interface]) to call the
method with (external) name @racket[id].

If @racket[class-or-interface-expr] does not produce a class or
interface, the @exnraise[exn:fail:contract]. If the resulting class or
interface does not contain a method named @racket[id], the
@exnraise[exn:fail:object].}

@defform*[[(send-generic obj-expr generic-expr arg ...)
           (send-generic obj-expr generic-expr arg ... . arg-list-expr)]]{

Calls a method of the object produced by @racket[obj-expr] as
indicated by the generic produced by @racket[generic-expr]. Each
@racket[arg] is as for @racket[#%app]: either @racket[_arg-expr] or
@racket[_keyword _arg-expr]. The second form is analogous to calling a
procedure with @racket[apply], where @racket[arg-list-expr] is not a
parenthesized expression.

If @racket[obj-expr] does not produce an object, or if
@racket[generic-expr] does not produce a generic, the
@exnraise[exn:fail:contract]. If the result of @racket[obj-expr] is
not an instance of the class or interface encapsulated by the result
of @racket[generic-expr], the @exnraise[exn:fail:object].}

@defproc[(make-generic [type (or/c class? interface?)]
                       [method-name symbol?])
         generic?]{

Like the @racket[generic] form, but as a procedure that accepts a
symbolic method name.}

@; ------------------------------------------------------------------------

@section[#:tag "mixins"]{Mixins}

@defform[(mixin (interface-expr ...) (interface-expr ...)
           class-clause ...)]{

Produces a @deftech{mixin}, which is a procedure that encapsulates a
class extension, leaving the superclass unspecified.  Each time that a
mixin is applied to a specific superclass, it produces a new derived
class using the encapsulated extension.

The given class must implement interfaces produced by the first set of
@racket[interface-expr]s.  The result of the procedure is a subclass
of the given class that implements the interfaces produced by the
second set of @racket[interface-expr]s. The @racket[class-clause]s are
as for @racket[class*], to define the class extension encapsulated by
the mixin.

Evaluation of a @racket[mixin] form checks that the
@racket[class-clause]s are consistent with both sets of
@racket[interface-expr]s.}

@; ------------------------------------------------------------------------

@section[#:tag "trait"]{Traits}

@note-lib-only[racket/trait]

A @deftech{trait} is a collection of methods that can be converted to
a @tech{mixin} and then applied to a @tech{class}. Before a trait is
converted to a mixin, the methods of a trait can be individually
renamed, and multiple traits can be merged to form a new trait.

@defform/subs[#:literals (public pubment public-final override override-final overment augment augride
                          augment-final private inherit inherit/super inherit/inner rename-super
                          field inherit-field)

              (trait trait-clause ...)
              ([trait-clause (public maybe-renamed ...)
                             (pubment maybe-renamed ...)
                             (public-final maybe-renamed ...)
                             (override maybe-renamed ...)
                             (overment maybe-renamed ...)
                             (override-final maybe-renamed ...)
                             (augment maybe-renamed ...)
                             (augride maybe-renamed ...)
                             (augment-final maybe-renamed ...)
                             (inherit maybe-renamed ...)
                             (inherit/super maybe-renamed ...)
                             (inherit/inner maybe-renamed ...)
                             method-definition
                             (field field-declaration ...)
                             (inherit-field maybe-renamed ...)])]{

Creates a @tech{trait}.  The body of a @racket[trait] form is similar to the
body of a @racket[class*] form, but restricted to non-private method
definitions.  In particular, the grammar of
@racket[maybe-renamed], @racket[method-definition], and
@racket[field-declaration] are the same as for @racket[class*], and
every @racket[method-definition] must have a corresponding declaration
(one of @racket[public], @racket[override], etc.).  As in
@racket[class], uses of method names in direct calls, @racket[super]
calls, and @racket[inner] calls depend on bringing method names into
scope via @racket[inherit], @racket[inherit/super],
@racket[inherit/inner], and other method declarations in the same
trait; an exception, compared to @racket[class] is that
@racket[overment] binds a method name only in the corresponding
method, and not in other methods of the same trait. Finally, macros
such as @racket[public*] and @racket[define/public] work in
@racket[trait] as in @racket[class].

External identifiers in @racket[trait], @racket[trait-exclude],
@racket[trait-exclude-field], @racket[trait-alias],
@racket[trait-rename], and @racket[trait-rename-field] forms are
subject to binding via @racket[define-member-name] and
@racket[define-local-member-name]. Although @racket[private] methods
or fields are not allowed in a @racket[trait] form, they can be
simulated by using a @racket[public] or @racket[field] declaration and
a name whose scope is limited to the @racket[trait] form.}


@defproc[(trait? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a trait, @racket[#f] otherwise.}


@defproc[(trait->mixin [tr trait?]) (class? . -> . class?)]{

Converts a @tech{trait} to a @tech{mixin}, which can be applied to a
@tech{class} to produce a new @tech{class}. An expression of the form

@racketblock[
(trait->mixin
 (trait
   _trait-clause ...))
]

is equivalent to

@racketblock[
(lambda (%)
  (class %
    _trait-clause ...
    (super-new)))
]

Normally, however, a trait's methods are changed and combined with
other traits before converting to a mixin.}


@defproc[(trait-sum [tr trait?] ...+) trait?]{

Produces a @tech{trait} that combines all of the methods of the given
@racket[tr]s. For example,

@racketblock[
(define t1
  (trait
    (define/public (m1) 1)))
(define t2
  (trait
    (define/public (m2) 2)))
(define t3 (trait-sum t1 t2))
]

creates a trait @racket[t3] that is equivalent to

@racketblock[
(trait
  (define/public (m1) 1)
  (define/public (m2) 2))
]

but @racket[t1] and @racket[t2] can still be used individually or
combined with other traits.

When traits are combined with @racket[trait-sum], the combination
drops @racket[inherit], @racket[inherit/super],
@racket[inherit/inner], and @racket[inherit-field] declarations when a
definition is supplied for the same method or field name by another
trait. The @racket[trait-sum] operation fails (the
@exnraise[exn:fail:contract]) if any of the traits to combine define a
method or field with the same name, or if an @racket[inherit/super] or
@racket[inherit/inner] declaration to be dropped is inconsistent with
the supplied definition. In other words, declaring a method with
@racket[inherit], @racket[inherit/super], or @racket[inherit/inner],
does not count as defining the method; at the same time, for example,
a trait that contains an @racket[inherit/super] declaration for a
method @racket[m] cannot be combined with a trait that defines
@racket[m] as @racket[augment], since no class could satisfy the
requirements of both @racket[augment] and @racket[inherit/super] when
the trait is later converted to a mixin and applied to a class.}


@defform[(trait-exclude trait-expr id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@racket[trait-expr], but with the definition of a method named by
@racket[id] removed; as the method definition is removed, either an
@racket[inherit], @racket[inherit/super], or @racket[inherit/inner]
declaration is added:

@itemize[

 @item{A method declared with @racket[public], @racket[pubment], or
  @racket[public-final] is replaced with an @racket[inherit]
  declaration.}

 @item{A method declared with @racket[override] or @racket[override-final]
 is replaced with an @racket[inherit/super] declaration.}

  @item{A method declared with @racket[augment], @racket[augride], or
  @racket[augment-final] is replaced with an @racket[inherit/inner] declaration.}

 @item{A method declared with @racket[overment] is not replaced
  with any @racket[inherit] declaration.}

]

If the trait produced by @racket[trait-expr] has no method definition for
@racket[id], the @exnraise[exn:fail:contract].}


@defform[(trait-exclude-field trait-expr id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@racket[trait-expr], but with the definition of a field named by
@racket[id] removed; as the field definition is removed, an
@racket[inherit-field] declaration is added.}


@defform[(trait-alias trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@racket[trait-expr], but the definition and declaration of the method
named by @racket[id] is duplicated with the name @racket[new-id]. The
consistency requirements for the resulting trait are the same as for
@racket[trait-sum], otherwise the @exnraise[exn:fail:contract]. This
operation does not rename any other use of @racket[id], such as in
method calls (even method calls to @racket[identifier] in the cloned
definition for @racket[new-id]).}


@defform[(trait-rename trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@racket[trait-expr], but all definitions and references to methods
named @racket[id] are replaced by definitions and references to
methods named by @racket[new-id]. The consistency requirements for the
resulting trait are the same as for @racket[trait-sum], otherwise the
@exnraise[exn:fail:contract].}


@defform[(trait-rename-field trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@racket[trait-expr], but all definitions and references to fields
named @racket[id] are replaced by definitions and references to fields
named by @racket[new-id]. The consistency requirements for the
resulting trait are the same as for @racket[trait-sum], otherwise the
@exnraise[exn:fail:contract].}

@; ------------------------------------------------------------------------

@section{Object and Class Contracts}

@defform/subs[
#:literals (field init init-field inherit inherit-field super inner override augment augride absent)

(class/c maybe-opaque member-spec ...)

([maybe-opaque
  (code:line)
  (code:line #:opaque)
  (code:line #:opaque #:ignore-local-member-names)]

 [member-spec
  method-spec
  (field field-spec ...)
  (init field-spec ...)
  (init-field field-spec ...)
  (inherit method-spec ...)
  (inherit-field field-spec ...)
  (super method-spec ...)
  (inner method-spec ...)
  (override method-spec ...)
  (augment method-spec ...)
  (augride method-spec ...)
  (absent absent-spec ...)]
 
 [method-spec
  method-id
  (method-id method-contract-expr)]
 [field-spec
  field-id
  (field-id contract-expr)]
 [absent-spec
  method-id
  (field field-id ...)])]{
Produces a contract for a class.

There are two major categories of contracts listed in a @racket[class/c]
form: external and internal contracts. External contracts govern behavior
when an object is instantiated from a class or when methods or fields are
accessed via an object of that class. Internal contracts govern behavior
when method or fields are accessed within the class hierarchy.  This
separation allows for stronger contracts for class clients and weaker
contracts for subclasses.

Method contracts must contain an additional initial argument which corresponds
to the implicit @racket[this] parameter of the method.  This allows for
contracts which discuss the state of the object when the method is called
(or, for dependent contracts, in other parts of the contract).  Alternative
contract forms, such as @racket[->m], are provided as a shorthand
for writing method contracts.

Methods and fields listed in an @racket[absent] clause must @emph{not} be present in the class.

A class contract can be specified to be @emph{opaque} with the @racket[#:opaque]
keyword. An opaque class contract will only accept a class that defines
exactly the external methods and fields specified by the contract. A contract error
is raised if the contracted class contains any methods or fields that are
not specified. Methods or fields with local member names (i.e., defined with
@racket[define-local-member-name]) are ignored for this check if
@racket[#:ignore-local-member-names] is provided.

The external contracts are as follows:

@itemize[
 @item{An external method contract without a tag describes the behavior
   of the implementation of @racket[method-id] on method sends to an
   object of the contracted class.  This contract will continue to be
   checked in subclasses until the contracted class's implementation is
   no longer the entry point for dynamic dispatch.
   
   If only the field name is present, this is equivalent to insisting only
   that the method is present in the class.
   
   @examples[#:eval class-eval
                (eval:no-prompt
                 (define woody%
                   (class object%
                     (define/public (draw who)
                       (format "reach for the sky, ~a" who))
                     (super-new)))
                
                 (define/contract woody+c%
                   (class/c [draw (->m symbol? string?)])
                   woody%))
                
                (send (new woody%) draw #f)
                (send (new woody+c%) draw 'zurg)
                (eval:error (send (new woody+c%) draw #f))]
   }
 @item{An external field contract, tagged with @racket[field], describes the
   behavior of the value contained in that field when accessed from outside
   the class.  Since fields may be mutated, these contracts
   are checked on any external access (via @racket[get-field])
   and external mutations (via @racket[set-field!]) of the field.

   If only the field name is present, this is equivalent to using the 
   contract @racket[any/c] (but it is checked more efficiently).
   
   @examples[#:eval class-eval
                (eval:no-prompt
                 (define woody/hat%
                   (class woody%
                     (field [hat-location 'uninitialized])
                     (define/public (lose-hat) (set! hat-location 'lost))
                     (define/public (find-hat) (set! hat-location 'on-head))
                     (super-new)))
                 (define/contract woody/hat+c%
                   (class/c [draw (->m symbol? string?)]
                            [lose-hat (->m void?)]
                            [find-hat (->m void?)]
                            (field [hat-location (or/c 'on-head 'lost)]))
                   woody/hat%))
                
                (get-field hat-location (new woody/hat%))
                (let ([woody (new woody/hat+c%)])
                  (send woody lose-hat)
                  (get-field hat-location woody))
                (eval:error (get-field hat-location (new woody/hat+c%)))
                (eval:error
                 (let ([woody (new woody/hat+c%)])
                   (set-field! hat-location woody 'under-the-dresser)))]
   
   }
 @item{An initialization argument contract, tagged with @racket[init],
   describes the expected behavior of the value paired with that name
   during class instantiation.  The same name can be provided more than
   once, in which case the first such contract in the @racket[class/c]
   form is applied to the first value tagged with that name in the list
   of initialization arguments, and so on.
   
   If only the initialization argument name is present, this is equivalent to using the 
   contract @racket[any/c] (but it is checked more efficiently).
   
   @examples[#:eval class-eval
                (eval:no-prompt
                 (define woody/init-hat%
                   (class woody%
                     (init init-hat-location)
                     (field [hat-location init-hat-location])
                     (define/public (lose-hat) (set! hat-location 'lost))
                     (define/public (find-hat) (set! hat-location 'on-head))
                     (super-new)))
                 (define/contract woody/init-hat+c%
                   (class/c [draw (->m symbol? string?)]
                            [lose-hat (->m void?)]
                            [find-hat (->m void?)]
                            (init [init-hat-location (or/c 'on-head 'lost)])
                            (field [hat-location (or/c 'on-head 'lost)]))
                   woody/init-hat%))
                (get-field hat-location
                           (new woody/init-hat+c%
                                [init-hat-location 'lost]))
                (eval:error
                 (get-field hat-location
                            (new woody/init-hat+c%
                                 [init-hat-location 'slinkys-mouth])))]
   
   }
 @item{The contracts listed in an @racket[init-field] section are
   treated as if each contract appeared in an @racket[init] section and
   a @racket[field] section.}
]

The internal contracts restrict the behavior of method calls
made between classes and their subclasses; such calls are not
controlled by the class contracts described above. 

As with the external contracts, when a method or field name is specified
 but no contract appears, the contract is satisfied merely with the
 presence of the corresponding field or method.

@itemize[
 @item{A method contract tagged with @racket[inherit] describes the
   behavior of the method when invoked directly (i.e., via
   @racket[inherit]) in any subclass of the contracted class.  This
   contract, like external method contracts, applies until the
   contracted class's method implementation is no longer the entry point
   for dynamic dispatch.
   
   @examples[#:eval class-eval
                (new (class woody+c%
                       (inherit draw)
                       (super-new)
                       (printf "woody sez: “~a”\n" (draw "evil dr porkchop"))))
                (eval:no-prompt
                 (define/contract woody+c-inherit%
                   (class/c (inherit [draw (->m symbol? string?)]))
                   woody+c%))
                (eval:error
                 (new (class woody+c-inherit%
                        (inherit draw)
                        (printf "woody sez: ~a\n" (draw "evil dr porkchop")))))]
   
   }
  @item{A method contract tagged with @racket[super] describes the behavior of
   @racket[method-id] when called by the @racket[super] form in a
   subclass.  This contract only affects @racket[super] calls in
   subclasses which call the contract class's implementation of
   @racket[method-id].
   
   This example shows how to extend the @racket[draw] method
   so that if it is passed two arguments, it combines two
   calls to the original @racket[draw] method, but with a 
   contract the controls how the @racket[super] methods must
   be invoked.
   
   @examples[#:eval class-eval
                (eval:no-prompt
                 (define/contract woody2+c%
                   (class/c (super [draw (->m symbol? string?)]))
                   (class woody%
                     (define/override draw
                       (case-lambda
                         [(a) (super draw a)]
                         [(a b) (string-append (super draw a)
                                               " and "
                                               (super draw b))]))
                     (super-new))))
                (send (new woody2+c%) draw 'evil-dr-porkchop  'zurg)
                (send (new woody2+c%) draw "evil dr porkchop" "zurg")]
   
   The last call signals an error blaming @racket[woody2%] because
   there is no contract checking the initial @racket[draw] call.
   }
 @item{A method contract tagged with @racket[inner] describes the
   behavior the class expects of an augmenting method in a subclass.
   This contract affects any implementations of @racket[method-id] in
   subclasses which can be called via @racket[inner] from the contracted
   class.  This means a subclass which implements @racket[method-id] via
   @racket[augment] or @racket[overment] stop future subclasses from
   being affected by the contract, since further extension cannot be
   reached via the contracted class.}
 @item{A method contract tagged with @racket[override] describes the
   behavior expected by the contracted class for @racket[method-id] when
   called directly (i.e. by the application @racket[(method-id ...)]).
   This form can only be used if overriding the method in subclasses
   will change the entry point to the dynamic dispatch chain (i.e., the
   method has never been augmentable).
   
   This time, instead of overriding @racket[draw] to support
   two arguments, we can make a new method, @racket[draw2] that
   takes the two arguments and calls @racket[draw]. We also
   add a contract to make sure that overriding @racket[draw]
   doesn't break @racket[draw2].   
   
   @examples[#:eval class-eval
                (eval:no-prompt
                 (define/contract woody2+override/c%
                   (class/c (override [draw (->m symbol? string?)]))
                   (class woody+c%
                     (inherit draw)
                     (define/public (draw2 a b)
                       (string-append (draw a)
                                      " and "
                                      (draw b)))
                     (super-new)))
                
                 (define woody2+broken-draw
                   (class woody2+override/c%
                     (define/override (draw x)
                       'not-a-string)
                     (super-new))))

                (eval:error
                 (send (new woody2+broken-draw) draw2 
                       'evil-dr-porkchop
                       'zurg))]
   
   
   }
 @item{A method contract tagged with either @racket[augment] or
   @racket[augride] describes the behavior provided by the contracted
   class for @racket[method-id] when called directly from subclasses.
   These forms can only be used if the method has previously been
   augmentable, which means that no augmenting or overriding
   implementation will change the entry point to the dynamic dispatch
   chain.  @racket[augment] is used when subclasses can augment the
   method, and @racket[augride] is used when subclasses can override the
   current augmentation.}
 @item{A field contract tagged with @racket[inherit-field] describes
   the behavior of the value contained in that field when accessed
   directly (i.e., via @racket[inherit-field]) in any subclass of the
   contracted class.  Since fields may be mutated, these contracts are
   checked on any access and/or mutation of the field that occurs in
   such subclasses.}

@history[#:changed "6.1.1.8"
         @string-append{Opaque class/c now optionally ignores local
                        member names if an additional keyword is supplied.}]
]}

@defform[(absent absent-spec ...)]{
See @racket[class/c]; use outside of a @racket[class/c] form is a syntax error.
}

@defform[(->m dom ... range)]{
Similar to @racket[->], except that the domain of the resulting contract
contains one more element than the stated domain, where the first
(implicit) argument is contracted with @racket[any/c].  This contract is
useful for writing simpler method contracts when no properties of
@racket[this] need to be checked.}

@defform[(->*m (mandatory-dom ...) (optional-dom ...) rest range)]{
Similar to @racket[->*], except that the mandatory domain of the
resulting contract contains one more element than the stated domain,
where the first (implicit) argument is contracted with
@racket[any/c]. This contract is useful for writing simpler method
contracts when no properties of @racket[this] need to be checked.}

@defform[(case->m (-> dom ... rest range) ...)]{
Similar to @racket[case->], except that the mandatory domain of each
case of the resulting contract contains one more element than the stated
domain, where the first (implicit) argument is contracted with
@racket[any/c]. This contract is useful for writing simpler method
contracts when no properties of @racket[this] need to be checked.}

@defform[(->dm (mandatory-dependent-dom ...)
               (optional-dependent-dom ...)
               dependent-rest
               pre-cond
               dep-range)]{
Similar to @racket[->d], except that the mandatory domain of the resulting contract
contains one more element than the stated domain, where the first (implicit) argument is contracted
with @racket[any/c]. In addition, @racket[this] is appropriately bound in the body of the contract.
This contract is useful for writing simpler method contracts when no properties
of @racket[this] need to be checked.}

@defform/subs[
#:literals (field)

(object/c member-spec ...)

([member-spec
  method-spec
  (field field-spec ...)]
 
 [method-spec
  method-id
  (method-id method-contract)]
 [field-spec
  field-id
  (field-id contract-expr)])]{
Produces a contract for an object.

Unlike the older form @racket[object-contract], but like
@racket[class/c], arbitrary contract expressions are allowed.
Also, method contracts for @racket[object/c] follow those for
@racket[class/c].  An object wrapped with @racket[object/c]
behaves as if its class had been wrapped with the equivalent
@racket[class/c] contract.
}

@defproc[(instanceof/c [class-contract contract?]) contract?]{
Produces a contract for an object, where the object is an
instance of a class that conforms to @racket[class-contract].
}

@defproc[(dynamic-object/c [method-names (listof symbol?)]
                           [method-contracts (listof contract?)]
                           [field-names (listof symbol?)]
                           [field-contracts (listof contract?)])
         contract?]{
Produces a contract for an object, similar to @racket[object/c] but
where the names and contracts for both methods and fields can be
computed dynamically. The list of names and contracts for both
methods and field respectively must have the same lengths.
}

@defform/subs[
#:literals (field -> ->* ->d)

(object-contract member-spec ...)

([member-spec
  (method-id method-contract)
  (field field-id contract-expr)]

 [method-contract
  (-> dom ... range)
  (->* (mandatory-dom ...)
       (optional-dom ...)
       rest
       range)
  (->d (mandatory-dependent-dom ...) 
       (optional-dependent-dom ...) 
       dependent-rest
       pre-cond
       dep-range)]

 [dom dom-expr (code:line keyword dom-expr)]
 [range range-expr (values range-expr ...) any]
 [mandatory-dom dom-expr (code:line keyword dom-expr)]
 [optional-dom dom-expr (code:line keyword dom-expr)]
 [rest (code:line) (code:line #:rest rest-expr)]
 [mandatory-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [optional-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [dependent-rest (code:line) (code:line #:rest id rest-expr)]
 [pre-cond (code:line) (code:line #:pre-cond boolean-expr)]
 [dep-range any
            (code:line [id range-expr] post-cond)
            (code:line (values [id range-expr] ...) post-cond)]
 [post-cond (code:line) (code:line #:post-cond boolean-expr)]
)]{

Produces a contract for an object.

Each of the contracts for a method has the same semantics as
the corresponding function contract, but the syntax of the
method contract must be written directly in the body of the
object-contract---much like the way that methods in class
definitions use the same syntax as regular function
definitions, but cannot be arbitrary procedures.  Unlike the
method contracts for @racket[class/c], the implicit @racket[this]
argument is not part of the contract.  To allow for the use of
@racket[this] in dependent contracts, @racket[->d] contracts
implicitly bind @racket[this] to the object itself.}


@defthing[mixin-contract contract?]{

A @tech{function contract} that recognizes mixins. It guarantees that
the input to the function is a class and the result of the function is
a subclass of the input.}

@defproc[(make-mixin-contract [type (or/c class? interface?)] ...) contract?]{

Produces a @tech{function contract} that guarantees the input to the
function is a class that implements/subclasses each @racket[type], and
that the result of the function is a subclass of the input.}

@defproc[(is-a?/c [type (or/c class? interface?)]) flat-contract?]{

Accepts a class or interface and returns a flat contract that
recognizes objects that instantiate the class/interface.

See @racket[is-a?].}

@defproc[(implementation?/c [interface interface?]) flat-contract?]{

Returns a flat contract that recognizes classes that implement
@racket[interface].

See @racket[implementation?].}

@defproc[(subclass?/c [class class?]) flat-contract?]{

Returns a flat contract that recognizes classes that
are subclasses of @racket[class].

See @racket[subclass?].}

@; ------------------------------------------------------------------------

@section[#:tag "objectequality"]{Object Equality and Hashing}

By default, objects that are instances of different classes or that
are instances of a non-transparent class are @racket[equal?] only if
they are @racket[eq?]. Like transparent structures, two objects that
are instances of the same transparent class (i.e., every superclass of
the class has @racket[#f] as its inspector) are @racket[equal?] when
their field values are @racket[equal?].

To customize the way that a class instance is compared to other
instances by @racket[equal?], implement the @racket[equal<%>]
interface.

@definterface[equal<%> ()]{

The @racket[equal<%>] interface includes three methods, which are
analogous to the functions provided for a structure type with
@racket[prop:equal+hash]:

@itemize[

 @item{@racket[equal-to?] --- Takes two arguments. The first argument
 is an object that is an instance of the same class (or a subclass
 that does not re-declare its implementation of @racket[equal<%>])
 and that is being compared to the target object. The second argument
 is an @racket[equal?]-like procedure of two arguments that should be
 used for recursive equality testing. The result should be a true
 value if the object and the first argument of the method are equal,
 @racket[#f] otherwise.}

 @item{@racket[equal-hash-code-of] --- Takes one argument, which is a
 procedure of one argument that should be used for recursive hash-code
 computation. The result should be an exact integer representing the
 target object's hash code.}

 @item{@racket[equal-secondary-hash-code-of] --- Takes one argument,
 which is a procedure of one argument that should be used for
 recursive hash-code computation. The result should be an exact
 integer representing the target object's secondary hash code.}

]

The @racket[equal<%>] interface is unusual in that declaring the
implementation of the interface is different from inheriting the
interface. Two objects can be equal only if they are instances of
classes whose most specific ancestor to explicitly implement
@racket[equal<%>] is the same ancestor.

See @racket[prop:equal+hash] for more information on equality
comparisons and hash codes. The @racket[equal<%>] interface is
implemented with @racket[interface*] and @racket[prop:equal+hash].}

Example:
@codeblock|{
#lang racket

;; Case insensitive words:
(define ci-word% 
  (class* object% (equal<%>)
    
    ;; Initialization
    (init-field word)
    (super-new)
        
    ;; We define equality to ignore case:
    (define/public (equal-to? other recur)
      (string-ci=? word (get-field word other)))

    ;; The hash codes need to be insensitive to casing as well.
    ;; We'll just downcase the word and get its hash code.
    (define/public (equal-hash-code-of hash-code)
      (hash-code (string-downcase word)))
    
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (string-downcase word)))))

;; We can create a hash with a single word:
(define h (make-hash))
(hash-set! h (new ci-word% [word "inconceivable!"]) 'value)

;; Lookup into the hash should be case-insensitive, so that
;; both of these should return 'value.
(hash-ref h (new ci-word% [word "inconceivable!"]))
(hash-ref h (new ci-word% [word "INCONCEIVABLE!"]))

;; Comparison fails if we use a non-ci-word%:
(hash-ref h "inconceivable!" 'i-dont-think-it-means-what-you-think-it-means)
}|

@; ------------------------------------------------------------------------

@section[#:tag "objectserialize"]{Object Serialization}

@defform[
(define-serializable-class* class-id superclass-expr 
                                     (interface-expr ...)
  class-clause ...)
]{

Binds @racket[class-id] to a class, where @racket[superclass-expr],
the @racket[interface-expr]s, and the @racket[class-clause]s are as in
@racket[class*].

This form can only be used at the top level, either within a module
or outside. The @racket[class-id] identifier is bound to the new
class, and @racketidfont{deserialize-info:}@racket[class-id] is also
defined; if the definition is within a module, then the latter is
provided from a @racket[deserialize-info] submodule via @racket[module+].

Serialization for the class works in one of two ways:

@itemize[

 @item{If the class implements the built-in interface
       @racket[externalizable<%>], then an object is serialized by
       calling its @racket[externalize] method; the result can be
       anything that is serializable (but, obviously, should not be
       the object itself). Deserialization creates an instance of the
       class with no initialization arguments, and then calls the
       object's @racket[internalize] method with the result of
       @racket[externalize] (or, more precisely, a deserialized
       version of the serialized result of a previous call).

       To support this form of serialization, the class must be
       instantiable with no initialization arguments. Furthermore,
       cycles involving only instances of the class (and other such
       classes) cannot be serialized.}

 @item{If the class does not implement @racket[externalizable<%>],
       then every superclass of the class must be either serializable
       or transparent (i.e,. have @racket[#f] as its
       inspector). Serialization and deserialization are fully
       automatic, and may involve cycles of instances.

       To support cycles of instances, deserialization may create an
       instance of the call with all fields as the undefined value,
       and then mutate the object to set the field
       values. Serialization support does not otherwise make an
       object's fields mutable.}

]

In the second case, a serializable subclass can implement
@racket[externalizable<%>], in which case the @racket[externalize]
method is responsible for all serialization (i.e., automatic
serialization is lost for instances of the subclass). In the first
case, all serializable subclasses implement
@racket[externalizable<%>], since a subclass implements all of the
interfaces of its parent class.

In either case, if an object is an immediate instance of a subclass
(that is not itself serializable), the object is serialized as if it
was an immediate instance of the serializable class. In particular,
overriding declarations of the @racket[externalize] method are ignored
for instances of non-serializable subclasses.}


@defform[
(define-serializable-class class-id superclass-expr
  class-clause ...)
]{

Like @racket[define-serializable-class*], but without interface
expressions (analogous to @racket[class]).}


@definterface[externalizable<%> ()]{

The @racket[externalizable<%>] interface includes only the
@racket[externalize] and @racket[internalize] methods. See
@racket[define-serializable-class*] for more information.}

@; ------------------------------------------------------------------------

@section[#:tag "objectprinting"]{Object Printing}

To customize the way that a class instance is printed by
@racket[print], @racket[write] and @racket[display], implement the
@racket[printable<%>] interface.

@defthing[printable<%> interface?]{

The @racket[printable<%>] interface includes only the
@racket[custom-print], @racket[custom-write], and
@racket[custom-display] methods. The @racket[custom-print] method
accepts two arguments: the destination port and the current
@racket[quasiquote] depth as an exact nonnegative integer. The
@racket[custom-write] and @racket[custom-display] methods each accepts
a single argument, which is the destination port to @racket[write] or
@racket[display] the object.

Calls to the @racket[custom-print], @racket[custom-write], or
@racket[custom-display] methods are like calls to a procedure attached
to a structure type through the @racket[prop:custom-write]
property. In particular, recursive printing can trigger an escape from
the call.

See @racket[prop:custom-write] for more information. The
@racket[printable<%>] interface is implemented with
@racket[interface*] and @racket[prop:custom-write].}

@defthing[writable<%> interface?]{

Like @racket[printable<%>], but includes only the
@racket[custom-write] and @racket[custom-display] methods.
A @racket[print] request is directed to @racket[custom-write].}

@; ------------------------------------------------------------------------

@section[#:tag "objectutils"]{Object, Class, and Interface Utilities}

@defproc[(object? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an object, @racket[#f] otherwise.

@examples[#:eval class-eval
  (object? (new object%))
  (object? object%)
  (object? "clam chowder")
]}


@defproc[(class? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a class, @racket[#f] otherwise.

@examples[#:eval class-eval
  (class? object%)
  (class? (class object% (super-new)))
  (class? (new object%))
  (class? "corn chowder")
]}


@defproc[(interface? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an interface, @racket[#f] otherwise.

@examples[#:eval class-eval
  (interface? (interface () empty cons first rest))
  (interface? object%)
  (interface? "gazpacho")
]}


@defproc[(generic? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{generic}, @racket[#f] otherwise.

@examples[#:eval class-eval
  (define c%
    (class object%
      (super-new)
      (define/public (m x)
        (+ 3.14 x))))

  (generic? (generic c% m))
  (generic? c%)
  (generic? "borscht")
]}


@defproc[(object=? [a object?] [b object?]) boolean?]{

Determines whether @racket[a] and @racket[b] were returned from
the same call to @racket[new] or not. If the two objects
have fields, this procedure determines whether mutating a field
of one would change that field in the other.

This procedure is similar in spirit to
@racket[eq?] but also works properly with contracts
(and has a stronger guarantee).

@examples[#:eval class-ctc-eval
  (define obj-1 (new object%))
  (define obj-2 (new object%))
  (define/contract obj-3 (object/c) obj-1)
  
  (object=? obj-1 obj-1)
  (object=? obj-1 obj-2)
  (object=? obj-1 obj-3)
  
  (eq? obj-1 obj-1)
  (eq? obj-1 obj-2)
  (eq? obj-1 obj-3)
]}


@defproc[(object-or-false=? [a (or/c object? #f)] [b (or/c object? #f)]) boolean?]{

Like @racket[object=?], but accepts @racket[#f] for either argument and
returns @racket[#t] if both arguments are @racket[#f].

@examples[#:eval class-ctc-eval
   (object-or-false=? #f (new object%))
   (object-or-false=? (new object%) #f)
   (object-or-false=? #f #f)
   ]

@history[#:added "6.1.1.8"]}


@defproc[(object->vector [object object?] [opaque-v any/c #f]) vector?]{

Returns a vector representing @racket[object] that shows its
inspectable fields, analogous to @racket[struct->vector].

@examples[#:eval class-eval
  (object->vector (new object%))
  (object->vector (new (class object%
                         (super-new)
                         (field [x 5] [y 10]))))
]}


@defproc[(class->interface [class class?]) interface?]{

Returns the interface implicitly defined by @racket[class].

@examples[#:eval class-eval
  (class->interface object%)
]}


@defproc[(object-interface [object object?]) interface?]{

Returns the interface implicitly defined by the class of
@racket[object].

@examples[#:eval class-eval
  (object-interface (new object%))
]}

 
@defproc[(is-a? [v any/c] [type (or/c interface? class?)]) boolean?]{

Returns @racket[#t] if @racket[v] is an instance of a class
@racket[type] or a class that implements an interface @racket[type],
@racket[#f] otherwise.

@examples[#:eval class-eval
  (define point<%> (interface () get-x get-y))
  (define 2d-point%
    (class* object% (point<%>)
      (super-new)
      (field [x 0] [y 0])
      (define/public (get-x) x)
      (define/public (get-y) y)))

  (is-a? (new 2d-point%) 2d-point%)
  (is-a? (new 2d-point%) point<%>)
  (is-a? (new object%) 2d-point%)
  (is-a? (new object%) point<%>)
]}


@defproc[(subclass? [v any/c] [cls class?]) boolean?]{

Returns @racket[#t] if @racket[v] is a class derived from (or equal
to) @racket[cls], @racket[#f] otherwise.

@examples[#:eval class-eval
  (subclass? (class object% (super-new)) object%)
  (subclass? object% (class object% (super-new)))
  (subclass? object% object%)
]}


@defproc[(implementation? [v any/c] [intf interface?]) boolean?]{

Returns @racket[#t] if @racket[v] is a class that implements
@racket[intf], @racket[#f] otherwise.

@examples[#:eval class-eval
  (define i<%> (interface () go))
  (define c%
    (class* object% (i<%>)
      (super-new)
      (define/public (go) 'go)))

  (implementation? c% i<%>)
  (implementation? object% i<%>)
]}


@defproc[(interface-extension? [v any/c] [intf interface?]) boolean?]{

Returns @racket[#t] if @racket[v] is an interface that extends
@racket[intf], @racket[#f] otherwise.

@examples[#:eval class-eval
  (define point<%> (interface () get-x get-y))
  (define colored-point<%> (interface (point<%>) color))

  (interface-extension? colored-point<%> point<%>)
  (interface-extension? point<%> colored-point<%>)
  (interface-extension? (interface () get-x get-y get-z) point<%>)
]}


@defproc[(method-in-interface? [sym symbol?] [intf interface?]) boolean?]{

Returns @racket[#t] if @racket[intf] (or any of its ancestor
interfaces) includes a member with the name @racket[sym], @racket[#f]
otherwise.

@examples[#:eval class-eval
  (define i<%> (interface () get-x get-y))
  (method-in-interface? 'get-x i<%>)
  (method-in-interface? 'get-z i<%>)
]}


@defproc[(interface->method-names [intf interface?]) (listof symbol?)]{

Returns a list of symbols for the method names in @racket[intf],
including methods inherited from superinterfaces, but not including
methods whose names are local (i.e., declared with
@racket[define-local-member-name]).

@examples[#:eval class-eval
  (define i<%> (interface () get-x get-y))
  (interface->method-names i<%>)
]}


@defproc[(object-method-arity-includes? [object object?] [sym symbol?] [cnt exact-nonnegative-integer?])
         boolean?]{

Returns @racket[#t] if @racket[object] has a method named @racket[sym]
that accepts @racket[cnt] arguments, @racket[#f] otherwise.

@examples[#:eval class-eval
(define c%
  (class object%
    (super-new)
    (define/public (m x [y 0])
      (+ x y))))

(object-method-arity-includes? (new c%) 'm 1)
(object-method-arity-includes? (new c%) 'm 2)
(object-method-arity-includes? (new c%) 'm 3)
(object-method-arity-includes? (new c%) 'n 1)
]}


@defproc[(field-names [object object?]) (listof symbol?)]{

Returns a list of all of the names of the fields bound in
@racket[object], including fields inherited from superinterfaces, but
not including fields whose names are local (i.e., declared with
@racket[define-local-member-name]).

@examples[#:eval class-eval
  (field-names (new object%))
  (field-names (new (class object% (super-new) (field [x 0] [y 0]))))
]}


@defproc[(object-info [object object?]) (values (or/c class? #f) boolean?)]{

Returns two values, analogous to the return
values of @racket[struct-info]:
@itemize[

  @item{@racket[_class]: a class or @racket[#f]; the result is
  @racket[#f] if the current inspector does not control any class for
  which the @racket[object] is an instance.}

  @item{@racket[_skipped?]: @racket[#f] if the first result corresponds
  to the most specific class of @racket[object], @racket[#t]
  otherwise.}

]}


@defproc[(class-info [class class?])
         (values symbol?
                 exact-nonnegative-integer?
                 (listof symbol?)
                 (any/c exact-nonnegative-integer? . -> . any/c)
                 (any/c exact-nonnegative-integer? any/c . -> . any/c)
                 (or/c class? #f)
                 boolean?)]{

Returns seven values, analogous to the return
values of @racket[struct-type-info]:

@itemize[

  @item{@racket[_name]: the class's name as a symbol;}

  @item{@racket[_field-cnt]: the number of fields (public and private)
   defined by the class;}

  @item{@racket[_field-name-list]: a list of symbols corresponding to the
  class's public fields; this list can be larger than @racket[_field-cnt]
  because it includes inherited fields;}

  @item{@racket[_field-accessor]: an accessor procedure for obtaining
  field values in instances of the class; the accessor takes an
  instance and a field index between @racket[0] (inclusive)
  and @racket[_field-cnt] (exclusive);}

  @item{@racket[_field-mutator]: a mutator procedure for modifying
  field values in instances of the class; the mutator takes an
  instance, a field index between @racket[0] (inclusive)
  and @racket[_field-cnt] (exclusive), and a new field value;}

  @item{@racket[_super-class]: a class for the most specific ancestor of
   the given class that is controlled by the current inspector,
   or @racket[#f] if no ancestor is controlled by the current
   inspector;}

  @item{@racket[_skipped?]: @racket[#f] if the sixth result is the most
   specific ancestor class, @racket[#t] otherwise.}

]}

@defstruct[(exn:fail:object exn:fail) ()]{

Raised for @racket[class]-related failures, such as attempting to call
a method that is not supplied by an object.

}

@defproc[(class-seal [class class?]
                     [key symbol?]
                     [unsealed-inits (listof symbol?)]
                     [unsealed-fields (listof symbol?)]
                     [unsealed-methods (listof symbol?)]
                     [inst-proc (-> class? any)]
                     [member-proc (-> class? (listof symbol?) any)])
         class?]{

Adds a seal to a given class keyed with the symbol @racket[key]. The
given @racket[unsealed-inits], @racket[unsealed-fields], and
@racket[unsealed-methods] list corresponding class members that are
unaffected by sealing.

When a class has any seals, the @racket[inst-proc] procedure is called
on instantiation (normally, this is used to raise an error on
instantiation) and the @racket[member-proc] function is called
(again, this is normally used to raise an error) when a subclass
attempts to add class members that are not listed in the unsealed lists.

The @racket[inst-proc] is called with the class value on which an
instantiation was attempted. The @racket[member-proc] is called with
the class value and the list of initialization argument, field, or
method names.
}

@defproc[(class-unseal [class class?]
                       [key symbol?]
                       [wrong-key-proc (-> class? any)])
         class?]{

Removes a seal on a class that has been previously sealed with the
@racket[class-seal] function and the given @racket[key].

If the unseal removed all of the seals in the class, the class
value can be instantiated or subclassed freely. If the given
class value does not contain or any seals or does not contain
any seals with the given key, the @racket[wrong-key-proc] function
is called with the class value.
}

@; ----------------------------------------------------------------------

@include-section["surrogate.scrbl"]

@close-eval[class-eval]
