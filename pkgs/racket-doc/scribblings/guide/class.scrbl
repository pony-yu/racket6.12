#lang scribble/doc
@(require scribble/manual scribble/eval racket/class "guide-utils.rkt"
          (for-label racket/class racket/trait racket/contract))

@(define class-eval
   (let ([e (make-base-eval)])
     (e '(require racket/class))
     e))

@; FIXME: at some point, discuss classes vs. units vs. modules


@;{@title[#:tag "classes"]{Classes and Objects}}
@title[#:tag "classes"]{类和对象}

@margin-note{
 @;{This chapter is based on a paper @cite["Flatt06"].}
   本章基于一篇论文@cite["Flatt06"]。
}

@;{A @racket[class] expression denotes a first-class value,
just like a @racket[lambda] expression:}
一个@racket[类（class）]表达式表示一类值，就像一个@racket[lambda]表达式一样：

@specform[(class superclass-expr decl-or-expr ...)]

@;{The @racket[_superclass-expr] determines the superclass for the new
class. Each @racket[_decl-or-expr] is either a declaration related to
methods, fields, and initialization arguments, or it is an expression
that is evaluated each time that the class is instantiated. In other
words, instead of a method-like constructor, a class has
initialization expressions interleaved with field and method
declarations.}
@racket[_superclass-expr]确定为新类的基类。每个@racket[_decl-or-expr]既是一个声明，关系到对方法、字段和初始化参数，也是一个表达式，每次求值就实例化类。换句话说，与方法之类的构造器不同，类具有与字段和方法声明交错的初始化表达式。

@;{By convention, class names end with @racketidfont{%}. The built-in root class is
@racket[object%]. The following expression creates a class with
public methods @racket[get-size], @racket[grow], and @racket[eat]:}
按照惯例，类名以@racketidfont{%}结束。内置根类是@racket[object%]。下面的表达式用公共方法@racket[get-size]、@racket[grow]和@racket[eat]创建一个类：

@racketblock[
(class object%
  (init size)                (code:comment #,(t @;{"initialization argument"}"初始化参数"))

  (define current-size size) (code:comment #,(t @;{"field"}"字段"))

  (super-new)                (code:comment #,(t @;{"superclass initialization"}"基类初始化"))

  (define/public (get-size)
    current-size)

  (define/public (grow amt)
    (set! current-size (+ amt current-size)))

  (define/public (eat other-fish)
    (grow (send other-fish get-size))))
]

@(interaction-eval
#:eval class-eval
(define fish%
  (class object%
    (init size)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size))))))

@;{The @racket[size] initialization argument must be supplied via a named
 argument when instantiating the class through the @racket[new] form:}
当通过@racket[new]表实例化类时，@racket[size]的初始化参数必须通过一个命名参数提供：

@racketblock[
(new (class object% (init size) ....) [size 10])
]

@;{Of course, we can also name the class and its instance:}
当然，我们还可以命名类及其实例：

@racketblock[
(define fish% (class object% (init size) ....))
(define charlie (new fish% [size 10]))
]

@(interaction-eval
#:eval class-eval
(define charlie (new fish% [size 10])))

@;{In the definition of @racket[fish%], @racket[current-size] is a
private field that starts out with the value of the @racket[size]
initialization argument. Initialization arguments like @racket[size]
are available only during class instantiation, so they cannot be
referenced directly from a method. The @racket[current-size] field, in
contrast, is available to methods.}
在@racket[fish%]的定义中，@racket[current-size]是一个以@racket[size]值初始化参数开头的私有字段。像@racket[size]这样的初始化参数只有在类实例化时才可用，因此不能直接从方法引用它们。与此相反，@racket[current-size]字段可用于方法。

@;{The @racket[(super-new)] expression in @racket[fish%] invokes the
initialization of the superclass. In this case, the superclass is
@racket[object%], which takes no initialization arguments and performs
no work; @racket[super-new] must be used, anyway, because a class must
always invoke its superclass's initialization.}
在@racket[class]中的@racket[(super-new)]表达式调用基类的初始化。在这种情况下，基类是@racket[object%]，它没有带初始化参数也没有执行任何工作；必须使用@racket[super-new]，因为一个类总必须总是调用其基类的初始化。

@;{Initialization arguments, field declarations, and expressions such as
@racket[(super-new)] can appear in any order within a @racket[class],
and they can be interleaved with method declarations. The relative
order of expressions in the class determines the order of evaluation
during instantiation. For example, if a field's initial value requires
calling a method that works only after superclass initialization, then
the field declaration must be placed after the @racket[super-new]
call. Ordering field and initialization declarations in this way helps
avoid imperative assignment. The relative order of method declarations
makes no difference for evaluation, because methods are fully defined
before a class is instantiated.}
初始化参数、字段声明和表达式如@racket[(super-new)]可以以@racket[类（class）]中的任何顺序出现，并且它们可以与方法声明交织在一起。类中表达式的相对顺序决定了实例化过程中的求值顺序。例如，如果一个字段的初始值需要调用一个方法，它只有在基类初始化后才能工作，然后字段声明必须放在@racket[super-new]调用后。以这种方式排序字段和初始化声明有助于规避不可避免的求值。方法声明的相对顺序对求值没有影响，因为方法在类实例化之前被完全定义。

@;{@section[#:tag "methods"]{Methods}}
@section[#:tag "methods"]{方法}

@;{Each of the three @racket[define/public] declarations in
@racket[fish%] introduces a new method. The declaration uses the same
syntax as a Racket function, but a method is not accessible as an
independent function.  A call to the @racket[grow] method of a
@racket[fish%] object requires the @racket[send] form:}
@racket[fish%]中的三个@racket[define/public]声明都引入了一种新方法。声明使用与Racket函数相同的语法，但方法不能作为独立函数访问。调用@racket[fish%]对象的@racket[grow]方法需要@racket[send]表：

@interaction[
#:eval class-eval
(send charlie grow 6)
(send charlie get-size)
]

@;{Within @racket[fish%], self methods can be called like functions,
because the method names are in scope.  For example, the @racket[eat]
method within @racket[fish%] directly invokes the @racket[grow]
method.  Within a class, attempting to use a method name in any way
other than a method call results in a syntax error.}
在@racket[fish%]中，自方法可以被像函数那样调用，因为方法名在作用域中。例如，@racket[fish%]中的@racket[eat]方法直接调用@racket[grow]方法。在类中，试图以除方法调用以外的任何方式使用方法名会导致语法错误。

@;{In some cases, a class must call methods that are supplied by the superclass
but not overridden. In that case, the class can use @racket[send]
with @racket[this] to access the method:}
在某些情况下，一个类必须调用由基类提供但不能被重写的方法。在这种情况下，类可以使用带@racket[this]的@racket[send]来访问该方法：

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)
                         (send this eat fish2))))
]

@;{Alternately, the class can declare the existence of a method using @racket[inherit],
which brings the method name into scope for a direct call:}
另外，类可以声明一个方法使用@racket[inherit]（继承）的存在，该方法将方法名引入到直接调用的作用域中：

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

@;{With the @racket[inherit] declaration, if @racket[fish%] had not
provided an @racket[eat] method, an error would be signaled in the
evaluation of the @racket[class] form for @racket[hungry-fish%]. In
contrast, with @racket[(send this ....)], an error would not be
signaled until the @racket[eat-more] method is called and the
@racket[send] form is evaluated. For this reason, @racket[inherit] is
preferred.}
在@racket[inherit]声明中，如果@racket[fish%]没有提供一个@racket[eat]方法，那么在对 @racket[hungry-fish%]类表的求值中会出现一个错误。与此相反，用@racket[(send this ....)]，直到@racket[eat-more]方法被调和@racket[send]表被求值前不会发出错误信号。因此，@racket[inherit]是首选。

@;{Another drawback of @racket[send] is that it is less efficient than
@racket[inherit]. Invocation of a method via @racket[send] involves
finding a method in the target object's class at run time, making
@racket[send] comparable to an interface-based method call in Java. In
contrast, @racket[inherit]-based method invocations use an offset
within the class's method table that is computed when the class is
created.}
@racket[send]的另一个缺点是它比@racket[inherit]效率低。一个方法的请求通过@racket[send]调用寻找在运行时在目标对象的类的方法，使@racket[send]类似于java方法调用接口。相反，基于@racket[inherit]的方法调用使用一个类的方法表中的偏移量，它在类创建时计算。

@;{To achieve performance similar to @racket[inherit]-based method calls when
invoking a method from outside the method's class, the programmer must use the
@racket[generic] form, which produces a class- and method-specific
@defterm{generic method} to be invoked with @racket[send-generic]:}
为了在从方法类之外调用方法时实现与继承方法调用类似的性能，程序员必须使用@racket[generic]（泛型）表，它生成一个特定类和特定方法的@defterm{generic方法}，用@racket[send-generic]调用：

@def+int[
#:eval class-eval
(define get-fish-size (generic fish% get-size))
(send-generic charlie get-fish-size)
(send-generic (new hungry-fish% [size 32]) get-fish-size)
(send-generic (new object%) get-fish-size)
]

@;{Roughly speaking, the form translates the class and the external
method name to a location in the class's method table. As illustrated
by the last example, sending through a generic method checks that its
argument is an instance of the generic's class.}
粗略地说，表单将类和外部方法名转换为类方法表中的位置。如上一个例子所示，通过泛型方法发送检查它的参数是泛型类的一个实例。

@;{Whether a method is called directly within a @racket[class],
through a generic method,
or through @racket[send], method overriding works in the usual way:}
是否在@racket[class]内直接调用方法，通过泛型方法，或通过@racket[send]，方法以通常的方式重写工程：

@defs+int[
#:eval class-eval
[
(define picky-fish% (class fish% (super-new)
                      (define/override (grow amt)
                        ;; Doesn't eat all of its food
                        (super grow (* 3/4 amt)))))
(define daisy (new picky-fish% [size 20]))
]
(send daisy eat charlie)
(send daisy get-size)
]

@;{The @racket[grow] method in @racket[picky-fish%] is declared with
@racket[define/override] instead of @racket[define/public], because
@racket[grow] is meant as an overriding declaration. If @racket[grow]
had been declared with @racket[define/public], an error would have
been signaled when evaluating the @racket[class] expression, because
@racket[fish%] already supplies @racket[grow].}
在@racket[picky-fish%]的@racket[grow]方法是用@racket[define/override]声明的，而不是 @racket[define/public]，因为@racket[grow]是作为一个重写的申明的意义。如果@racket[grow]已经用@racket[define/public]声明，那么在对类表达式求值时会发出一个错误，因为@racket[fish%]已经提供了@racket[grow]。

@;{Using @racket[define/override] also allows the invocation of the
overridden method via a @racket[super] call. For example, the
@racket[grow] implementation in @racket[picky-fish%] uses
@racket[super] to delegate to the superclass implementation.}
使用@racket[define/override]也允许通过@racket[super]调用调用重写的方法。例如，@racket[grow]在@racket[picky-fish%]实现使用@racket[super]代理给基类的实现。

@;=============================================================
@;{@section[#:tag "initargs"]{Initialization Arguments}}
@section[#:tag "initargs"]{初始化参数}

@;{Since @racket[picky-fish%] declares no initialization arguments, any
initialization values supplied in @racket[(new picky-fish% ....)]  are
propagated to the superclass initialization, i.e., to @racket[fish%].
A subclass can supply additional initialization arguments for its
superclass in a @racket[super-new] call, and such initialization
arguments take precedence over arguments supplied to @racket[new]. For
example, the following @racket[size-10-fish%] class always generates
fish of size 10:}
因为@racket[picky-fish%]申明没有任何初始化参数，任何初始化值在@racket[(new picky-fish% ....)]里提供都被传递给基类的初始化，即传递给@racket[fish%]。子类可以在@racket[super-new]调用其基类时提供额外的初始化参数，这样的初始化参数会优先于参数提供给@racket[new]。例如，下面的@racket[size-10-fish%]类总是产生大小为10的鱼：

@def+int[
#:eval class-eval
(define size-10-fish% (class fish% (super-new [size 10])))
(send (new size-10-fish%) get-size)
]

@;{In the case of @racket[size-10-fish%], supplying a @racket[size]
initialization argument with @racket[new] would result in an
initialization error; because the @racket[size] in @racket[super-new]
takes precedence, a @racket[size] supplied to @racket[new] would have
no target declaration.}
就@racket[size-10-fish%]来说，用@racket[new]提供一个@racket[size]初始化参数会导致初始化错误；因为在@racket[super-new]里的@racket[size]优先，@racket[size]提供给@racket[new]没有目标申明。

@;{An initialization argument is optional if the @racket[class] form
declares a default value. For example, the following @racket[default-10-fish%]
class accepts a @racket[size] initialization argument, but its value defaults to
10 if no value is supplied on instantiation:}
如果@racket[class]表声明一个默认值，则初始化参数是可选的。例如，下面的@racket[default-10-fish%]类接受一个@racket[size]的初始化参数，但如果在实例里没有提供值那它的默认值是10：

@def+int[
#:eval class-eval
(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))
(new default-10-fish%)
(new default-10-fish% [size 20])
]

@;{In this example, the @racket[super-new] call propagates its own
@racket[size] value as the @racket[size] initialization argument to
the superclass.}
在这个例子中，@racket[super-new]调用传递它自己的@racket[size]值作为@racket[size]初始化初始化参数传递给基类。

@;{@section[#:tag "intnames"]{Internal and External Names}}
@section[#:tag "intnames"]{内部和外部名称}

@;{The two uses of @racket[size] in @racket[default-10-fish%] expose the
double life of class-member identifiers. When @racket[size] is the
first identifier of a bracketed pair in @racket[new] or
@racket[super-new], @racket[size] is an @defterm{external name} that
is symbolically matched to an initialization argument in a class. When
@racket[size] appears as an expression within
@racket[default-10-fish%], @racket[size] is an @defterm{internal name}
that is lexically scoped. Similarly, a call to an inherited
@racket[eat] method uses @racket[eat] as an internal name, whereas a
@racket[send] of @racket[eat] uses @racket[eat] as an external name.}
在@racket[default-10-fish%]中@racket[size]的两个使用揭示了类成员标识符的双重身份。当@racket[size]是@racket[new]或@racket[super-new]中的一个括号对的第一标识符，@racket[size]是一个@defterm{外部名称（external name）}，象征性地匹配到类中的初始化参数。当@racket[size]作为一个表达式出现在@racket[default-10-fish%]中，@racket[size]是一个@defterm{内部名称（internal name）}，它是词法作用域。类似地，对继承的@racket[eat]方法的调用使用@racket[eat]作为内部名称，而一个@racket[eat]的@racket[send]的使用作为一个外部名称。

@;{The full syntax of the @racket[class] form allows a programmer to
specify distinct internal and external names for a class member. Since
internal names are local, they can be renamed to avoid shadowing or
conflicts. Such renaming is not frequently necessary, but workarounds
in the absence of renaming can be especially cumbersome.}
@racket[class]表的完整语法允许程序员为类成员指定不同的内部和外部名称。由于内部名称是本地的，因此可以重命名它们，以避免覆盖或冲突。这样的改名不总是必要的，但重命名缺乏的解决方法可以是特别繁琐。

@;{@section{Interfaces}}
@section[#:tag "Interfaces"]{接口}

@;{Interfaces are useful for checking that an object or a class
implements a set of methods with a particular (implied) behavior.
This use of interfaces is helpful even without a static type system
(which is the main reason that Java has interfaces).}
接口对于检查一个对象或一个类实现一组具有特定（隐含）行为的方法非常有用。接口的这种使用有帮助的，即使没有静态类型系统（那是java有接口的主要原因）。

@;{An interface in Racket is created using the @racket[interface]
form, which merely declares the method names required to implement the
interface. An interface can extend other interfaces, which means that
implementations of the interface automatically implement the extended
interfaces.}
Racket中的接口通过使用@racket[interface]表创建，它只声明需要去实现的接口的方法名称。接口可以扩展其它接口，这意味着接口的实现会自动实现扩展接口。

@specform[(interface (superinterface-expr ...) id ...)]

@;{To declare that a class implements an interface, the
@racket[class*] form must be used instead of @racket[class]:}
为了声明一个实现一个接口的类，必须使用@racket[class*]表代替@racket[class]：

@specform[(class* superclass-expr (interface-expr ...) decl-or-expr ...)]

@;{For example, instead of forcing all fish classes to be derived from
@racket[fish%], we can define @racket[fish-interface] and change the
@racket[fish%] class to declare that it implements
@racket[fish-interface]:}
例如，我们不必强制所有的@racket[fish%]类都是源自于@racket[fish%]，我们可以定义@racket[fish-interface]并改变@racket[fish%]类来声明它实现了@racket[fish-interface]：

@racketblock[
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface) ....))
]

@;{If the definition of @racket[fish%] does not include
@racket[get-size], @racket[grow], and @racket[eat] methods, then an
error is signaled in the evaluation of the @racket[class*] form,
because implementing the @racket[fish-interface] interface requires
those methods.}
如果@racket[fish%]的定义不包括@racket[get-size]、@racket[grow]和@racket[eat]方法，那么在@racket[class*]表求值时会出现错误，因为实现@racket[fish-interface]接口需要这些方法。

@;{The @racket[is-a?] predicate accepts an object as its first argument
and either a class or interface as its second argument. When given a
class, @racket[is-a?] checks whether the object is an instance of that
class or a derived class.  When given an interface, @racket[is-a?]
checks whether the object's class implements the interface. In
addition, the @racket[implementation?]  predicate checks whether a
given class implements a given interface.}
@racket[is-a?]判断接受一个对象作为它的第一个参数，同时类或接口作为它的第二个参数。当给了一个类，无论对象是该类的实例或者派生类的实例，@racket[is-a?]都执行检查。当给一个接口，无论对象的类是否实现接口，@racket[is-a?]都执行检查。另外，@racket[implementation?]判断检查给定类是否实现给定接口。

@;{@section[#:tag "inner"]{Final, Augment, and Inner}}
@section[#:tag "inner"]{Final、Augment和Inner}

@;{As in Java, a method in a @racket[class] form can be specified as
@defterm{final}, which means that a subclass cannot override the
method.  A final method is declared using @racket[public-final] or
@racket[override-final], depending on whether the declaration is for a
new method or an overriding implementation.}
在java中，一个@racket[class]表的方法可以被指定为@defterm{最终的（final）}，这意味着一个子类不能重写方法。一个最终方法是使用@racket[public-final]或@racket[override-final]申明，取决于声明是为一个新方法还是一个重写实现。

@;{Between the extremes of allowing arbitrary overriding and disallowing
overriding entirely, the class system also supports Beta-style
@defterm{augmentable} methods @cite["Goldberg04"]. A method
declared with @racket[pubment] is like @racket[public], but the method
cannot be overridden in subclasses; it can be augmented only. A
@racket[pubment] method must explicitly invoke an augmentation (if any)
using @racket[inner]; a subclass augments the method using
@racket[augment], instead of @racket[override].}
在允许与不允许任意完全重写的两个极端之间，类系统还支持Beta类型的@defterm{可扩展（augmentable）}方法。一个带@racket[pubment]声明的方法类似于@racket[public]，但方法不能在子类中重写；它仅仅是可扩充。一个@racket[pubment]方法必须显式地使用@racket[inner]调用一个扩展（如果有）；一个子类使用@racket[pubment]扩展方法，而不是使用@racket[override]。

@;{In general, a method can switch between augment and override modes in
a class derivation. The @racket[augride] method specification
indicates an augmentation to a method where the augmentation is itself
overrideable in subclasses (though the superclass's implementation
cannot be overridden). Similarly, @racket[overment] overrides a method
and makes the overriding implementation augmentable.}
一般来说，一个方法可以在类派生的扩展模式和重写模式之间进行切换。@racket[augride]方法详述表明了一个扩展，这里这个扩展本身在子类中是可重写的的方法（虽然这个基类的实现不能重写）。同样，@racket[overment]重写一个方法并使得重写的实现变得可扩展。

@;{@section[#:tag "extnames"]{Controlling the Scope of External Names}}
@section[#:tag "extnames"]{控制外部名称的范围}

@margin-note{
  @;{Java's access modifiers (like @index["protected method"]{@tt{protected}})
  play a role similar to @racket[define-member-name], but
  unlike in Java, Racket's mechanism for controlling access
  is based on lexical scope, not the inheritance hierarchy.}
    java的访问修饰符（如@index["protected method"]{@tt{受保护的（protected）}}）扮演的一个角色类似于@racket[define-member-name]，但不像java，访问控制Racket的机制是基于词法范围，不能继承层次结构。
}

@;{As noted in @secref["intnames"], class members have both
internal and external names. A member definition binds an internal
name locally, and this binding can be locally renamed.  External
names, in contrast, have global scope by default, and a member
definition does not bind an external name. Instead, a member
definition refers to an existing binding for an external name, where
the member name is bound to a @defterm{member key}; a class ultimately
maps member keys to methods, fields, and initialization arguments.}
正如@secref["intnames"]所指出的，类成员既有内部名称，也有外部名称。成员定义在本地绑定内部名称，此绑定可以在本地重命名。与此相反，外部名称默认情况下具有全局范围，成员定义不绑定外部名称。相反，成员定义指的是外部名称的现有绑定，其中成员名绑定到@defterm{成员键（member key）}；一个类最终将成员键映射到方法、字段和初始化参数。

@;{Recall the @racket[hungry-fish%] @racket[class] expression:}
回头看@racket[hungry-fish%]类（@racket[class]）表达式：

@racketblock[
(define hungry-fish% (class fish% ....
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

@;{During its evaluation, the @racket[hungry-fish%] and @racket[fish%]
classes refer to the same global binding of @racket[eat].  At run
time, calls to @racket[eat] in @racket[hungry-fish%] are matched with
the @racket[eat] method in @racket[fish%] through the shared method
key that is bound to @racket[eat].}
在求值过程中@racket[hungry-fish%]类和@racket[fish%]类指相同的@racket[eat]的全局绑定。在运行时，在@racket[hungry-fish%]中调用@racket[eat]是通过共享绑定到@racket[eat]的方法键和@racket[fish%]中的@racket[eat]方法相匹配。

@;{The default binding for an external name is global, but a
programmer can introduce an external-name binding with the
@racket[define-member-name] form.}
对外部名称的默认绑定是全局的，但程序员可以用@racket[define-member-name]表引入外部名称绑定。

@specform[(define-member-name id member-key-expr)]

@;{In particular, by using @racket[(generate-member-key)] as the
@racket[member-key-expr], an external name can be localized for a
particular scope, because the generated member key is inaccessible
outside the scope. In other words, @racket[define-member-name] gives
an external name a kind of package-private scope, but generalized from
packages to arbitrary binding scopes in Racket.}
特别是，通过使用@racket[(generate-member-key)]作为@racket[member-key-expr]，外部名称可以为一个特定的范围局部化，因为生成的成员键范围之外的访问。换句话说，@racket[define-member-name]给外部名称一种私有包范围，但从包中概括为Racket中的任意绑定范围。

@;{For example, the following @racket[fish%] and @racket[pond%] classes cooperate
via a @racket[get-depth] method that is only accessible to the
cooperating classes:}
例如，下面的@racket[fish%]类和@racket[pond%]类通过一个@racket[get-depth]方法配合，只有这个配合类可以访问：

@racketblock[
(define-values (fish% pond%) (code:comment #,(t @;{"two mutually recursive classes"}"两个相互递归类"))
  (let () ; create a local definition scope
    (define-member-name get-depth (generate-member-key))
    (define fish%
      (class ....
        (define my-depth ....)
	(define my-pond ....)
	(define/public (dive amt)
        (set! my-depth
              (min (+ my-depth amt)
                   (send my-pond get-depth))))))
    (define pond%
      (class ....
        (define current-depth ....)
        (define/public (get-depth) current-depth)))
    (values fish% pond%)))
]

@;{External names are in a namespace that separates them from other Racket
names. This separate namespace is implicitly used for the method name in
@racket[send], for initialization-argument names in @racket[new], or for
the external name in a member definition.  The special form
@racket[member-name-key] provides access to the binding of an external name
in an arbitrary expression position: @racket[(member-name-key id)]
produces the member-key binding of @racket[id] in the current scope.}
外部名称在名称空间中，将它们与其它Racket名称分隔开。这个单独的命名空间被隐式地用于@racket[send]中的方法名、在@racket[new]中的初始化参数名称，或成员定义中的外部名称。特殊表 @racket[member-name-key]提供对任意表达式位置外部名称的绑定的访问：@racket[(member-name-key id)]在当前范围内生成@racket[id]的成员键绑定。

@;{A member-key value is primarily used with a
@racket[define-member-name] form. Normally, then,
@racket[(member-name-key id)] captures the method key of @racket[id]
so that it can be communicated to a use of @racket[define-member-name]
in a different scope. This capability turns out to be useful for
generalizing mixins, as discussed next.}
成员键值主要用于@racket[define-member-name]表。通常，@racket[(member-name-key id)]捕获@racket[id]的方法键，以便它可以在不同的范围内传递到@racket[define-member-name]的使用。这种能力证明推广混合是有用的，作为接下来的讨论。

@; ----------------------------------------------------------------------

@;{@section{Mixins}}
@section[#:tag "Mixins"]{混合}

@;{Since @racket[class] is an expression form instead of a top-level
declaration as in Smalltalk and Java, a @racket[class] form can be
nested inside any lexical scope, including @racket[lambda]. The result
is a @deftech{mixin}, i.e., a class extension that is parameterized
with respect to its superclass.}
因为@racket[class]（类）是一种表达表，而不是如同在Smalltalk和java里的一个顶级的声明，一个@racket[class]表可以嵌套在任何词法范围内，包括lambda（λ）。其结果是一个@deftech{混合（mixin）}，即，一个类的扩展，是相对于它的基类的参数化。

@;{For example, we can parameterize the @racket[picky-fish%] class over
its superclass to define @racket[picky-mixin]:}
例如，我们可以参数化@racket[picky-fish%]类来覆盖它的基类从而定义@racket[picky-mixin]：

@racketblock[
(define (picky-mixin %)
  (class % (super-new)
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define picky-fish% (picky-mixin fish%))
]

@;{Many small differences between Smalltalk-style classes and Racket
classes contribute to the effective use of mixins. In particular, the
use of @racket[define/override] makes explicit that
@racket[picky-mixin] expects a class with a @racket[grow] method. If
@racket[picky-mixin] is applied to a class without a @racket[grow]
method, an error is signaled as soon as @racket[picky-mixin] is
applied.}
Smalltalk风格类和Racket类之间的许多小的差异有助于混合的有效利用。特别是，@racket[define/override]的使用使得@racket[picky-mixin]期望一个类带有一个@racket[grow]方法更明确。如果@racket[picky-mixin]应用于一个没有@racket[grow]方法的类，一旦应用@racket[picky-mixin]则会发出一个错误的信息。

@;{Similarly, a use of @racket[inherit] enforces a ``method existence''
requirement when the mixin is applied:}
同样，当应用混合时使用@racket[inherit]（继承）执行“方法存在（method existence）”的要求：

@racketblock[
(define (hungry-mixin %)
  (class % (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2) 
      (eat fish1) 
      (eat fish2))))
]

@;{The advantage of mixins is that we can easily combine them to create
new classes whose implementation sharing does not fit into a
single-inheritance hierarchy---without the ambiguities associated with
multiple inheritance. Equipped with @racket[picky-mixin] and
@racket[hungry-mixin], creating a class for a hungry, yet picky fish
is straightforward:}
mixin的优势是，我们可以很容易地将它们结合起来以创建新的类，其共享的实现不适合一个继承层次——没有多继承相关的歧义。配备@racket[picky-mixin]和@racket[hungry-mixin]，为“hungry”创造了一个类，但“picky fish”是直截了当的：

@racketblock[
(define picky-hungry-fish% 
  (hungry-mixin (picky-mixin fish%)))
]

@;{The use of keyword initialization arguments is critical for the easy
use of mixins. For example, @racket[picky-mixin] and
@racket[hungry-mixin] can augment any class with suitable @racket[eat]
and @racket[grow] methods, because they do not specify initialization
arguments and add none in their @racket[super-new] expressions:}
关键词初始化参数的使用是混合的易于使用的重点。例如，@racket[picky-mixin]和@racket[hungry-mixin]可以通过合适的@racket[eat]方法和@racket[grow]方法增加任何类，因为它们在它们的@racket[super-new]表达式里没有指定初始化参数也没有添加东西：

@racketblock[
(define person% 
  (class object%
    (init name age)
    ....
    (define/public (eat food) ....)
    (define/public (grow amt) ....)))
(define child% (hungry-mixin (picky-mixin person%)))
(define oliver (new child% [name "Oliver"] [age 6]))
]

@;{Finally, the use of external names for class members (instead of
lexically scoped identifiers) makes mixin use convenient. Applying
@racket[picky-mixin] to @racket[person%] works because the names
@racket[eat] and @racket[grow] match, without any a priori declaration
that @racket[eat] and @racket[grow] should be the same method in
@racket[fish%] and @racket[person%]. This feature is a potential
drawback when member names collide accidentally; some accidental
collisions can be corrected by limiting the scope external names, as
discussed in @secref["extnames"].}
最后，对类成员的外部名称的使用（而不是词法作用域标识符）使得混合使用很方便。添加@racket[picky-mixin]到@racket[person%]运行，因为这个名字@racket[eat]和@racket[grow]匹配，在@racket[fish%]和@racket[person%]里没有任何@racket[eat]和@racket[grow]的优先申明可以是同样的方法。当成员名称意外碰撞后，此特性是一个潜在的缺陷；一些意外冲突可以通过限制外部名称作用域来纠正，就像在《@secref["extnames"]（Controlling the Scope of External Names）》所讨论的那样。

@;{@subsection{Mixins and Interfaces}}
@subsection[#:tag "Mixins-and-Interfaces"]{混合和接口}

@;{Using @racket[implementation?], @racket[picky-mixin] could require
that its base class implements @racket[grower-interface], which could
be implemented by both @racket[fish%] and @racket[person%]:}
使用@racket[implementation?]，@racket[picky-mixin]可以要求其基类实现@racket[grower-interface]，这可以是由@racket[fish%]和@racket[person%]实现：

@racketblock[
(define grower-interface (interface () grow))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class % ....))
]

@;{Another use of interfaces with a mixin is to tag classes generated by
the mixin, so that instances of the mixin can be recognized. In other
words, @racket[is-a?] cannot work on a mixin represented as a
function, but it can recognize an interface (somewhat like a
@defterm{specialization interface}) that is consistently implemented
by the mixin.  For example, classes generated by @racket[picky-mixin]
could be tagged with @racket[picky-interface], enabling the
@racket[is-picky?] predicate:}
另一个使用带混合的接口是标记类通过混合产生，因此，混合实例可以被识别。换句话说，@racket[is-a?]不能在一个混合上体现为一个函数运行，但它可以识别为一个接口（有点像一个@defterm{特定的接口}），它总是被混合所实现。例如，通过@racket[picky-mixin]生成的类可以被@racket[picky-interface]所标记，使是@racket[is-picky?]去判定:

@racketblock[
(define picky-interface (interface ()))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class* % (picky-interface) ....))
(define (is-picky? o)
  (is-a? o picky-interface))
]

@;{@subsection{The @racket[mixin] Form}}
@subsection[#:tag "The-mixin-Form"]{@racket[mixin]表}

@;{To codify the @racket[lambda]-plus-@racket[class] pattern for
implementing mixins, including the use of interfaces for the domain
and range of the mixin, the class system provides a @racket[mixin]
macro:}
为执行混合而编纂@racket[lambda]加@racket[class]模式，包括对混合的定义域和值域接口的使用，类系统提供了一个@racket[mixin]宏：

@specform[
(mixin (interface-expr ...) (interface-expr ...)
  decl-or-expr ...)
]

@;{The first set of @racket[interface-expr]s determines the domain of the
mixin, and the second set determines the range. That is, the expansion
is a function that tests whether a given base class implements the
first sequence of @racket[interface-expr]s and produces a class that
implements the second sequence of @racket[interface-expr]s. Other
requirements, such as the presence of @racket[inherit]ed methods in
the superclass, are then checked for the @racket[class] expansion of
the @racket[mixin] form.  For example:}
@racket[interface-expr]的第一个集合确定混合的定义域，第二个集合确定值域。就是说，扩张是一个函数，它测试是否一个给定的基类实现@racket[interface-expr]的第一个序列，并产生一个类实现@racket[interface-expr]的第二个序列。其它要求，如在基类的继承方法的存在，然后检查@racket[mixin]表的@racket[class]扩展。例如:

@interaction[
#:eval class-eval

(define choosy-interface (interface () choose?))
(define hungry-interface (interface () eat))
(define choosy-eater-mixin
  (mixin (choosy-interface) (hungry-interface)
    (inherit choose?)
    (super-new)
    (define/public (eat x)
      (cond
        [(choose? x)
         (printf "chomp chomp chomp on ~a.\n" x)]
        [else
         (printf "I'm not crazy about ~a.\n" x)]))))

(define herring-lover% 
  (class* object% (choosy-interface)
    (super-new)
    (define/public (choose? x)
      (regexp-match #px"^herring" x))))

(define herring-eater% (choosy-eater-mixin herring-lover%))
(define eater (new herring-eater%))
(send eater eat "elderberry")
(send eater eat "herring")
(send eater eat "herring ice cream")
]

@;{Mixins not only override methods and introduce public methods, they
can also augment methods, introduce augment-only methods, add an
overrideable augmentation, and add an augmentable override --- all of
the things that a class can do (see @secref["inner"]).}
混合不仅覆盖方法，并引入公共方法，它们也可以扩展方法，引入扩展的方法，添加一个可重写的扩展，并添加一个可扩展的覆盖——所有这些事一个类都能完成（参见@secref["inner"]部分）。

@;{@subsection[#:tag "parammixins"]{Parameterized Mixins}}
@subsection[#:tag "parammixins"]{参数化的混合}

@;{As noted in @secref["extnames"], external names can be bound with
@racket[define-member-name]. This facility allows a mixin to be
generalized with respect to the methods that it defines and uses.  For
example, we can parameterize @racket[hungry-mixin] with respect to the
external member key for @racket[eat]:}
正如在@secref["extnames"]中指出的，外部名称可以用@racket[define-member-name]绑定。这个工具允许一个混合用定义或使用的方法概括。例如，我们可以通过对@racket[eat]的外部成员键的使用参数化@racket[hungry-mixin]：

@racketblock[
(define (make-hungry-mixin eat-method-key)
  (define-member-name eat eat-method-key)
  (mixin () () (super-new)
    (inherit eat)
    (define/public (eat-more x y) (eat x) (eat y))))
]

@;{To obtain a particular hungry-mixin, we must apply this function to a
member key that refers to a suitable
@racket[eat] method, which we can obtain using @racket[member-name-key]: }
获得一个特定的hungry-mixin，我们必须应用这个函数到一个成员键，它指向一个适当的@racket[eat]方法，我们可以获得 @racket[member-name-key]的使用：

@racketblock[
((make-hungry-mixin (member-name-key eat))
 (class object% .... (define/public (eat x) 'yum)))
]

@;{Above, we apply @racket[hungry-mixin] to an anonymous class that provides
@racket[eat], but we can also combine it with a class that provides 
@racket[chomp], instead:}
以上，我们应用@racket[hungry-mixin]给一个匿名类，它提供@racket[eat]，但我们也可以把它和一个提供@racket[chomp]的类组合，相反：

@racketblock[
((make-hungry-mixin (member-name-key chomp))
 (class object% .... (define/public (chomp x) 'yum)))
]

@; ----------------------------------------------------------------------

@;{@section{Traits}}
@section[#:tag "Traits"]{特征}

@;{A @defterm{trait} is similar to a mixin, in that it encapsulates a set
of methods to be added to a class. A trait is different from a mixin
in that its individual methods can be manipulated with trait operators
such as @racket[trait-sum] (merge the methods of two traits), @racket[trait-exclude]
(remove a method from a trait), and @racket[trait-alias] (add a copy of a
method with a new name; do not redirect any calls to the old name).}
一个@defterm{特征（trait）}类似于一个mixin，它封装了一组方法添加到一个类里。一个特征不同于一个mixin，它自己的方法是可以用特征运算符操控的，比如@racket[trait-sum]（合并这两个特征的方法）、@racket[trait-exclude]（从一个特征中移除方法）以及@racket[trait-alias]（添加一个带有新名字的方法的拷贝；它不重定向到对任何旧名字的调用）。

@;{The practical difference between mixins and traits is that two traits
can be combined, even if they include a common method and even if
neither method can sensibly override the other. In that case, the
programmer must explicitly resolve the collision, usually by aliasing
methods, excluding methods, and merging a new trait that uses the
aliases.}
混合和特征之间的实际差别是两个特征可以组合，即使它们包括了共有的方法，而且即使两者的方法都可以合理地覆盖其它方法。在这种情况下，程序员必须明确地解决冲突，通常通过混淆方法，排除方法，以及合并使用别名的新特性。

@;{Suppose our @racket[fish%] programmer wants to define two class
extensions, @racket[spots] and @racket[stripes], each of which
includes a @racket[get-color] method. The fish's spot color should not
override the stripe color nor vice versa; instead, a
@racket[spots+stripes-fish%] should combine the two colors, which is
not possible if @racket[spots] and @racket[stripes] are implemented as
plain mixins. If, however, @racket[spots] and @racket[stripes] are
implemented as traits, they can be combined. First, we alias
@racket[get-color] in each trait to a non-conflicting name. Second,
the @racket[get-color] methods are removed from both and the traits
with only aliases are merged. Finally, the new trait is used to create
a class that introduces its own @racket[get-color] method based on the
two aliases, producing the desired @racket[spots+stripes] extension.}
假设我们的@racket[fish%]程序员想要定义两个类扩展，@racket[spots]和@racket[stripes]，每个都包含@racket[get-color]方法。fish的spot不应该覆盖的stripe，反之亦然；相反，一个@racket[spots+stripes-fish%]应结合两种颜色，这是不可能的如果@racket[spots]和@racket[stripes]是普通混合实现。然而，如果spots和stripes作为特征来实现，它们可以组合在一起。首先，我们在每个特征中给@racket[get-color]起一个别名为一个不冲突的名称。第二，@racket[get-color]方法从两者中移除，只有别名的特征被合并。最后，新特征用于创建一个类，它基于这两个别名引入自己的@racket[get-color]方法，生成所需的@racket[spots+stripes]扩展。

@;{@subsection{Traits as Sets of Mixins}}
@subsection[#:tag "Traits-as-Sets-of-Mixins"]{特征作为混合集}

@;{One natural approach to implementing traits in Racket is as a set
of mixins, with one mixin per trait method.  For example, we might
attempt to define the spots and stripes traits as follows, using
association lists to represent sets:}
在Racket里实现特征的一个自然的方法是如同一组混合，每个特征方法带一个mixin。例如，我们可以尝试如下定义spots和stripes的特征，使用关联列表来表示集合：

@racketblock[
(define spots-trait
  (list (cons 'get-color 
               (lambda (%) (class % (super-new)
                             (define/public (get-color) 
                               'black))))))
(define stripes-trait
  (list (cons 'get-color 
              (lambda (%) (class % (super-new)
                            (define/public (get-color) 
                              'red))))))
]

@;{A set representation, such as the above, allows @racket[trait-sum] and
@racket[trait-exclude] as simple manipulations; unfortunately, it does
not support the @racket[trait-alias] operator. Although a mixin can be
duplicated in the association list, the mixin has a fixed method name,
e.g., @racket[get-color], and mixins do not support a method-rename
operation. To support @racket[trait-alias], we must parameterize the
mixins over the external method name in the same way that @racket[eat]
was parameterized in @secref["parammixins"].}
一个集合的表示，如上面所述，允许@racket[trait-sum]和@racket[trait-exclude]做为简单操作；不幸的是，它不支持@racket[trait-alias]运算符。虽然一个混合可以在关联表里复制，混合有一个固定的方法名称，例如，@racket[get-color]，而且混合不支持方法重命名操作。支持@racket[trait-alias]，我们必须在扩展方法名上参数化混合，同样地@racket[eat]在参数化混合（@secref["parammixins"]）中进行参数化。

@;{To support the @racket[trait-alias] operation, @racket[spots-trait]
should be represented as:}
为了支持@racket[trait-alias]操作，@racket[spots-trait]应表示为：

@racketblock[
(define spots-trait
  (list (cons (member-name-key get-color)
              (lambda (get-color-key %) 
                (define-member-name get-color get-color-key)
                (class % (super-new)
                  (define/public (get-color) 'black))))))
]

@;{When the @racket[get-color] method in @racket[spots-trait] is aliased
to @racket[get-trait-color] and the @racket[get-color] method is
removed, the resulting trait is the same as}
当@racket[spots-trait]中的@racket[get-color]方法是给@racket[get-trait-color]的别名并且@racket[get-color]方法被去除，由此产生的特性如下：

@racketblock[
(list (cons (member-name-key get-trait-color)
            (lambda (get-color-key %)
              (define-member-name get-color get-color-key)
              (class % (super-new)
                (define/public (get-color) 'black)))))
]

@;{To apply a trait @racket[_T] to a class @racket[_C] and obtain a derived
class, we use @racket[((trait->mixin _T) _C)]. The @racket[trait->mixin]
function supplies each mixin of @racket[_T] with the key for the mixin's
method and a partial extension of @racket[_C]:}
应用特征@racket[_T]到一个类@racket[_C]和获得一个派生类，我们用@racket[((trait->mixin _T) _C)]。@racket[trait->mixin]函数用给混合的方法和部分@racket[_C]扩展的键提供每个@racket[_T]的混合：


@racketblock[
(define ((trait->mixin T) C)
  (foldr (lambda (m %) ((cdr m) (car m) %)) C T))
]

@;{Thus, when the trait above is combined with other traits and then
applied to a class, the use of @racket[get-color] becomes a reference
to the external name @racket[get-trait-color].}
因此，当上述特性与其它特性结合，然后应用到类中时，@racket[get-color]的使用将成为外部名称@racket[get-trait-color]的引用。

@;{@subsection{Inherit and Super in Traits}}
@subsection[#:tag "Inherit-and-Super-in-Traits"]{特征的继承与基类}

@;{This first implementation of traits supports @racket[trait-alias], and it
 supports a trait method that calls itself, but it does not support
 trait methods that call each other. In particular, suppose that a spot-fish's
 market value depends on the color of its spots:}
特性的这个第一个实现支持@racket[trait-alias]，它支持一个调用自身的特性方法，但是它不支持调用彼此的特征方法。特别是，假设一个spot-fish的市场价值取决于它的斑点颜色：

@racketblock[
(define spots-trait
  (list (cons (member-name-key get-color) ....)
        (cons (member-name-key get-price)
              (lambda (get-price %) ....
                (class % ....
                  (define/public (get-price) 
                    .... (get-color) ....))))))
]

@;{In this case, the definition of @racket[spots-trait] fails, because
@racket[get-color] is not in scope for the @racket[get-price]
mixin. Indeed, depending on the order of mixin application when the
trait is applied to a class, the @racket[get-color] method may not be
available when @racket[get-price] mixin is applied to the class.
Therefore adding an @racket[(inherit get-color)] declaration to the
@racket[get-price] mixin does not solve the problem.}
在这种情况下，@racket[spots-trait]的定义失败，因为@racket[get-color]是不在@racket[get-price]混合范围之内。事实上，当特征应用于一个类时依赖于混合程序的顺序，当@racket[get-price]混合应用于类时@racket[get-color]方法可能不可获得。因此添加一个@racket[(inherit get-color)]申明给@racket[get-price]混合并不解决问题。

@;{One solution is to require the use of @racket[(send this get-color)] in
methods such as @racket[get-price]. This change works because
@racket[send] always delays the method lookup until the method call is
evaluated. The delayed lookup is more expensive than a direct call,
however. Worse, it also delays checking whether a @racket[get-color] method
even exists.}
一种解决方案是要求在像@racket[get-price]方法中使用@racket[(send this get-color)]。这种更改是有效的，因为@racket[send]总是延迟方法查找，直到对方法的调用被求值。然而，延迟查找比直接调用更为昂贵。更糟糕的是，它也延迟检查@racket[get-color]方法是否存在。

@;{A second, effective, and efficient solution is to change the encoding
of traits. Specifically, we represent each method as a pair of mixins:
one that introduces the method and one that implements it. When a
trait is applied to a class, all of the method-introducing mixins are
applied first. Then the method-implementing mixins can use
@racket[inherit] to directly access any introduced method.}
第二个，实际上，并且有效的解决方案是改变特征编码。具体来说，我们代表每个方法作为一对混合：一个引入方法，另一个实现它。当一个特征应用于一个类，所有的引入方法混合首先被应用。然后实现方法混合可以使用@racket[inherit]去直接访问任何引入的方法。

@racketblock[
(define spots-trait
  (list (list (local-member-name-key get-color)
              (lambda (get-color get-price %) ....
                (class % ....
                  (define/public (get-color) (void))))
              (lambda (get-color get-price %) ....
                (class % ....
                  (define/override (get-color) 'black))))
        (list (local-member-name-key get-price)
              (lambda (get-price get-color %) ....
                (class % ....
                  (define/public (get-price) (void))))
              (lambda (get-color get-price %) ....
                (class % ....
                  (inherit get-color)
                  (define/override (get-price)
                    .... (get-color) ....))))))
]

@;{With this trait encoding, @racket[trait-alias] adds a new method with
a new name, but it does not change any references to the old method.}
有了这个特性编码，  @racket[trait-alias]添加一个带新名称的新方法，但它不会改变对旧方法的任何引用。

@;==============================================================
@;{@subsection{The @racket[trait] Form}}
@subsection[#:tag "The-trait-Form"]{@racket[trait]表}

@;{The general-purpose trait pattern is clearly too complex for a
programmer to use directly, but it is easily codified in a
@racket[trait] macro:}
通用特性模式显然对程序员直接使用来说太复杂了，但很容易在@racket[trait]宏中编译：

@specform[
(trait trait-clause ...)
]

The @racket[id]s in the optional @racket[inherit] clause are available for direct
reference in the method @racket[expr]s, and they must be supplied
either by other traits or the base class to which
the trait is ultimately applied.
在可选项的@racket[inherit]（继承）从句中的@racket[id]对@racket[expr]方法中的直接引用是有效的，并且它们必须提供其它特征或者基类，其特征被最终应用。

@;{Using this form in conjunction with trait operators such as
@racket[trait-sum], @racket[trait-exclude], @racket[trait-alias], and
@racket[trait->mixin], we can implement @racket[spots-trait] and
@racket[stripes-trait] as desired.}
使用这个表结合特征操作符，如@racket[trait-sum]、@racket[trait-exclude]、@racket[trait-alias]和@racket[trait->mixin],我们可以实现@racket[spots-trait]和@racket[stripes-trait]作为所需。

@racketblock[
(define spots-trait
  (trait
    (define/public (get-color) 'black)
    (define/public (get-price) ... (get-color) ...)))

(define stripes-trait
  (trait 
    (define/public (get-color) 'red)))

(define spots+stripes-trait
  (trait-sum
   (trait-exclude (trait-alias spots-trait
                               get-color get-spots-color)
                  get-color)
   (trait-exclude (trait-alias stripes-trait
                               get-color get-stripes-color)
                  get-color)
   (trait
     (inherit get-spots-color get-stripes-color)
     (define/public (get-color)
       .... (get-spots-color) .... (get-stripes-color) ....))))
]

@; ----------------------------------------------------------------------

@; Set up uses of contract forms below
@(class-eval '(require racket/contract))

@;===============================================
@;{@section{Class Contracts}}
@section[#:tag "Class-Contracts"]{类合约}

@;{As classes are values, they can flow across contract boundaries, and we
may wish to protect parts of a given class with contracts.  For this,
the @racket[class/c] form is used.  The @racket[class/c] form has many
subforms, which describe two types of contracts on fields and methods:
those that affect uses via instantiated objects and those that affect
subclasses.}
由于类是值，它们可以跨越合约边界，我们可能希望用合约保护给定类的一部分。为此，使用@racket[class/c]表。@racket[class/c]表具有许多子表，其描述关于字段和方法两种类型的合约：有些通过实例化对象影响使用，有些影响子类。

@;{@subsection{External Class Contracts}}
@subsection[#:tag "External-Class-Contracts"]{外部类合约}

@;{In its simplest form, @racket[class/c] protects the public fields and methods
of objects instantiated from the contracted class.  There is also an
@racket[object/c] form that can be used to similarly protect the public fields
and methods of a particular object. Take the following definition of
@racket[animal%], which uses a public field for its @racket[size] attribute:}
在最简单的表中，@racket[class/c]保护从合约类实例化的对象的公共字段和方法。还有一种@racket[object/c]表，可用于类似地保护特定对象的公共字段和方法。获取@racket[animal%]的以下定义，它使用公共字段作为其@racket[size]属性：

@racketblock[
(define animal%
  (class object% 
    (super-new)
    (field [size 10])
    (define/public (eat food)
      (set! size (+ size (get-field size food))))))]

@;{For any instantiated @racket[animal%], accessing the @racket[size] field
should return a positive number.  Also, if the @racket[size] field is set,
it should be assigned a positive number.  Finally, the @racket[eat] method
should receive an argument which is an object with a @racket[size] field
that contains a positive number. To ensure these conditions, we will define
the @racket[animal%] class with an appropriate contract:}
对于任何实例化的@racket[animal%]，访问@racket[size]字段应该返回一个正数。另外，如果设置了@racket[size]字段，则应该分配一个正数。最后，@racket[eat]方法应该接收一个参数，它是一个包含一个正数的@racket[size]字段的对象。为了确保这些条件，我们将用适当的合约定义@racket[animal%]类：

@racketblock[
(define positive/c (and/c number? positive?))
(define edible/c (object/c (field [size positive/c])))
(define/contract animal%
  (class/c (field [size positive/c])
           [eat (->m edible/c void?)])
  (class object% 
    (super-new)
    (field [size 10])
    (define/public (eat food)
      (set! size (+ size (get-field size food))))))]

@interaction-eval[
#:eval class-eval
(begin
  (define positive/c
    (flat-named-contract 'positive/c (and/c number? positive?)))
  (define edible/c (object/c (field [size positive/c])))
  (define/contract animal%
    (class/c (field [size positive/c])
             [eat (->m edible/c void?)])
    (class object% 
      (super-new)
      (field [size 10])
      (define/public (eat food)
        (set! size (+ size (get-field size food)))))))]

@;{Here we use @racket[->m] to describe the behavior of @racket[eat] since we
do not need to describe any requirements for the @racket[this] parameter.
Now that we have our contracted class, we can see that the contracts
on both @racket[size] and @racket[eat] are enforced:}
这里我们使用@racket[->m]来描述@racket[eat]的行为，因为我们不需要描述这个@racket[this]参数的任何要求。既然我们有我们的合约类，就可以看出对@racket[size]和@racket[eat]的合约都是强制执行的：

@interaction[
#:eval class-eval
(define bob (new animal%))
(set-field! size bob 3)
(get-field size bob)
(set-field! size bob 'large)
(define richie (new animal%))
(send bob eat richie)
(get-field size bob)
(define rock (new object%))
(send bob eat rock)
(define giant (new (class object% (super-new) (field [size 'large]))))
(send bob eat giant)]

@;{There are two important caveats for external class contracts. First,
external method contracts are only enforced when the target of dynamic
dispatch is the method implementation of the contracted class, which
lies within the contract boundary.  Overriding that implementation, and
thus changing the target of dynamic dispatch, will mean that the contract
is no longer enforced for clients, since accessing the method no longer
crosses the contract boundary.  Unlike external method contracts, external
field contracts are always enforced for clients of subclasses, since fields
cannot be overridden or shadowed.}
对于外部类合同有两个重要的警告。首先，当动态分派的目标是合约类的方法实施时，只有在合同边界内才实施外部方法合同。重写该实现，从而改变动态分派的目标，将意味着不再为客户机强制执行该合约，因为访问该方法不再越过合约边界。与外部方法合约不同，外部字段合约对于子类的客户机总是强制执行，因为字段不能被覆盖或屏蔽。

@;{Second, these contracts do not restrict subclasses of @racket[animal%]
in any way.  Fields and methods that are inherited and used by subclasses
are not checked by these contracts, and uses of the superclass's methods
via @racket[super] are also unchecked.  The following example illustrates
both caveats:}
第二，这些合约不以任何方式限制@racket[animal%]的子类。被子类继承和使用的字段和方法不被这些合约检查，并且通过@racket[super]对基类方法的使用也不检查。下面的示例说明了两个警告：

@def+int[
#:eval class-eval
(define large-animal%
  (class animal%
    (super-new)
    (inherit-field size)
    (set! size 'large)
    (define/override (eat food)
      (display "Nom nom nom") (newline))))
(define elephant (new large-animal%))
(send elephant eat (new object%))
(get-field size elephant)]

@;===========================================
@;{@subsection{Internal Class Contracts}}
@subsection[#:tag "Internal-Class-Contracts"]{内部类合约}

@;{Notice that retrieving the @racket[size] field from the object
@racket[elephant] blames @racket[animal%] for the contract violation.
This blame is correct, but unfair to the @racket[animal%] class,
as we have not yet provided it with a method for protecting itself from
subclasses.  To this end we add internal class contracts, which
provide directives to subclasses for how they may access and override
features of the superclass.  This distinction between external and internal
class contracts allows for weaker contracts within the class hierarchy, where
invariants may be broken internally by subclasses but should be enforced
for external uses via instantiated objects.}
注意，从@racket[elephant]对象检索@racket[size]字段归咎于@racket[animal%]违反合约。这种归咎是正确的，但对@racket[animal%]类来说是不公平的，因为我们还没有提供一种保护自己免受子类攻击的方法。为此我们添加内部类合约，它提供指令给子类以指明它们如何访问和重写基类的特征。外部类和内部类合约之间的区别在于是否允许类层次结构中较弱的合约，其不变性可能被子类内部破坏，但应通过实例化的对象强制用于外部使用。

@;{As a simple example of what kinds of protection are available, we provide
an example aimed at the @racket[animal%] class that uses all the applicable
forms:}
作为可用的保护类型的简单示例，我们提供了一个针对@racket[animal%]类的示例，它使用所有适用的表：

@racketblock[
(class/c (field [size positive/c])
         (inherit-field [size positive/c])
         [eat (->m edible/c void?)]
         (inherit [eat (->m edible/c void?)])
         (super [eat (->m edible/c void?)])
         (override [eat (->m edible/c void?)]))]

@;{This class contract not only ensures that objects of class @racket[animal%]
are protected as before, but also ensure that subclasses of @racket[animal%]
only store appropriate values within the @racket[size] field and use
the implementation of @racket[size] from @racket[animal%] appropriately.
These contract forms only affect uses within the class hierarchy, and only
for method calls that cross the contract boundary.}
这个类合约不仅确保@racket[animal%]类的对象像以前一样受到保护，而且确保@racket[animal%]类的子类只在@racket[size]字段中存储适当的值，并适当地使用@racket[animal%]的@racket[size]实现。这些合约表只影响类层次结构中的使用，并且只影响跨合约边界的方法调用。

@;{That means that @racket[inherit] will only affect subclass uses of a method
until a subclass overrides that method, and that @racket[override] only
affects calls from the superclass into a subclass's overriding implementation
of that method.  Since these only affect internal uses, the @racket[override]
form does not automatically enter subclasses into obligations when objects of
those classes are used.  Also, use of @racket[override] only makes sense, and
thus can only be used, for methods where no Beta-style augmentation has taken
place. The following example shows this difference:}
这意味着，@racket[inherit]（继承）只会影响到一个方法的子类使用直到子类重写方法，而@racket[override]只影响从基类进入方法的子类的重写实现。由于这些仅影响内部使用，所以在使用这些类的对象时，override表不会自动将子类插入到义务（obligations）中。此外，使用@racket[override]仅是说得通，因此只能用于没有beta样式增强的方法。下面的示例显示了这种差异：

@racketblock[
(define/contract sloppy-eater%
  (class/c [eat (->m edible/c edible/c)])
  (begin
    (define/contract glutton%
      (class/c (override [eat (->m edible/c void?)]))
      (class animal%
        (super-new)
        (inherit eat)
        (define/public (gulp food-list)
          (for ([f food-list])
            (eat f)))))
    (class glutton%
      (super-new)
      (inherit-field size)
      (define/override (eat f)
        (let ([food-size (get-field size f)])
          (set! size (/ food-size 2))
          (set-field! size f (/ food-size 2))
          f)))))]

@interaction-eval[
#:eval class-eval
(define/contract sloppy-eater%
  (class/c [eat (->m edible/c edible/c)])
  (begin
    (define/contract glutton%
      (class/c (override [eat (->m edible/c void?)]))
      (class animal%
        (super-new)
        (inherit eat)
        (define/public (gulp food-list)
          (for ([f food-list])
            (eat f)))))
    (class glutton%
      (super-new)
      (inherit-field size)
      (define/override (eat f)
        (let ([food-size (get-field size f)])
          (set! size (/ food-size 2))
          (set-field! size f (/ food-size 2))
          f)))))]

@interaction[
#:eval class-eval
(define pig (new sloppy-eater%))
(define slop1 (new animal%))
(define slop2 (new animal%))
(define slop3 (new animal%))
(send pig eat slop1)
(get-field size slop1)
(send pig gulp (list slop1 slop2 slop3))]

@;{In addition to the internal class contract forms shown here, there are
similar forms for Beta-style augmentable methods.  The @racket[inner]
form describes to the subclass what is expected from augmentations of
a given method.  Both @racket[augment] and @racket[augride] tell the
subclass that the given method is a method which has been augmented and
that any calls to the method in the subclass will dynamically
dispatch to the appropriate implementation in the superclass.  Such
calls will be checked according to the given contract.  The two forms
differ in that  use of @racket[augment] signifies that subclasses can
augment the given method, whereas use of @racket[augride] signifies that
subclasses must override the current augmentation instead.}
除了这里的内部类合约表所显示的之外，这里有beta样式可扩展的方法类似的表。@racket[inner]表描述了这个子类，它被要求从一个给定的方法扩展。@racket[augment]和@racket[augride]告诉子类，该给定的方法是一种被增强的方法，并且对子类方法的任何调用将动态分配到基类中相应的实现。这样的调用将根据给定的合约进行检查。这两种表的区别在于@racket[augment]的使用意味着子类可以增强给定的方法，而@racket[augride]的使用表示子类必须反而重写当前增强。

@;{This means that not all forms can be used at the same time.  Only one of the
@racket[override], @racket[augment], and @racket[augride] forms can be used
for a given method, and none of these forms can be used if the given method
has been finalized.  In addition, @racket[super] can be specified for a given
method only if @racket[augride] or @racket[override] can be specified.
Similarly, @racket[inner] can be specified only if @racket[augment] or
@racket[augride] can be specified.}
这意味着并不是所有的表都可以同时使用。只有@racket[override]、@racket[augment]和@racket[augride]中的一个表可用于一个给定的方法，而如果给定的方法已经完成，这些表没有一个可以使用。此外， 仅在@racket[augride]或@racket[override]可以指定时，@racket[super]可以被指定为一个给定的方法。同样，只有@racket[augment]或@racket[augride]可以指定时，@racket[inner]可以被指定。

@; ----------------------------------------------------------------------

@close-eval[class-eval]
