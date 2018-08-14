#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/contract racket/gui))

@(define ex-eval (make-base-eval))
@(ex-eval '(require racket/contract))

@;{@title{Building New Contracts}}
@title[#:tag "Building-New-Contracts"]{建立新合约}

@;{Contracts are represented internally as functions that
accept information about the contract (who is to blame,
source locations, @|etc|) and produce projections (in the
spirit of Dana Scott) that enforce the contract.}
合约在内部作为函数来表示，这个函数接受关于合约的信息（归咎于谁、源程序位置等等）并产生执行合约的推断（本着Dana Scott的精神）。

@;{In a general sense, a
projection is a function that accepts an arbitrary value,
and returns a value that satisfies the corresponding
contract. For example, a projection that accepts only
integers corresponds to the contract @racket[(flat-contract
integer?)], and can be written like this:}
一般意义上，一个推断是接受一个任意值的一个函数，并返回满足相应合约的一个值。例如，只接受整数的一个推断对应于合约@racket[(flat-contract
integer?)]，同时可以这样编写：

@racketblock[
(define int-proj
  (λ (x)
    (if (integer? x)
        x
        (signal-contract-violation))))
]

@;{As a second example, a projection that accepts unary functions
on integers looks like this:}
作为第二个例子，接受整数上的一元函数的一个推断看起来像这样：

@racketblock[
(define int->int-proj
  (λ (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (x) (int-proj (f (int-proj x))))
        (signal-contract-violation))))
]

@;{Although these projections have the right error behavior,
they are not quite ready for use as contracts, because they
do not accommodate blame and do not provide good error
messages. In order to accommodate these, contracts do not
just use simple projections, but use functions that accept a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{blame object} encapsulating
the names of two parties that are the candidates for blame,
as well as a record of the source location where the
contract was established and the name of the contract. They
can then, in turn, pass that information
to @racket[raise-blame-error] to signal a good error
message.}
虽然这些推断具有恰当的错误行为，但它们还不太适合作为合约使用，因为它们不容纳归咎也不提供良好的错误消息。为了适应这些，合约不只使用简单的推断，而是使用接受一个@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{归咎对象（blame object）}的函数将被归咎双方的名字封装起来，以及合约建立的源代码位置和合约名称的记录。然后，它们可以依次传递这些信息给@racket[raise-blame-error]来发出一个良好的错误信息。

@;{Here is the first of those two projections, rewritten for
use in the contract system:}
这里是这两个推断中的第一个，被重写以在合约系统中使用：

@racketblock[
(define (int-proj blame)
  (λ (x)
    (if (integer? x)
        x
        (raise-blame-error
         blame
         x
         '(expected: "<integer>" given: "~e")
         x))))
]

@;{The new argument specifies who is to be blamed for
positive and negative contract violations.}
新的论据指明了谁将因为正数和负数的合约违约被归咎。

@;{Contracts, in this system, are always
established between two parties. One party, called the server, provides some
value according to the contract, and the other, the client, consumes the
value, also according to the contract. The server is called
the positive position and the client the negative position. So,
in the case of just the integer contract, the only thing
that can go wrong is that the value provided is not an
integer. Thus, only the positive party (the server) can ever accrue
blame.  The @racket[raise-blame-error] function always blames
the positive party.}
在这个系统中，合约总是建立在双方之间。一方称为服务器，根据这个合约提供一些值；另一方称为客户端，也根据这个合约接受这些值。服务器称为主动位置，客户端称为被动位置。因此，对于仅在整数合约的情况下，唯一可能出错的是所提供的值不是一个整数。因此，永远只有主动的一方（服务器）才能获得归咎。@racket[raise-blame-error]函数总是归咎主动的一方。

@;{Compare that to the projection for our function contract:}
与我们的函数合约的推断的比较：

@racketblock[
(define (int->int-proj blame)
  (define dom (int-proj (blame-swap blame)))
  (define rng (int-proj blame))
  (λ (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (x) (rng (f (dom x))))
        (raise-blame-error
         blame
         f
         '(expected "a procedure of one argument" given: "~e")
         f))))
]

@;{In this case, the only explicit blame covers the situation
where either a non-procedure is supplied to the contract or
the procedure does not accept one argument. As with
the integer projection, the blame here also lies with the
producer of the value, which is
why @racket[raise-blame-error] is passed @racket[blame] unchanged.}
在这种情况下，唯一明确的归咎涵盖了一个提供给合约的非过程或一个这个不接受一个参数的过程的情况。与整数推断一样，这里的归咎也在于这个值的生成器，这就是为什么@racket[raise-blame-error]传递没有改变的@racket[blame]。

@;???????????????????????????????????????????????????????????
@;{The checking for the domain and range are delegated to
the @racket[int-proj] function, which is supplied its
arguments in the first two lines of
the @racket[int->int-proj] function. The trick here is that,
even though the @racket[int->int-proj] function always
blames what it sees as positive, we can swap the blame parties by
calling @racket[blame-swap] on the given
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{blame object}, replacing
the positive party with the negative party and vice versa.}
对于定义域和值域的检查是委托给@racket[int-proj]函数，它提供其参数在前面两行@racket[int->int-proj]函数。这里的诀窍是，即使@racket[int->int-proj]函数总是归咎于它所认为的正面，我们可以通过对给定的@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{归咎对象（blame object）}调用@racket[blame-swap]互换归咎的一方，取代正面一方与反面一方，反之亦然。

@;{This technique is not merely a cheap trick to get the example to work,
however. The reversal of the positive and the negative is a
natural consequence of the way functions behave. That is,
imagine the flow of values in a program between two
modules. First, one module (the server) defines a function, and then that
module is required by another (the client). So far, the function itself
has to go from the original, providing module to the
requiring module. Now, imagine that the requiring module
invokes the function, supplying it an argument. At this
point, the flow of values reverses. The argument is
traveling back from the requiring module to the providing
module! The client is ``serving'' the argument to the server,
and the server is receiving that value as a client.
And finally, when the function produces a result,
that result flows back in the original
direction from server to client.
Accordingly, the contract on the domain reverses
the positive and the negative blame parties, just like the flow
of values reverses.}
然而，这种技术并不仅仅是一个廉价的技巧来让这个例子发挥作用。正方和反方的反转是函数运作行为的自然结果。也就是说，想象在两个模块之间的程序中的值流。首先，一个模块（服务器）定义了一个函数，然后该模块由另一个模块（客户机）所依赖。到目前为止，函数本身必须从原来的，提供模块给依赖的模块。现在，假设依赖模块调用函数，为它提供一个参数。此时，值流反转。参数正在从依赖模块返回到提供模块！客户机正在将参数提供给服务器，服务器将作为客户机接收该值。最后，当函数产生一个结果时，结果会从服务器流向客户机。因此，定义域上的合约颠倒了正方和反方的责任方，就像值流逆转一样。

@;{We can use this insight to generalize the function contracts
and build a function that accepts any two contracts and
returns a contract for functions between them.}
我们可以利用这个领悟来概括函数合约并构建一个函数，它接受任意两个合约，并为它们之间的函数返回一个合约。

@;{This projection also goes further and uses
@racket[blame-add-context] to improve the error messages
when a contract violation is detected.}
这一推断也更进一步在检测到违反合约的情况下，使用@racket[blame-add-context]来改进错误消息。

@racketblock[
(define (make-simple-function-contract dom-proj range-proj)
  (λ (blame)
    (define dom (dom-proj (blame-add-context blame
                                             "the argument of"
                                             #:swap? #t)))
    (define rng (range-proj (blame-add-context blame
                                               "the range of")))
    (λ (f)
      (if (and (procedure? f)
               (procedure-arity-includes? f 1))
          (λ (x) (rng (f (dom x))))
          (raise-blame-error
           blame
           f
           '(expected "a procedure of one argument" given: "~e")
           f)))))
]

@;{While these projections are supported by the contract library
and can be used to build new contracts, the contract library
also supports a different API for projections that can be more
efficient. Specifically, a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{late neg projection} accepts
a blame object without the negative blame information and then
returns a function that accepts both the value to be contracted and
the name of the negative party, in that order.
The returned function then in turn
returns the value with the contract. Rewriting @racket[int->int-proj]
to use this API looks like this:}
虽然这些推断得到了合约库的支持，并且可以用来构建新的合约，但是合约库也支持不同的API来进行更有效的推断。具体来说，一个@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{后负推断（late neg projection）}接受一个归咎对象不带反面归咎的信息，然后按照这个顺序返回一个函数，它既接受合约约定的值也接受反方的名称。然后返回的函数依次根据合约返回值。重写@racket[int->int-proj]以使用这个API，看起来像这样：

@interaction/no-prompt[#:eval ex-eval
(define (int->int-proj blame)
  (define dom-blame (blame-add-context blame
                                       "the argument of"
                                       #:swap? #t))
  (define rng-blame (blame-add-context blame "the range of"))
  (define (check-int v to-blame neg-party)
    (unless (integer? v)
      (raise-blame-error
       to-blame #:missing-party neg-party
       v
       '(expected "an integer" given: "~e")
       v)))
  (λ (f neg-party)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (x)
          (check-int x dom-blame neg-party)
          (define ans (f x))
          (check-int ans rng-blame neg-party)
          ans)
        (raise-blame-error
         blame #:missing-party neg-party
         f
         '(expected "a procedure of one argument" given: "~e")
         f))))]

@;{The advantage of this style of contract is that the @racket[_blame]
argument can be supplied on the server side of the
contract boundary and the result can be used for each different
client. With the simpler situation, a new blame object has to be
created for each client.}
这种类型的合约的优点是，可以在合同范围的服务器端提供@racket[_blame]参数，而且这个结果可以用于每个不同的客户机。在较简单的情况下，必须为每个客户机创建一个新的归咎对象。

@;{One final problem remains before this contract can be used with the
rest of the contract system. In the function above,
the contract is implemented by creating a wrapper function for
@racket[f], but this wrapper function does not cooperate with
@racket[equal?], nor does it let the runtime system know that there
is a relationship between the result function and @racket[f], the input
function.}
最后一个问题仍然是这个合约可以与合约系统的其它部分一起使用。在上面的函数中，通过为@racket[f]创建一个包装函数来实现这个合约，但是这个包装器函数与@racket[equal?]不协作，它也不让运行时系统知道结果函数与输入函数@racket[f]之间的关系。

@;{To remedy these two problems, we should use
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperones} instead
of just using @racket[λ] to create the wrapper function. Here is the
@racket[int->int-proj] function rewritten to use a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperone}:}
为了解决这两个问题，我们应该使用@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{陪护（chaperones）}而不是仅仅使用@racket[λ]创建包装函数。这里是@racket[int->int-proj]函数被重写以使用@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{陪护（chaperone）}：

@interaction/no-prompt[#:eval ex-eval
(define (int->int-proj blame)
  (define dom-blame (blame-add-context blame
                                       "the argument of"
                                       #:swap? #t))
  (define rng-blame (blame-add-context blame "the range of"))
  (define (check-int v to-blame neg-party)
    (unless (integer? v)
      (raise-blame-error
       to-blame #:missing-party neg-party
       v
       '(expected "an integer" given: "~e")
       v)))
  (λ (f neg-party)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (chaperone-procedure
         f
         (λ (x)
           (check-int x dom-blame neg-party)
           (values (λ (ans)
                     (check-int ans rng-blame neg-party)
                     ans)
                   x)))
        (raise-blame-error
         blame #:missing-party neg-party
         f
         '(expected "a procedure of one argument" given: "~e")
         f))))]

@;{Projections like the ones described above, but suited to
other, new kinds of value you might make, can be used with
the contract library primitives. Specifically, we can use 
@racket[make-chaperone-contract] to build it:}
推断就像如上所述的一个情况，但适合于其它你可能制造的新类型的值，可以与合约库的基本类型一起使用。具体来说，我们可以用@racket[make-chaperone-contract]来构建它：

@interaction/no-prompt[#:eval ex-eval
 (define int->int-contract
   (make-contract
    #:name 'int->int
    #:late-neg-projection int->int-proj))]

@;{and then combine it with a value and get some contract
checking.}
然后将其与一个值相结合，得到一些合约检查。

@def+int[#:eval 
         ex-eval
         (define/contract (f x)
           int->int-contract
           "not an int")
         (f #f)
         (f 1)]

@;{@section{Contract Struct Properties}}
@section[#:tag "Contract-Struct-Properties"]{合约结构属性}

@;{The @racket[make-chaperone-contract] function is okay for one-off contracts,
but often you want to make many different contracts that differ only
in some pieces. The best way to do that is to use a @racket[struct]
with either @racket[prop:contract], @racket[prop:chaperone-contract], or
@racket[prop:flat-contract]. }
对于一次性合约来说，@racket[make-chaperone-contract]函数是可以的，但通常你想做许多不同的合约，只在某些部分有所不同。做到这一点的最好方法是使用一个@racket[struct]（结构），带@racket[prop:contract]、@racket[prop:chaperone-contract]或@racket[prop:flat-contract]。

@;{For example, lets say we wanted to make a simple form of the @racket[->]
contract that accepts one contract for the range and one for the domain. 
We should define a struct with two fields and use 
@racket[build-chaperone-contract-property] to construct the chaperone contract
property we need.}
例如，假设我们想做一个@racket[->]合约的简单表，它接受值域的一个合约和定义域的一个合约。我们应该定义一个具有两个字段的结构，并使用@racket[build-chaperone-contract-property]构建我们需要的监护合约属性。

@interaction/no-prompt[#:eval ex-eval
                              (struct simple-arrow (dom rng)
                                #:property prop:chaperone-contract
                                (build-chaperone-contract-property
                                 #:name
                                 (λ (arr) (simple-arrow-name arr))
                                 #:late-neg-projection
                                 (λ (arr) (simple-arrow-late-neg-proj arr))))]

@;{To do the automatic coercion of values like @racket[integer?] and @racket[#f]
into contracts, we need to call @racket[coerce-chaperone-contract]
(note that this rejects impersonator contracts and does not insist
on flat contracts; to do either of those things, call @racket[coerce-contract]
or @racket[coerce-flat-contract] instead).}
要像@racket[integer?]和@racket[#f]那样对值自动强制进入合约，我们需要调用@racket[coerce-chaperone-contract]（注意这个拒绝模拟合约并对扁平合约不予坚持；做的既可以是这些事情、调用@racket[coerce-contract]，也可以是代替@racket[coerce-flat-contract]。

@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-contract dom rng)
                                (simple-arrow (coerce-contract 'simple-arrow-contract dom)
                                              (coerce-contract 'simple-arrow-contract rng)))]

@;{To define @racket[_simple-arrow-name] is straight-forward; it needs to return
an s-expression representing the contract:}
定义@racket[_simple-arrow-name]是直截了当的；它需要返回一个S表达式来表达合约：

@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-name arr)
                                `(-> ,(contract-name (simple-arrow-dom arr))
                                     ,(contract-name (simple-arrow-rng arr))))]

@;{And we can define the projection using a generalization of the 
projection we defined earlier, this time using 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperones}:}
我们可以用我们前面定义的推断的一般化形式定义一个推断，这个时候使用@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{监护（chaperones）}：

@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-late-neg-proj arr)
                         (define dom-ctc (get/build-late-neg-projection (simple-arrow-dom arr)))
                         (define rng-ctc (get/build-late-neg-projection (simple-arrow-rng arr)))
                         (λ (blame)
                           (define dom+blame (dom-ctc (blame-add-context blame
                                                                         "the argument of"
                                                                         #:swap? #t)))
                           (define rng+blame (rng-ctc (blame-add-context blame "the range of")))
                           (λ (f neg-party)
                             (if (and (procedure? f)
                                      (procedure-arity-includes? f 1))
                                 (chaperone-procedure
                                  f
                                  (λ (arg) 
                                    (values 
                                     (λ (result) (rng+blame result neg-party))
                                     (dom+blame arg neg-party))))
                                 (raise-blame-error
                                  blame #:missing-party neg-party
                                  f
                                  '(expected "a procedure of one argument" given: "~e")
                                  f)))))]

@def+int[#:eval 
         ex-eval
         (define/contract (f x)
           (simple-arrow-contract integer? boolean?)
           "not a boolean")
         (f #f)
         (f 1)]

@;{@section{With all the Bells and Whistles}}
@section[#:tag "With-all-the-Bells-and-Whistles"]{所有的警告和报警}

@;{There are a number of optional pieces to a contract that 
@racket[simple-arrow-contract] did not add. In this section,
we walk through all of them to show examples of how they can
be implemented.}
合约中有一些可选部分，@racket[simple-arrow-contract]没有添加。在这一节中，我们将通过所有这些步骤来演示如何实现这些示例。

@;{The first is a first-order check. This is used by @racket[or/c]
in order to determine which of the higher-order argument contracts
to use when it sees a value. Here's the function for 
our simple arrow contract.}
第一个是一阶检查。这是用@racket[or/c]来确定当它看到一个值时使用哪个高阶参数合约。下面是我们简单箭头合约的功能。

@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-first-order ctc)
                                (λ (v) (and (procedure? v) 
                                            (procedure-arity-includes? v 1))))]

@;{It accepts a value and returns @racket[#f] if the value is guaranteed not
to satisfy the contract, and @racket[#t] if, as far as we can tell, 
the value satisfies the contract, just be inspecting first-order
properties of the value.}
它接受一个值并返回@racket[#f]，如果这个值确实不满足合同，并且如返回@racket[#t]，只要我们能够辨别这个值满足合约，就正是这个值的一阶属性检查。

@;{The next is random generation. Random generation in the contract
library consists of two pieces: the ability to randomly generate
values satisfying the contract and the ability to exercise values
that match the contract that are given, in the hopes of finding bugs
in them (and also to try to get them to produce interesting values to
be used elsewhere during generation).}
下一个是随机的生成。合约库中的随机生成分为两部分：随机运用满足合约值的能力和与应用与给定合约相匹配的值的能力，希望发现其中的错误（并试图使它们生成在生成期间在其他地方使用的有趣值）。

@;{To exercise contracts, we need to implement a function that
is given a @racket[arrow-contract] struct and some fuel. It should return
two values: a function that accepts values of the contract
and exercises them, plus a list of values that the exercising
process will always produce. In the case of our simple
contract, we know that we can always produce values of the range,
as long as we can generate values of the domain (since we can just
call the function). So, here's a function that matches the 
@racket[_exercise] argument of @racket[build-chaperone-contract-property]'s
contract:}
为了运用合约，我们需要实现一个赋予@racket[arrow-contract]结构和一些辅助函数。它应该返回两个值：一个函数，它接受合约值并运用它们；外加一个值列表，这个运用过程总会产生。对于我们简单的合约，我们知道我们总能产生值域的值，只要我们可以生成定义域的值（因为我们可以调用函数）。因此，这里有一个函数，它与@racket[build-chaperone-contract-property]合约的@racket[_exercise]参数相匹配：

@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-contract-exercise arr)
                         (define env (contract-random-generate-get-current-environment))
                         (λ (fuel)
                           (define dom-generate 
                             (contract-random-generate/choose (simple-arrow-dom arr) fuel))
                           (cond
                             [dom-generate
                              (values 
                               (λ (f) (contract-random-generate-stash
                                       env
                                       (simple-arrow-rng arr)
                                       (f (dom-generate))))
                               (list (simple-arrow-rng arr)))]
                             [else
                              (values void '())])))]

@;{If the domain contract can be generated, then we know we can do some good via exercising.
In that case, we return a procedure that calls @racket[_f] (the function matching
the contract) with something that we generated from the domain, and we stash the result
value in the environment too. We also return @racket[(simple-arrow-rng arr)]
to indicate that exercising will always produce something of that contract.}
如果定义域合约可以产生，那么我们知道我们可以通过运用来做一些好的事。在这种情况下，我们返回一个过程，它用我们从定义域生成的东西调用@racket[_f]（这个函数与合约匹配），而且我们也将结果值保存在环境中。我们也返回@racket[(simple-arrow-rng arr)]表明运用总是会产生合约的东西。

@;{If we cannot, then we simply return a function that
does no exercising (@racket[void]) and the empty list (indicating that we won't generate
any values).}
如果不能，那么我们只简单地返回一个函数，它不运用@racket[(void)]和空列表（表示我们不会生成任何值）。

@;{Then, to generate values matching the contract, we define a function
that when given the contract and some fuel, makes up a random function.
To help make it a more effective testing function, we can exercise
any arguments it receives, and also stash them into the generation
environment, but only if we can generate values of the range contract.}
然后，为了生成与合约相匹配的值，我们定义了一个函数，当给定合约和辅助函数时，它构成一个随机函数。为了帮助它成为一个更有效的测试函数，我们可以运用它接受的任何参数，并也将它们保存到生成环境中，但前提是我们可以生成值域合约的值。

@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-contract-generate arr)
                         (λ (fuel)
                           (define env (contract-random-generate-get-current-environment))
                           (define rng-generate 
                             (contract-random-generate/choose (simple-arrow-rng arr) fuel))
                           (cond
                             [rng-generate
                              (λ ()
                                (λ (arg)
                                  (contract-random-generate-stash env (simple-arrow-dom arr) arg)
                                  (rng-generate)))]
                             [else
                              #f])))]

@;{When the random generation pulls something out of the environment,
it needs to be able to tell if a value that has been passed to
@racket[contract-random-generate-stash] is a candidate for
the contract it is trying to generate. Of course, it the contract
passed to @racket[contract-random-generate-stash] is an exact
match, then it can use it. But it can also use the value if the
contract is stronger (in the sense that it accepts fewer values).}
当随机生成将环境中的某个东西拉出时，它需要能够判断是否一个被传递给@racket[contract-random-generate-stash]的值是试图生成的合约的候选对象。当然，合约传递给@racket[contract-random-generate-stash]的是一个精确的匹配，然后它就可以使用它。但是，如果合约更强大的话它也可以使用这个价值（在它接受更少的值的意义上）。

@;{To provide that functionality, we implement this function:}
为了提供这个功能，我们实现了这个函数：

@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-first-stronger? this that)
                                (and (simple-arrow? that)
                                     (contract-stronger? (simple-arrow-dom that)
                                                         (simple-arrow-dom this))
                                     (contract-stronger? (simple-arrow-rng this)
                                                         (simple-arrow-rng that))))]

@;{This function accepts @racket[_this] and @racket[_that], two contracts. It is
guaranteed that @racket[_this] will be one of our simple arrow contracts,
since we're supplying this function together with the simple arrow implementation.
But the @racket[_that] argument might be any contract. This function
checks to see if @racket[_that] is also a simple arrow contract and, if so
compares the domain and range. Of course, there are other contracts that we
could also check for (e.g., contracts built using @racket[->] or @racket[->*]),
but we do not need to. The stronger function is allowed to return @racket[#f]
if it doesn't know the answer but if it returns @racket[#t], then the contract
really must be stronger.}
这个函数接受@racket[_this]和@racket[_that]，两个合约。它保证@racket[_this]将是我们简单的箭头合约之一，因为我们将此函数与简单的箭头实现一起提供。但@racket[_that]参数可能是任何合约。如果同样比较定义域和值域，这个函数通过检查弄明白@racket[_that]是否也是一个简单的箭头合约。当然，还有其它的合约，我们也可以检查（例如，使用@racket[->]或@racket[->*]建立的合约），但我们并不需要。更强的函数是如果不知道答案，允许返回@racket[#f]；但如果它返回@racket[#t]，那么这个合约必须真正地强健。

@;{Now that we have all of the pieces implemented, we need to pass them
to @racket[build-chaperone-contract-property] so the contract system
starts using them:}
既然我们已经完成了所有的部分，我们需要传递它们给@racket[build-chaperone-contract-property]，这样合约系统就开始使用它们了：

@interaction/no-prompt[#:eval ex-eval
                              (struct simple-arrow (dom rng)
                                #:property prop:custom-write contract-custom-write-property-proc
                                #:property prop:chaperone-contract
                                (build-chaperone-contract-property
                                 #:name
                                 (λ (arr) (simple-arrow-name arr))
                                 #:late-neg-projection
                                 (λ (arr) (simple-arrow-late-neg-proj arr))
                                 #:first-order simple-arrow-first-order
                                 #:stronger simple-arrow-first-stronger?
                                 #:generate simple-arrow-contract-generate
                                 #:exercise simple-arrow-contract-exercise))
                              
                              (define (simple-arrow-contract dom rng)
                                (simple-arrow (coerce-contract 'simple-arrow-contract dom)
                                              (coerce-contract 'simple-arrow-contract rng)))]

@;{We also add a @racket[prop:custom-write] property so
that the contracts print properly, e.g.:}
我们还添加了一个@racket[prop:custom-write]属性，使合约正确打印，例如：

@interaction[#:eval ex-eval (simple-arrow-contract integer? integer?)]

@;{(We use @racket[prop:custom-write] because the contract library
can not depend on @racketmod[racket/generic] but yet still wants
to provide some help to make it easy to use the right printer.)}
（我们使用@racket[prop:custom-write]，因为合约库不能依赖于@racketmod[racket/generic]但仍然希望提供一些帮助，以便于使用正确的打印机。）

@;{Now that that's done, we can use the new functionality. Here's a random function, 
generated by the contract library, using our @racket[simple-arrow-contract-generate]
function:}
既然那些已经完成，我们就可以使用新功能。这里是一个随机函数，它由合约库生成，使用我们的@racket[simple-arrow-contract-generate]函数：

@def+int[#:eval 
         ex-eval
         (define a-random-function
           (contract-random-generate 
            (simple-arrow-contract integer? integer?)))
         (a-random-function 0)
         (a-random-function 1)]

@;{Here's how the contract system can now automatically find bugs in functions
that consume simple arrow contracts:}
下面是体现合约系统现在如何在使用简单箭头合约的函数中自动发现bug：

@def+int[#:eval 
         ex-eval
         (define/contract (misbehaved-f f)
           (-> (simple-arrow-contract integer? boolean?) any)
           (f "not an integer"))
         (contract-exercise misbehaved-f)]

@;{And if we hadn't implemented @racket[simple-arrow-first-order], then
@racket[or/c] would not be able to tell which branch of the @racket[or/c]
to use in this program:}
如果我们没有实现@racket[simple-arrow-first-order]，那么@racket[or/c]就不能判断这个程序中使用哪一个@racket[or/c]分支：

@def+int[#:eval
         ex-eval
         (define/contract (maybe-accepts-a-function f)
           (or/c (simple-arrow-contract real? real?)
                 (-> real? real? real?)
                 real?)
           (if (procedure? f)
               (if (procedure-arity-includes f 1)
                   (f 1132)
                   (f 11 2))
               f))
         (maybe-accepts-a-function sqrt)
         (maybe-accepts-a-function 123)]

@(close-eval ex-eval)
