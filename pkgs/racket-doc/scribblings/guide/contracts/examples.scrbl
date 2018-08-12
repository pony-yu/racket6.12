#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/contract racket/gui))

@;{@title[#:tag "contracts-examples"]{Additional Examples}}
@title[#:tag "contracts-examples"]{附加实例}

 @;{This section illustrates the current state of Racket's contract
 implementation with a series of examples from @italic{Design by
 Contract, by Example} @cite["Mitchell02"].}
本节说明Racket合约实施的当前状态，用一系列来自于@italic{《Design by
 Contract, by Example》}@cite["Mitchell02"]的例子。

@;{Mitchell and McKim's principles for design by contract DbC are derived
  from the 1970s style algebraic specifications. The overall goal of DbC is
  to specify the constructors of an algebra in terms of its
  observers. While we reformulate Mitchell and McKim's terminology and 
  we use a mostly applicative approach, we
  retain their terminology of ``classes'' and ``objects'':}
米切尔（Mitchell）和麦金（McKim）的合约设计准则DbC源于1970年代风格的代数规范。DbC的总体目标是依据它的观察指定一个代数的构造器。当我们换种方式表达米切尔和麦金的术语同时我们用最适合的途径，我们保留他们的术语“类”（classes）和“对象”（objects）：

@itemize[
@item{
  @;{@bold{Separate queries from commands.}}
    @bold{从命令中分离查询。}

    @;{A @italic{query} returns a result but does not change the observable
    properties of an object. A @italic{command} changes the visible
    properties of an object, but does not return a result. In applicative
    implementation a command typically returns an new object of the same
    class.}
一个@italic{查询（query）}返回一个结果但不会改变一个对象的可观察性。一个@italic{命令（command）}改变一个对象的可见性但不返回结果。在应用程序实现中一个命令通常返回同一个类的一个新对象。
      }

@item{
  @;{@bold{Separate basic queries from derived queries.}}
    @bold{从派生查询中分离基本查询。}

    @;{A @italic{derived query} returns a result that is computable in
    terms of basic queries.}
      一个@italic{派生查询（derived query）}返回一个根据基本查询可计算的结果。
 }

@item{
  @;{@bold{For each derived query, write a post-condition contract that
    specifies the result in terms of the basic queries.}}
    @bold{对于每个派生查询，编写一个根据基本查询指定结果的岗位条件合约。}
    }

@item{
  @;{@bold{For each command, write a post-condition contract that specifies the
    changes to the observable properties in terms of the basic queries.}}
    @bold{对于每个命令，编写一个根据基本查询指定对可观测性更改的岗位条件合约。}
    }

@item{
  @;{@bold{For each query and command, decide on a suitable
pre-condition contract.}}
    @bold{对于每个查询和命令，决定一个合适的前置条件合约。}
  }]

@;{Each of the following sections corresponds to a chapter in
 Mitchell and McKim's book (but not all chapters show up
 here). We recommend that you read the contracts first (near
 the end of the first modules), then the implementation (in
 the first modules), and then the test module (at the end of
 each section).}
以下各节对应于在米切尔和麦金的书中的一章（但不是所有的章都显示在这里）。我们建议你先阅读合约（在第一模块的末尾附近），然后是实现（在第一个模块中），然后是测试模块（在每一节的结尾）。

@;{Mitchell and McKim use Eiffel as the underlying programming language and
 employ a conventional imperative programming style. Our long-term goal is
 to transliterate their examples into applicative Racket,
 structure-oriented imperative Racket, and Racket's class system.}
米切尔和麦金使用Eiffel语言作为底层编程语言同时采用一个传统的命令式编程风格。我们的长期目标是翻译他们的例子为有应用价值的Racket、面向结构的命令式Racket以及Racket的类系统。

@;{Note: To mimic Mitchell and McKim's informal notion of parametericity
 (parametric polymorphism), we use first-class contracts. At several
 places, this use of first-class contracts improves on Mitchell and McKim's
 design (see comments in interfaces).}
注：模仿米切尔和McKim的参数性非正式概念（参数多态性），我们用一类合约。在几个地方，一类合约的使用改进了米切尔和麦金的设计（参见接口中的注释）。

@;{@section{A Customer-Manager Component}}
@section[#:tag "A-Customer-Manager-Component"]{一个客户管理器组建}

@;{This first module contains some struct definitions in a
separate module in order to better track bugs.}
为了更好地跟踪漏洞（bug），这第一个模块包含一个独立模块里的一些结构定义。

@external-file[1]

@;{This module contains the program that uses the above.}
该模块包含使用上述内容的程序。

@external-file[1b]

@;{The tests:}
测试：

@external-file[1-test]

@;{@section{A Parameteric (Simple) Stack}}
@section[#:tag "A-Parameteric-Simple-Stack"]{一个参数化（简单）栈}
    
@external-file[2]

@;{The tests:}
测试：

@external-file[2-test]

@;{@section{A Dictionary}}
@section[#:tag "A-Dictionary"]{一个字典}

@external-file[3]

@;{The tests:}
测试：

@external-file[3-test]

@;{@section{A Queue}}
@section[#:tag "A-Queue"]{一个队列}

@external-file[5]

@;{The tests:}
测试：

@external-file[5-test]
