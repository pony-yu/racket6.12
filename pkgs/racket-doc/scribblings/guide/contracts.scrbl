#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "contracts" #:style 'toc]{Contracts}}
@title[#:tag "contracts" #:style 'toc]{合约}

@;{This chapter provides a gentle introduction to Racket's
contract system.}
本章对Racket的合约系统提供了一个详细的介绍。

@;@refdetails["contracts"]{合约}
在《Racket参考》中的“合约（Contracts）”部分提供有对合约更详细的信息。

@local-table-of-contents[]

@;{

Somewhere, discuss eq? and its impact on lists and
procedures. 

Also, discuss difference between contracts on
mutable datastructures & contracts on immutable ones.

Fill in question on optional arguments in general-function contracts.

->d and dependency (commented out section in general contracts).

update string-pad-center to show examples via REPL notation:

(string-pad-center "nba" 10)
(code:comment "=> \"   abc    \"")

(string-pad-center "nba" 10 #\-) 
(code:comment "=> \"---abc----\"")


}


@include-section["contracts/intro.scrbl"]
@include-section["contracts/simple-function.scrbl"]
@include-section["contracts/general-function.scrbl"]
@include-section["contracts/first-extended-example.scrbl"]
@include-section["contracts/structure.scrbl"]
@include-section["contracts/exists.scrbl"]
@include-section["contracts/examples.scrbl"]
@include-section["contracts/new-combinators.scrbl"]
@include-section["contracts/gotchas.scrbl"]
