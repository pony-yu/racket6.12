#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "hash-tables"]{Hash Tables}}
@title[#:tag "hash-tables"]{哈希表（Hash Table）}

@;{A @deftech{hash table} implements a mapping from keys to values, where
both keys and values can be arbitrary Racket values, and access and
update to the table are normally constant-time operations. Keys are
compared using @racket[equal?], @racket[eqv?], or @racket[eq?], depending on whether the hash table is created with @racket[make-hash],
@racket[make-hasheqv], or @racket[make-hasheq].}
一个@deftech{哈希表（hash table）}实现了从键到值的一个映射，其中键和值都可以是任意的Racket值，以及对表的访问和更新通常是常量时间操作。键的比较使用@racket[equal?]、@racket[eqv?]或@racket[eq?]，取决于哈希表创建方式是否为@racket[make-hash]、@racket[make-hasheqv]或@racket[make-hasheq]。

@examples[
(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(hash-ref ht "apple")
(hash-ref ht "coconut")
(hash-ref ht "coconut" "not there")
]

@;{The @racket[hash], @racket[hasheqv], and @racket[hasheq] functions
create immutable hash tables from an initial set of keys and values,
in which each value is provided as an argument after its key. Immutable
hash tables can be extended with @racket[hash-set], which produces a
new immutable hash table in constant time.}
@racket[hash]、@racket[hasheqv]和@racket[hasheq]函数从键和值的一个初始设置创建不可变哈希表，其中每个值作为它键后边的一个参数提供。不可变哈希表可用@racket[hash-set]扩展，它在恒定时间里产生一个新的不可变哈希表。

@examples[
(define ht (hash "apple" 'red "banana" 'yellow))
(hash-ref ht "apple")
(define ht2 (hash-set ht "coconut" 'brown))
(hash-ref ht "coconut")
(hash-ref ht2 "coconut")
]

@;{A literal immutable hash table can be written as an expression by using
@litchar{#hash} (for an @racket[equal?]-based table),
@litchar{#hasheqv} (for an @racket[eqv?]-based table), or
@litchar{#hasheq} (for an @racket[eq?]-based table). A parenthesized
sequence must immediately follow @litchar{#hash}, @litchar{#hasheq},
or @litchar{#hasheqv}, where each element is a dotted
key--value pair. The @litchar{#hash}, etc. forms implicitly
@racket[quote] their key and value sub-forms.}
一个原义的不可变哈希表可以通过使用@litchar{#hash}（对基于@racket[equal?]的表）、@litchar{#hasheqv}（对基于@racket[eqv?]的表）或@litchar{#hasheq}（对基于@racket[eq?]的表）编写为一个表达式。一个带括号的序列必须紧跟着@litchar{#hash}、@litchar{#hasheq}或@litchar{#hasheqv}，其中每个元素是一个带点的键–值对。这个@litchar{#hash}等等这些表都隐含的@racket[quote]它们的键和值的子表。

@examples[
(define ht #hash(("apple" . red)
                 ("banana" . yellow)))
(hash-ref ht "apple")
]

@;{@refdetails/gory["parse-hashtable"]{the syntax of hash table literals}}
@margin-note{在《Racket参考》的“读取哈希表（Reading Hash Tables）”文档有关于哈希表原义的语法更好的知识点。}

@;{Both mutable and immutable hash tables print like immutable hash
tables, using a quoted @litchar{#hash}, @litchar{#hasheqv}, or
@litchar{#hasheq} form if all keys and values can be expressed with
@racket[quote] or using @racketresult[hash], @racketresult[hasheq], or
@racketresult[hasheqv] otherwise:}
可变和不可变的哈希表都像不可变哈希表一样打印，否则如果所有的键和值可以用@racket[quote]表示或者使用@racketresult[hash]、@racketresult[hasheq]或@racketresult[hasheqv]，那么使用一个带引用的@litchar{#hash}、@litchar{#hasheqv}或@litchar{#hasheq}表。

@examples[
#hash(("apple" . red)
      ("banana" . yellow))
(hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
]

@;{A mutable hash table can optionally retain its keys
@defterm{weakly}, so each mapping is retained only so long as the key
is retained elsewhere.}
一个可变哈希表可以选择性地@defterm{弱方式（weakly）}保留其键，因此仅仅只要在其它地方保留键，每个映射都被保留。

@examples[
(define ht (make-weak-hasheq))
(hash-set! ht (gensym) "can you see me?")
(collect-garbage)
(eval:alts (hash-count ht) 0)
]

@;{Beware that even a weak hash table retains its values strongly, as
long as the corresponding key is accessible. This creates a catch-22
dependency when a value refers back to its key, so that the mapping is
retained permanently. To break the cycle, map the key to an @defterm{ephemeron}
that pairs the value with its key (in addition to the implicit pairing
of the hash table).}
请注意，只要对应的键是可访问的，即使是一个弱哈希表也会强健地保留它的值。当一个值指回到它的键，就造成了一个两难的依赖，以致这个映射永久被保留。要打破这个循环，映射键到一个@defterm{暂存值（ephemeron）}，它用它的键（除这个哈希表的隐性配对之外）配对值。

@;{@refdetails/gory["ephemerons"]{using ephemerons}}
@margin-note{在《Racket参考》中的“星历（ephemerons）”文档有关于使用ephemerons更好的知识点。}

@examples[
(define ht (make-weak-hasheq))
(let ([g (gensym)])
  (hash-set! ht g (list g)))
(collect-garbage)
(eval:alts (hash-count ht) 1)
]

@interaction[
(define ht (make-weak-hasheq))
(let ([g (gensym)])
  (hash-set! ht g (make-ephemeron g (list g))))
(collect-garbage)
(eval:alts (hash-count ht) 0)
]

@;{@refdetails["hashtables"]{hash tables and hash-table procedures}}
@margin-note{在《Racket参考》中的“哈希表（Hash Tables）”会提供关于哈希表和哈希表过程更多的信息。}
