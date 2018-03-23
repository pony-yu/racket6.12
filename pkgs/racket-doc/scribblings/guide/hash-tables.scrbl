#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@;{@title[#:tag "hash-tables"]{Hash Tables}}
@title[#:tag "hash-tables"]{哈希表（Hash Table）}

@;{A @deftech{hash table} implements a mapping from keys to values, where
both keys and values can be arbitrary Racket values, and access and
update to the table are normally constant-time operations. Keys are
compared using @racket[equal?], @racket[eqv?], or @racket[eq?], depending on whether
the hash table is created with @racket[make-hash],
@racket[make-hasheqv], or @racket[make-hasheq].}
一个@deftech{哈希表（hash table）}实现了从键到值的映射，其中键和值可以是任意的Racket值，而对表的访问和更新通常是常量时间操作。键的比较用@racket[equal?]、@racket[eqv?]或@racket[eq?]，取决于哈希表的键创建方式为@racket[make-hash]、@racket[make-hasheqv]或@racket[make-hasheq]。

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
@racket[hash]、@racket[hasheqv]和@racket[hasheq]函数创建不可变的哈希表的键和值的初始设置，其中每个值在键后提供一个参数。不可变的哈希表可通过@racket[hash-set]扩展，在恒定的时间里产生一个新的不可变的哈希表。

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
一个原意的不可变哈希表可以写为一个表达式，使用@litchar{#hash}（以@racket[equal?]为基础的表）、@litchar{#hasheqv}（以@racket[eqv?]为基础的表）或@litchar{#hasheq}（以@racket[eq?]为基础的表）。一个括号序列必须紧跟@litchar{#hash}、@litchar{#hasheq}或@litchar{#hasheqv}，其中每个元素是一个点的键–值对。这个@litchar{#hash}等其它表都暗含@racket[quote]它们的键和值的子表。

@examples[
(define ht #hash(("apple" . red)
                 ("banana" . yellow)))
(hash-ref ht "apple")
]

@;{@refdetails/gory["parse-hashtable"]{the syntax of hash table literals}}
@refdetails/gory["parse-hashtable"]{哈希表的字面语法}

@;{Both mutable and immutable hash tables print like immutable hash
tables, using a quoted @litchar{#hash}, @litchar{#hasheqv}, or
@litchar{#hasheq} form if all keys and values can be expressed with
@racket[quote] or using @racketresult[hash], @racketresult[hasheq], or
@racketresult[hasheqv] otherwise:}
可变和不可变的哈希表都像不可变的哈希表一样打印，如果所有的键和值可以通过引用或使用别的@litchar{#hash}、@litchar{#hasheqv}或@litchar{#hasheq}，那么使用一个被引用的@racketresult[hash]、@racketresult[hasheq]或@racketresult[hasheqv]表：

@examples[
#hash(("apple" . red)
      ("banana" . yellow))
(hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
]

@;{A mutable hash table can optionally retain its keys
@defterm{weakly}, so each mapping is retained only so long as the key
is retained elsewhere.}
一个可变哈希表可以选择性地@defterm{弱方式（weakly）}保留其键，因此只要保留在其它地方的键，每个映射都被保留。

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
请注意，即使是弱哈希表，只要对应的键是可访问的，它的值也很强健。当一个值指回到它的键，就造成了一个两难的依赖，以致这个映射永久保持。要打破这个循环，映射一个键到一个@defterm{暂存值（ephemeron）}，配对它的键和值（除这个隐配对的哈希表之外）。

@;{@refdetails/gory["ephemerons"]{using ephemerons}}
@refdetails/gory["ephemerons"]{使用ephemerons}

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
@refdetails["hashtables"]{哈希表和哈希表程序}
