#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum
                     racket/unsafe/ops
                     racket/performance-hint))

@;{@title[#:tag "parallelism"]{Parallelism}}
@title[#:tag "parallelism"]{并行}

@;{Racket provides two forms of @deftech{parallelism}: @tech{futures} and
@tech{places}. On a platform that provides multiple processors,
parallelism can improve the run-time performance of a program.}
Racket提供两种形式的@deftech{并行（parallelism）}：@tech{前景（futures）}和@tech{现场（places）}。在提供多个处理器的平台上，并行可以提高一个程序的运行时性能。

@;{See also @secref["performance"] for information on sequential
performance in Racket. Racket also provides threads for
@tech{concurrency}, but threads do not provide parallelism; see
@secref["concurrency"] for more information.}
关于Racket里连续性能的信息又见@secref["performance"]。Racket还提供了对@tech{并发（concurrency）}的线程，但线程没有提供并行；更多的信息见@secref["concurrency"]。

@include-section["futures.scrbl"]
@include-section["places.scrbl"]
@include-section["distributed.scrbl"]
