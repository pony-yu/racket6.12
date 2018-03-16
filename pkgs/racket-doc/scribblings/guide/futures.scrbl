#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/flonum racket/future future-visualizer))

@(define future-eval (make-base-eval))
@(interaction-eval #:eval future-eval (require racket/future 
                                               future-visualizer/private/visualizer-drawing 
                                               future-visualizer/trace))

@;{@title[#:tag "effective-futures"]{Parallelism with Futures}}
@title[#:tag "effective-futures"]{前景并行}

@;{The @racketmodname[racket/future] library provides support for
performance improvement through parallelism with @deftech{futures} and the @racket[future]
and @racket[touch] functions. The level of parallelism available from
those constructs, however, is limited by several factors, and the
current implementation is best suited to numerical tasks. The caveats
in @secref["DrRacket-perf"] also apply to futures; notably,
the debugging instrumentation currently defeats futures.}
@racketmodname[racket/future]库通过与@deftech{前景（futures）}以及@racket[future]和@racket[touch]函数的并行，为性能改进提供支持。然而，这些结构的并行性受到几个因素的限制，当前的实现最适合于数值任务。在@secref["DrRacket-perf"]中的警告也适用于前景；值得注意的是，调试手段目前使前景失效了。

@;{@margin-note{Other functions, such as @racket[thread], support the
creation of reliably concurrent tasks. However, threads never run truly
in parallel, even if the hardware and operating system support
parallelism.}}
@margin-note{其它函数，如@racket[thread]，支持创建可靠的并发任务。然而，即使硬件和操作系统支持并行性，线程也不会真正并行运行。}

@;{As a starting example, the @racket[any-double?] function below takes a
list of numbers and determines whether any number in the list has a
double that is also in the list:}
作为一个开始的例子，@racket[any-double?]函数获取一个数字列表，并确定列表中的任何数字有一个也包含在列表中的double：

@racketblock[
(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))
]

@;{This function runs in quadratic time, so it can take a long time (on
the order of a second) on large lists like @racket[l1] and
@racket[l2]:}
这个函数在二次时间中运行，所以像@racket[l1]和@racket[l2]这样的大列表可能需要很长时间（按秒顺序）：

@racketblock[
(define l1 (for/list ([i (in-range 5000)]) 
             (+ (* 2 i) 1)))
(define l2 (for/list ([i (in-range 5000)]) 
             (- (* 2 i) 1)))
(or (any-double? l1)
    (any-double? l2))
]

@;{The best way to speed up @racket[any-double?]  is to use a different
algorithm. However, on a machine that offers at least two processing
units, the example above can run in about half the time using
@racket[future] and @racket[touch]:}
加速@racket[any-double?]的最好的办法是使用不同的算法。然而，在提供至少两个处理单元的机器上，上述示例可以使用@racket[future]和@racket[touch]的大约一半时间运行：

@racketblock[
(let ([f (future (lambda () (any-double? l2)))])
  (or (any-double? l1)
      (touch f)))
]

@;{The future @racket[f] runs @racket[(any-double? l2)] in parallel to
@racket[(any-double? l1)], and the result for @racket[(any-double?
l2)] becomes available about the same time that it is demanded by
@racket[(touch f)].}
前景@racket[f]在与@racket[(any-double? l1)]平行中运行@racket[(any-double? l2)]，同时对@racket[(any-double? l2)]的结果与@racket[(touch f)]所要求的时间相同。

@;{Futures run in parallel as long as they can do so safely, but the
notion of ``future safe'' is inherently tied to the
implementation. The distinction between ``future safe'' and ``future unsafe''
operations may be far from apparent at the level of a Racket program.
The remainder of this section works through an example to illustrate
this distinction and to show how to use the future visualizer
can help shed light on it.}
只要他们能安全地做到这一点，前景就可以并行运行，但“前景安全”的概念实际上与实施有关。“前景安全”和“前景不安全”操作之间的区别在Racket程序级别上可能还不太明显。本节剩余部分通过一个例子来说明这种区别，并显示如何使用前景的可视化工具有助于阐明这一点。

@;{Consider the following core of a Mandelbrot-set computation:}
考虑一下曼德尔布罗特集合计算的以下核心：

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
              [(> (+ zrq ziq) 4) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2 zr zi) ci))]))))))
]

@;{The expressions @racket[(mandelbrot 10000000 62 500 1000)] and
@racket[(mandelbrot 10000000 62 501 1000)] each take a while to
produce an answer. Computing them both, of course, takes twice as
long:}
表达式@racket[(mandelbrot 10000000 62 500 1000)]和@racket[(mandelbrot 10000000 62 501 1000)]每次都要花一点时间产生一个答案。当然，计算两者都需要两倍的时间：

@racketblock[
(list (mandelbrot 10000000 62 500 1000)
      (mandelbrot 10000000 62 501 1000))
]

@;{Unfortunately, attempting to run the two computations in parallel with
@racket[future] does not improve performance:}
不幸的是，试图用@racket[future]并行运行两个计算并不能提高性能：

@racketblock[
 (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
   (list (mandelbrot 10000000 62 500 1000)
         (touch f)))
]

@;{To see why, use the @racketmodname[future-visualizer], like this:}
要知道为什么，使用@racketmodname[future-visualizer]，像这样：

@racketblock[ 
  (require future-visualizer) 
  (visualize-futures 
   (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
     (list (mandelbrot 10000000 62 500 1000)
           (touch f))))] 
 
@;{This opens a window showing a graphical view of a trace of the computation.
The upper-left portion of the window contains an execution timeline:}
这将打开一个窗口，显示计算跟踪的图形视图。窗口的左上部分包含一个执行时间线：

@(interaction-eval 
  #:eval future-eval 
  (define bad-log 
    (list (indexed-future-event 0 '#s(future-event #f 0 create 1334778390997.936 #f 1))
          (indexed-future-event 1 '#s(future-event 1 1 start-work 1334778390998.137 #f #f))
          (indexed-future-event 2 '#s(future-event 1 1 sync 1334778390998.145 #f #f))
          (indexed-future-event 3 '#s(future-event 1 0 sync 1334778391001.616 [allocate memory] #f))
          (indexed-future-event 4 '#s(future-event 1 0 result 1334778391001.629 #f #f))
          (indexed-future-event 5 '#s(future-event 1 1 result 1334778391001.643 #f #f))
          (indexed-future-event 6 '#s(future-event 1 1 block 1334778391001.653 #f #f))
          (indexed-future-event 7 '#s(future-event 1 1 suspend 1334778391001.658 #f #f))
          (indexed-future-event 8 '#s(future-event 1 1 end-work 1334778391001.658 #f #f))
          (indexed-future-event 9 '#s(future-event 1 0 block 1334778392134.226 > #f))
          (indexed-future-event 10 '#s(future-event 1 0 result 1334778392134.241 #f #f))
          (indexed-future-event 11 '#s(future-event 1 1 start-work 1334778392134.254 #f #f))
          (indexed-future-event 12 '#s(future-event 1 1 sync 1334778392134.339 #f #f))
          (indexed-future-event 13 '#s(future-event 1 0 sync 1334778392134.375 [allocate memory] #f))
          (indexed-future-event 14 '#s(future-event 1 0 result 1334778392134.38 #f #f))
          (indexed-future-event 15 '#s(future-event 1 1 result 1334778392134.387 #f #f))
          (indexed-future-event 16 '#s(future-event 1 1 block 1334778392134.39 #f #f))
          (indexed-future-event 17 '#s(future-event 1 1 suspend 1334778392134.391 #f #f))
          (indexed-future-event 18 '#s(future-event 1 1 end-work 1334778392134.391 #f #f))
          (indexed-future-event 19 '#s(future-event 1 0 touch-pause 1334778392134.432 #f #f))
          (indexed-future-event 20 '#s(future-event 1 0 touch-resume 1334778392134.433 #f #f))
          (indexed-future-event 21 '#s(future-event 1 0 block 1334778392134.533 * #f))
          (indexed-future-event 22 '#s(future-event 1 0 result 1334778392134.537 #f #f))
          (indexed-future-event 23 '#s(future-event 1 2 start-work 1334778392134.568 #f #f))
          (indexed-future-event 24 '#s(future-event 1 2 sync 1334778392134.57 #f #f))
          (indexed-future-event 25 '#s(future-event 1 0 touch-pause 1334778392134.587 #f #f))
          (indexed-future-event 26 '#s(future-event 1 0 touch-resume 1334778392134.587 #f #f))
          (indexed-future-event 27 '#s(future-event 1 0 block 1334778392134.6 [allocate memory] #f))
          (indexed-future-event 28 '#s(future-event 1 0 result 1334778392134.604 #f #f))
          (indexed-future-event 29 '#s(future-event 1 2 result 1334778392134.627 #f #f))
          (indexed-future-event 30 '#s(future-event 1 2 block 1334778392134.629 #f #f))
          (indexed-future-event 31 '#s(future-event 1 2 suspend 1334778392134.632 #f #f))
          (indexed-future-event 32 '#s(future-event 1 2 end-work 1334778392134.633 #f #f))
          (indexed-future-event 33 '#s(future-event 1 0 touch-pause 1334778392134.64 #f #f))
          (indexed-future-event 34 '#s(future-event 1 0 touch-resume 1334778392134.64 #f #f))
          (indexed-future-event 35 '#s(future-event 1 0 block 1334778392134.663 > #f))
          (indexed-future-event 36 '#s(future-event 1 0 result 1334778392134.666 #f #f))
          (indexed-future-event 37 '#s(future-event 1 1 start-work 1334778392134.673 #f #f))
          (indexed-future-event 38 '#s(future-event 1 1 block 1334778392134.676 #f #f))
          (indexed-future-event 39 '#s(future-event 1 1 suspend 1334778392134.677 #f #f))
          (indexed-future-event 40 '#s(future-event 1 1 end-work 1334778392134.677 #f #f))
          (indexed-future-event 41 '#s(future-event 1 0 touch-pause 1334778392134.704 #f #f))
          (indexed-future-event 42 '#s(future-event 1 0 touch-resume 1334778392134.704 #f #f))
          (indexed-future-event 43 '#s(future-event 1 0 block 1334778392134.727 * #f))
          (indexed-future-event 44 '#s(future-event 1 0 result 1334778392134.73 #f #f))
          (indexed-future-event 45 '#s(future-event 1 2 start-work 1334778392134.737 #f #f))
          (indexed-future-event 46 '#s(future-event 1 2 block 1334778392134.739 #f #f))
          (indexed-future-event 47 '#s(future-event 1 2 suspend 1334778392134.74 #f #f))
          (indexed-future-event 48 '#s(future-event 1 2 end-work 1334778392134.741 #f #f))
          (indexed-future-event 49 '#s(future-event 1 0 touch-pause 1334778392134.767 #f #f))
          (indexed-future-event 50 '#s(future-event 1 0 touch-resume 1334778392134.767 #f #f))
          (indexed-future-event 51 '#s(future-event 1 0 block 1334778392134.79 > #f))
          (indexed-future-event 52 '#s(future-event 1 0 result 1334778392134.793 #f #f))
          (indexed-future-event 53 '#s(future-event 1 1 start-work 1334778392134.799 #f #f))
          (indexed-future-event 54 '#s(future-event 1 1 block 1334778392134.801 #f #f))
          (indexed-future-event 55 '#s(future-event 1 1 suspend 1334778392134.802 #f #f))
          (indexed-future-event 56 '#s(future-event 1 1 end-work 1334778392134.803 #f #f))
          (indexed-future-event 57 '#s(future-event 1 0 touch-pause 1334778392134.832 #f #f))
          (indexed-future-event 58 '#s(future-event 1 0 touch-resume 1334778392134.832 #f #f))
          (indexed-future-event 59 '#s(future-event 1 0 block 1334778392134.854 * #f))
          (indexed-future-event 60 '#s(future-event 1 0 result 1334778392134.858 #f #f))
          (indexed-future-event 61 '#s(future-event 1 2 start-work 1334778392134.864 #f #f))
          (indexed-future-event 62 '#s(future-event 1 2 block 1334778392134.876 #f #f))
          (indexed-future-event 63 '#s(future-event 1 2 suspend 1334778392134.877 #f #f))
          (indexed-future-event 64 '#s(future-event 1 2 end-work 1334778392134.882 #f #f))
          (indexed-future-event 65 '#s(future-event 1 0 touch-pause 1334778392134.918 #f #f))
          (indexed-future-event 66 '#s(future-event 1 0 touch-resume 1334778392134.918 #f #f))
          (indexed-future-event 67 '#s(future-event 1 0 block 1334778392134.94 > #f))
          (indexed-future-event 68 '#s(future-event 1 0 result 1334778392134.943 #f #f))
          (indexed-future-event 69 '#s(future-event 1 1 start-work 1334778392134.949 #f #f))
          (indexed-future-event 70 '#s(future-event 1 1 block 1334778392134.952 #f #f))
          (indexed-future-event 71 '#s(future-event 1 1 suspend 1334778392134.953 #f #f))
          (indexed-future-event 72 '#s(future-event 1 1 end-work 1334778392134.96 #f #f))
          (indexed-future-event 73 '#s(future-event 1 0 touch-pause 1334778392134.991 #f #f))
          (indexed-future-event 74 '#s(future-event 1 0 touch-resume 1334778392134.991 #f #f))
          (indexed-future-event 75 '#s(future-event 1 0 block 1334778392135.013 * #f))
          (indexed-future-event 76 '#s(future-event 1 0 result 1334778392135.016 #f #f))
          (indexed-future-event 77 '#s(future-event 1 2 start-work 1334778392135.027 #f #f))
          (indexed-future-event 78 '#s(future-event 1 2 block 1334778392135.033 #f #f))
          (indexed-future-event 79 '#s(future-event 1 2 suspend 1334778392135.034 #f #f))
          (indexed-future-event 80 '#s(future-event 1 2 end-work 1334778392135.04 #f #f))
          (indexed-future-event 81 '#s(future-event 1 0 touch-pause 1334778392135.075 #f #f))
          (indexed-future-event 82 '#s(future-event 1 0 touch-resume 1334778392135.075 #f #f))
          (indexed-future-event 83 '#s(future-event 1 0 block 1334778392135.098 > #f))
          (indexed-future-event 84 '#s(future-event 1 0 result 1334778392135.101 #f #f))
          (indexed-future-event 85 '#s(future-event 1 1 start-work 1334778392135.107 #f #f))
          (indexed-future-event 86 '#s(future-event 1 1 block 1334778392135.117 #f #f))
          (indexed-future-event 87 '#s(future-event 1 1 suspend 1334778392135.118 #f #f))
          (indexed-future-event 88 '#s(future-event 1 1 end-work 1334778392135.123 #f #f))
          (indexed-future-event 89 '#s(future-event 1 0 touch-pause 1334778392135.159 #f #f))
          (indexed-future-event 90 '#s(future-event 1 0 touch-resume 1334778392135.159 #f #f))
          (indexed-future-event 91 '#s(future-event 1 0 block 1334778392135.181 * #f))
          (indexed-future-event 92 '#s(future-event 1 0 result 1334778392135.184 #f #f))
          (indexed-future-event 93 '#s(future-event 1 2 start-work 1334778392135.19 #f #f))
          (indexed-future-event 94 '#s(future-event 1 2 block 1334778392135.191 #f #f))
          (indexed-future-event 95 '#s(future-event 1 2 suspend 1334778392135.192 #f #f))
          (indexed-future-event 96 '#s(future-event 1 2 end-work 1334778392135.192 #f #f))
          (indexed-future-event 97 '#s(future-event 1 0 touch-pause 1334778392135.221 #f #f))
          (indexed-future-event 98 '#s(future-event 1 0 touch-resume 1334778392135.221 #f #f))
          (indexed-future-event 99 '#s(future-event 1 0 block 1334778392135.243 > #f))
          )))
                   
@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict bad-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300) 
]

@;{Each horizontal row represents an OS-level thread, and the colored 
dots represent important events in the execution of the program (they are 
color-coded to distinguish one event type from another).  The upper-left blue 
dot in the timeline represents the future's creation.  The future 
executes for a brief period (represented by a green bar in the second line) on thread 
1, and then pauses to allow the runtime thread to perform a future-unsafe operation.}
每个水平行代表一个操作系统级线程，着色点代表程序执行中的重要事件（它们被颜色编码以区分一个事件类型与另一个事件）。时间轴的上左位置蓝色圆点代表未来的创造。前景在线程1上执行一个短暂的时期（由第二行中的绿色条表示），然后暂停以允许运行时线程执行前景不安全操作。

@;{In the Racket implementation, future-unsafe operations fall into one of two categories. 
A @deftech{blocking} operation halts the evaluation of the future, and will not allow 
it to continue until it is touched.  After the operation completes within @racket[touch], 
the remainder of the future's work will be evaluated sequentially by the runtime 
thread.  A @deftech{synchronized} operation also halts the future, but the runtime thread 
may perform the operation at any time and, once completed, the future may continue 
running in parallel.  Memory allocation and JIT compilation are two common examples 
of synchronized operations.}
在Racket的实现中，前景不安全操作分为两类。一个@deftech{阻塞（blocking）}操作中止前景求值，同时不允许它继续下去，直到它被接触（touched）。在@racket[touch]中的操作完成之后，前景工作的剩余部分将由运行时线程依次进行求值。一个@deftech{同步（synchronized）}操作也中止前景，但运行时线程可以在任何时间执行操作，一旦完成，前景可能在并行中继续运行。内存分配和JIT编译是同步操作的两个常见示例。

@;{In the timeline, we see an orange dot just to the right of the green bar on thread 1 -- 
this dot represents a synchronized operation (memory allocation).  The first orange 
dot on thread 0 shows that the runtime thread performed the allocation shortly after 
the future paused.  A short time later, the future halts on a blocking operation 
(the first red dot) and must wait until the @racket[touch] for it to be evaluated 
(slightly after the 1049ms mark).}
在时间线中，我们在线程1的绿色条的右边看到一个橙色点——这个点代表一个同步操作（内存分配）。线程0上的第一个橙色圆点表示运行时线程在将来暂停后很快执行分配。不久之后，在一个阻塞操作前景中止（第一个红点），并且必须等到它被求值的@racket[touch]（略后1049ms标记）。

@;{When you move your mouse over an event, the visualizer shows you 
detailed information about the event and draws arrows
connecting all of the events in the corresponding future.
This image shows those connections for our future.}
当你把鼠标移动到一个事件，可视化工具显示你的有关事件和画箭头连接在相应的前景事件的详细信息。这张图片显示了对我们的未来的联系。

@interaction-eval-show[
     #:eval future-eval 
            (timeline-pict bad-log 
                           #:x 0
                           #:y 0 
                           #:width 600 
                           #:height 300 
                           #:selected-event-index 6)
]

@;{The dotted orange line connects the first event in the future to
the future that created it, and the purple lines connect adjacent
events within the future. }
虚线橙色线连接前景中的第一个事件到创造它的前景，同时紫色线连接前景里的邻近事件。

@;{The reason that we see no parallelism is that the @racket[<] and @racket[*] operations 
in the lower portion of the loop in @racket[mandelbrot] involve a mixture of 
floating-point and fixed (integer) values.  Such mixtures typically trigger a slow 
path in execution, and the general slow path will usually be blocking.}
我们没有看到并行性的原因是，@racket[mandelbrot]中的循环的下一部分中的@racket[<]和@racket[*]操作包括一个浮点值和固定（整数）值的混合。这种混合通常触发一个在执行过程中慢路径，并且这个普通的慢路径通常会阻塞。

@;{Changing constants to be floating-points numbers in @racket[mandelbrot] addresses that 
first problem: }
将常数变为第一个问题的@racket[mandelbrot]地址中的浮点数：

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]
        [cr (- (/ (* 2.0 x) n) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (* zr zr)]
                [ziq (* zi zi)])
            (cond
              [(> (+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (+ (- zrq ziq) cr)
                          (+ (* 2.0 zr zi) ci))]))))))
]

@;{With that change, @racket[mandelbrot] computations can run in 
parallel.  Nevertheless, we still see a special type of 
slow-path operation limiting our parallelism (orange dots):}
随着这种变化，@racket[mandelbrot]计算可以并行运行。然而，我们仍然看到一种特殊的慢路径操作限制了我们的并行性（橙色点）：

@interaction-eval[
    #:eval future-eval            
    (define better-log 
      (list (indexed-future-event 0 '#s(future-event #f 0 create 1334779296782.22 #f 2))
            (indexed-future-event 1 '#s(future-event 2 2 start-work 1334779296782.265 #f #f))
            (indexed-future-event 2 '#s(future-event 2 2 sync 1334779296782.378 #f #f))
            (indexed-future-event 3 '#s(future-event 2 0 sync 1334779296795.582 [allocate memory] #f))
            (indexed-future-event 4 '#s(future-event 2 0 result 1334779296795.587 #f #f))
            (indexed-future-event 5 '#s(future-event 2 2 result 1334779296795.6 #f #f))
            (indexed-future-event 6 '#s(future-event 2 2 sync 1334779296795.689 #f #f))
            (indexed-future-event 7 '#s(future-event 2 0 sync 1334779296795.807 [allocate memory] #f))
            (indexed-future-event 8 '#s(future-event 2 0 result 1334779296795.812 #f #f))
            (indexed-future-event 9 '#s(future-event 2 2 result 1334779296795.818 #f #f))
            (indexed-future-event 10 '#s(future-event 2 2 sync 1334779296795.827 #f #f))
            (indexed-future-event 11 '#s(future-event 2 0 sync 1334779296806.627 [allocate memory] #f))
            (indexed-future-event 12 '#s(future-event 2 0 result 1334779296806.635 #f #f))
            (indexed-future-event 13 '#s(future-event 2 2 result 1334779296806.646 #f #f))
            (indexed-future-event 14 '#s(future-event 2 2 sync 1334779296806.879 #f #f))
            (indexed-future-event 15 '#s(future-event 2 0 sync 1334779296806.994 [allocate memory] #f))
            (indexed-future-event 16 '#s(future-event 2 0 result 1334779296806.999 #f #f))
            (indexed-future-event 17 '#s(future-event 2 2 result 1334779296807.007 #f #f))
            (indexed-future-event 18 '#s(future-event 2 2 sync 1334779296807.023 #f #f))
            (indexed-future-event 19 '#s(future-event 2 0 sync 1334779296814.198 [allocate memory] #f))
            (indexed-future-event 20 '#s(future-event 2 0 result 1334779296814.206 #f #f))
            (indexed-future-event 21 '#s(future-event 2 2 result 1334779296814.221 #f #f))
            (indexed-future-event 22 '#s(future-event 2 2 sync 1334779296814.29 #f #f))
            (indexed-future-event 23 '#s(future-event 2 0 sync 1334779296820.796 [allocate memory] #f))
            (indexed-future-event 24 '#s(future-event 2 0 result 1334779296820.81 #f #f))
            (indexed-future-event 25 '#s(future-event 2 2 result 1334779296820.835 #f #f))
            (indexed-future-event 26 '#s(future-event 2 2 sync 1334779296821.089 #f #f))
            (indexed-future-event 27 '#s(future-event 2 0 sync 1334779296825.217 [allocate memory] #f))
            (indexed-future-event 28 '#s(future-event 2 0 result 1334779296825.226 #f #f))
            (indexed-future-event 29 '#s(future-event 2 2 result 1334779296825.242 #f #f))
            (indexed-future-event 30 '#s(future-event 2 2 sync 1334779296825.305 #f #f))
            (indexed-future-event 31 '#s(future-event 2 0 sync 1334779296832.541 [allocate memory] #f))
            (indexed-future-event 32 '#s(future-event 2 0 result 1334779296832.549 #f #f))
            (indexed-future-event 33 '#s(future-event 2 2 result 1334779296832.562 #f #f))
            (indexed-future-event 34 '#s(future-event 2 2 sync 1334779296832.667 #f #f))
            (indexed-future-event 35 '#s(future-event 2 0 sync 1334779296836.269 [allocate memory] #f))
            (indexed-future-event 36 '#s(future-event 2 0 result 1334779296836.278 #f #f))
            (indexed-future-event 37 '#s(future-event 2 2 result 1334779296836.326 #f #f))
            (indexed-future-event 38 '#s(future-event 2 2 sync 1334779296836.396 #f #f))
            (indexed-future-event 39 '#s(future-event 2 0 sync 1334779296843.481 [allocate memory] #f))
            (indexed-future-event 40 '#s(future-event 2 0 result 1334779296843.49 #f #f))
            (indexed-future-event 41 '#s(future-event 2 2 result 1334779296843.501 #f #f))
            (indexed-future-event 42 '#s(future-event 2 2 sync 1334779296843.807 #f #f))
            (indexed-future-event 43 '#s(future-event 2 0 sync 1334779296847.291 [allocate memory] #f))
            (indexed-future-event 44 '#s(future-event 2 0 result 1334779296847.3 #f #f))
            (indexed-future-event 45 '#s(future-event 2 2 result 1334779296847.312 #f #f))
            (indexed-future-event 46 '#s(future-event 2 2 sync 1334779296847.375 #f #f))
            (indexed-future-event 47 '#s(future-event 2 0 sync 1334779296854.487 [allocate memory] #f))
            (indexed-future-event 48 '#s(future-event 2 0 result 1334779296854.495 #f #f))
            (indexed-future-event 49 '#s(future-event 2 2 result 1334779296854.507 #f #f))
            (indexed-future-event 50 '#s(future-event 2 2 sync 1334779296854.656 #f #f))
            (indexed-future-event 51 '#s(future-event 2 0 sync 1334779296857.374 [allocate memory] #f))
            (indexed-future-event 52 '#s(future-event 2 0 result 1334779296857.383 #f #f))
            (indexed-future-event 53 '#s(future-event 2 2 result 1334779296857.421 #f #f))
            (indexed-future-event 54 '#s(future-event 2 2 sync 1334779296857.488 #f #f))
            (indexed-future-event 55 '#s(future-event 2 0 sync 1334779296869.919 [allocate memory] #f))
            (indexed-future-event 56 '#s(future-event 2 0 result 1334779296869.947 #f #f))
            (indexed-future-event 57 '#s(future-event 2 2 result 1334779296869.981 #f #f))
            (indexed-future-event 58 '#s(future-event 2 2 sync 1334779296870.32 #f #f))
            (indexed-future-event 59 '#s(future-event 2 0 sync 1334779296879.438 [allocate memory] #f))
            (indexed-future-event 60 '#s(future-event 2 0 result 1334779296879.446 #f #f))
            (indexed-future-event 61 '#s(future-event 2 2 result 1334779296879.463 #f #f))
            (indexed-future-event 62 '#s(future-event 2 2 sync 1334779296879.526 #f #f))
            (indexed-future-event 63 '#s(future-event 2 0 sync 1334779296882.928 [allocate memory] #f))
            (indexed-future-event 64 '#s(future-event 2 0 result 1334779296882.935 #f #f))
            (indexed-future-event 65 '#s(future-event 2 2 result 1334779296882.944 #f #f))
            (indexed-future-event 66 '#s(future-event 2 2 sync 1334779296883.311 #f #f))
            (indexed-future-event 67 '#s(future-event 2 0 sync 1334779296890.471 [allocate memory] #f))
            (indexed-future-event 68 '#s(future-event 2 0 result 1334779296890.479 #f #f))
            (indexed-future-event 69 '#s(future-event 2 2 result 1334779296890.517 #f #f))
            (indexed-future-event 70 '#s(future-event 2 2 sync 1334779296890.581 #f #f))
            (indexed-future-event 71 '#s(future-event 2 0 sync 1334779296894.362 [allocate memory] #f))
            (indexed-future-event 72 '#s(future-event 2 0 result 1334779296894.369 #f #f))
            (indexed-future-event 73 '#s(future-event 2 2 result 1334779296894.382 #f #f))
            (indexed-future-event 74 '#s(future-event 2 2 sync 1334779296894.769 #f #f))
            (indexed-future-event 75 '#s(future-event 2 0 sync 1334779296901.501 [allocate memory] #f))
            (indexed-future-event 76 '#s(future-event 2 0 result 1334779296901.51 #f #f))
            (indexed-future-event 77 '#s(future-event 2 2 result 1334779296901.556 #f #f))
            (indexed-future-event 78 '#s(future-event 2 2 sync 1334779296901.62 #f #f))
            (indexed-future-event 79 '#s(future-event 2 0 sync 1334779296905.428 [allocate memory] #f))
            (indexed-future-event 80 '#s(future-event 2 0 result 1334779296905.434 #f #f))
            (indexed-future-event 81 '#s(future-event 2 2 result 1334779296905.447 #f #f))
            (indexed-future-event 82 '#s(future-event 2 2 sync 1334779296905.743 #f #f))
            (indexed-future-event 83 '#s(future-event 2 0 sync 1334779296912.538 [allocate memory] #f))
            (indexed-future-event 84 '#s(future-event 2 0 result 1334779296912.547 #f #f))
            (indexed-future-event 85 '#s(future-event 2 2 result 1334779296912.564 #f #f))
            (indexed-future-event 86 '#s(future-event 2 2 sync 1334779296912.625 #f #f))
            (indexed-future-event 87 '#s(future-event 2 0 sync 1334779296916.094 [allocate memory] #f))
            (indexed-future-event 88 '#s(future-event 2 0 result 1334779296916.1 #f #f))
            (indexed-future-event 89 '#s(future-event 2 2 result 1334779296916.108 #f #f))
            (indexed-future-event 90 '#s(future-event 2 2 sync 1334779296916.243 #f #f))
            (indexed-future-event 91 '#s(future-event 2 0 sync 1334779296927.233 [allocate memory] #f))
            (indexed-future-event 92 '#s(future-event 2 0 result 1334779296927.242 #f #f))
            (indexed-future-event 93 '#s(future-event 2 2 result 1334779296927.262 #f #f))
            (indexed-future-event 94 '#s(future-event 2 2 sync 1334779296927.59 #f #f))
            (indexed-future-event 95 '#s(future-event 2 0 sync 1334779296934.603 [allocate memory] #f))
            (indexed-future-event 96 '#s(future-event 2 0 result 1334779296934.612 #f #f))
            (indexed-future-event 97 '#s(future-event 2 2 result 1334779296934.655 #f #f))
            (indexed-future-event 98 '#s(future-event 2 2 sync 1334779296934.72 #f #f))
            (indexed-future-event 99 '#s(future-event 2 0 sync 1334779296938.773 [allocate memory] #f))
            ))
]

@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict better-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300)
]

@;{The problem is that most every arithmetic operation in this example 
produces an inexact number whose storage must be allocated.  While some allocation 
can safely be performed exclusively without the aid of the runtime thread, especially 
frequent allocation requires synchronized operations which defeat any performance 
improvement.}
问题是，这个例子中的大多数算术运算都会产生一个不精确的数字，它的存储必须被分配。虽然有些配置可以安全地只在没有运行时线程的情况下安全地执行，特别是频繁分配需要同步操作来克服任何性能改进。

@;{By using @tech{flonum}-specific operations (see
@secref["fixnums+flonums"]), we can re-write @racket[mandelbrot] to use
much less allocation:}
利用@tech{flonum}具体操作（见@secref["fixnums+flonums"]），我们可以重写@racket[mandelbrot]以达到用更少的配置：

@interaction-eval[
    #:eval future-eval 
    (define good-log 
      (list (indexed-future-event 0 '#s(future-event #f 0 create 1334778395768.733 #f 3))
            (indexed-future-event 1 '#s(future-event 3 2 start-work 1334778395768.771 #f #f))
            (indexed-future-event 2 '#s(future-event 3 2 complete 1334778395864.648 #f #f))
            (indexed-future-event 3 '#s(future-event 3 2 end-work 1334778395864.652 #f #f))
            ))
]

@racketblock[
(define (mandelbrot iterations x y n)
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (fl* zr zr)]
                [ziq (fl* zi zi)])
            (cond
              [(fl> (fl+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (fl+ (fl- zrq ziq) cr)
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))
]

@;{This conversion can speed @racket[mandelbrot] by a factor of 8, even
in sequential mode, but avoiding allocation also allows
@racket[mandelbrot] to run usefully faster in parallel.
Executing this program yields the following in the visualizer: }
即使是在连续模式下，这种转换可以将@racket[mandelbrot]速度提高8倍，但避免分配也允许@racket[mandelbrot]在并行中更快地运行。执行这个程序产生下面的可视化工具：

@interaction-eval-show[
    #:eval future-eval 
           (timeline-pict good-log 
                          #:x 0 
                          #:y 0 
                          #:width 600 
                          #:height 300)
]

@;{Notice that only one green bar is shown here because one of the 
mandelbrot computations is not being evaluated by a future (on 
the runtime thread).}
注意，这里只显示一个绿色条，因为曼德尔布罗特计算中没有一个是由一个前景（运行时线程）求值的。

@;{As a general guideline, any operation that is inlined by the
@tech{JIT} compiler runs safely in parallel, while other operations
that are not inlined (including all operations if the JIT compiler is
disabled) are considered unsafe. The @exec{raco decompile} tool
annotates operations that can be inlined by the compiler (see
@secref[#:doc '(lib "scribblings/raco/raco.scrbl") "decompile"]), so the
decompiler can be used to help predict parallel performance.}
作为一个通用准则，在并行中通过@tech{JIT}编译器内联安全运行的任何操作，当没有内联（包括所有的操作如果JIT编译器是非激活的）的其它操作被认为是不安全的。@exec{raco反编译（raco decompile）}工具对操作可以被反编译器内联编译（见@secref[#:doc '(lib "scribblings/raco/raco.scrbl") "decompile"]），所以反编译器可以用来帮助预测并行性能。

@close-eval[future-eval]
