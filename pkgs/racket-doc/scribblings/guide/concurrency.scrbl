#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.rkt"
          (for-label racket))

@(define concurrency-eval (make-base-eval))

@(define reference-doc '(lib "scribblings/reference/reference.scrbl"))

@;{@title[#:tag "concurrency"]{Concurrency and Synchronization}}
@title[#:tag "concurrency"]{并发与同步}

@;{Racket provides @deftech{concurrency} in the form of
@deftech{threads}, and it provides a general @racket[sync] function
that can be used to synchronize both threads and other implicit forms of
concurrency, such as @tech{ports}.}
Racket以@deftech{线程（threads）}的形式提供@deftech{并发性（concurrency）}，
它提供了一个通用的@racket[sync]（同步）函数，可用于同步线程和其它隐式并发，如@tech{端口（ports）}。

@;{Threads run concurrently in the sense that one thread can preempt
another without its cooperation, but threads do not run in parallel in
the sense of using multiple hardware processors.  See
@secref["parallelism"] for information on parallelism in Racket.}
在一个线程能够不需要协作地抢占另一个这个意义上讲线程同时运行，但是在使用多硬件处理器这个意义上讲线程不并行运行。
参见@secref["parallelism"]以获取Racket的并行信息。

@;{@section{Threads}}
@section[#:tag "Threads"]{线程（thread）}

@;{To execute a procedure concurrently, use @racket[thread].  The
following example creates two new threads from the main thread:}
要同时执行一个过程，使用@racket[thread]（线程）。下面的示例从主线程创建两个新线程：

@racketblock[
(displayln "This is the original thread")
(thread (lambda () (displayln "This is a new thread.")))
(thread (lambda () (displayln "This is another new thread.")))
]

@;{The next example creates a new thread that would otherwise loop forever, but
the main thread uses @racket[sleep] to pause itself for 2.5 seconds, then
uses @racket[kill-thread] to terminate the worker thread:}
下一个示例创建了一个本来会无限循环的新线程，但主线程使用@racket[sleep]使自身暂停2.5秒，然后使用@racket[kill-thread]终止工作线程：

@racketblock[
(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))
(sleep 2.5)
(kill-thread worker)
]

@;{@margin-note{In DrRacket, the main thread keeps going until the Stop button is
clicked, so in DrRacket the @racket[thread-wait] is not necessary.}}
@margin-note{在DrRacket里，主线程一直运行直到按下Stop（停止）按钮，所以在DrRacket里@racket[thread-wait]（线程等待）是没有必要的。}

@;{If the main thread finishes or is killed, the application exits, even if
other threads are still running.  A thread can use @racket[thread-wait] to
wait for another thread to finish.  Here, the main thread uses
@racket[thread-wait] to make sure the worker thread finishes before the main
thread exits:}
一旦主线程结束或被杀死，应用程序将退出，即使其它线程仍在运行。一个线程可以使用@racket[thread-wait]（线程等待）来等待另一个线程完成。
在这里，主线程使用@racket[thread-wait]（线程等待）以确保工作线程在主线程退出之前结束：

@racketblock[
(define worker (thread
                 (lambda ()
                   (for ([i 100])
                     (printf "Working hard... ~a~n" i)))))
(thread-wait worker)
(displayln "Worker finished")
]

@;{@section{Thread Mailboxes}}
@section[#:tag "Thread_Mailboxes"]{线程邮箱}

@;{Each thread has a mailbox for receiving messages.  The @racket[thread-send] function
asynchronously sends a message to another thread's mailbox, while
@racket[thread-receive] returns the oldest message from the current
thread's mailbox, blocking to wait for a message if necessary.  In the
following example, the main thread sends data to the worker thread to be
processed, then sends a @racket['done] message when there is no more data and
waits for the worker thread to finish.}
每个线程都有一个用于接收消息的邮箱。@racket[thread-send]函数异步发送一个消息到另一个线程的邮箱，
而@racket[thread-receive]返回当前线程邮箱中最早的消息，如果需要的话则阻塞以等待新的消息。
在下面的示例中，主线程将数据发送到工作线程以被处理，然后在没有更多数据并等待工作线程完成时发送一个发送一个@racket['done]消息。

@racketblock[
(define worker-thread (thread
                       (lambda ()
                         (let loop ()
                           (match (thread-receive)
                             [(? number? num)
                              (printf "Processing ~a~n" num)
                              (loop)]
                             ['done
                              (printf "Done~n")])))))
(for ([i 20])
  (thread-send worker-thread i))
(thread-send worker-thread 'done)
(thread-wait worker-thread)
]

@;{In the next example, the main thread delegates work to multiple arithmetic
threads, then waits to receive the results.  The arithmetic threads process work
items then send the results to the main thread.}
在下一个示例中，主线程将工作委托给多个算术线程，然后等待接收结果。算术线程处理工作项，然后将结果发送到主线程。

@racketblock[
(define (make-arithmetic-thread operation)
  (thread (lambda ()
            (let loop ()
              (match (thread-receive)
                [(list oper1 oper2 result-thread)
                 (thread-send result-thread
                              (format "~a + ~a = ~a"
                                      oper1
                                      oper2
                                      (operation oper1 oper2)))
                 (loop)])))))

(define addition-thread (make-arithmetic-thread +))
(define subtraction-thread (make-arithmetic-thread -))

(define worklist '((+ 1 1) (+ 2 2) (- 3 2) (- 4 1)))
(for ([item worklist])
  (match item
    [(list '+ o1 o2)
     (thread-send addition-thread
                  (list o1 o2 (current-thread)))]
    [(list '- o1 o2)
     (thread-send subtraction-thread
                  (list o1 o2 (current-thread)))]))

(for ([i (length worklist)])
  (displayln (thread-receive)))
]

@;{@section{Semaphores}}
@section[#:tag "Semaphores"]{信号（Semaphores）}

@;{Semaphores facilitate synchronized access to an arbitrary shared resource.
Use semaphores when multiple threads must perform non-atomic operations on a
single resource.}
信号可用于帮助对任意共享资源的同步访问。当多个线程必须对单个资源执行非原子操作时，应该使用信号。

@;{In the following example, multiple threads print to standard output
concurrently.  Without synchronization, a line printed by one thread might
appear in the middle of a line printed by another thread.  By using a semaphore
initialized with a count of @racket[1], only one thread will print at a time.
The @racket[semaphore-wait] function blocks until the semaphore's internal counter is
non-zero, then decrements the counter and returns. The @racket[semaphore-post] function
increments the counter so that another thread can unblock and then print.}
在下面的示例中，多个线程同时打印到标准输出。如果没有同步，一个线程打印的行可能出现在另一个线程打印的行的中间。
通过使用计数为@racket[1]初始化的信号，每次只有一个线程能打印。@racket[semaphore-wait]函数将阻塞线程直到信号的内部计数器不为零，
然后递减计数器并返回。@racket[semaphore-post]函数递增计数器以便另一个线程可以解锁然后打印。

@racketblock[
(define output-semaphore (make-semaphore 1))
(define (make-thread name)
  (thread (lambda ()
            (for [(i 10)]
              (semaphore-wait output-semaphore)
              (printf "thread ~a: ~a~n" name i)
              (semaphore-post output-semaphore)))))
(define threads
  (map make-thread '(A B C)))
(for-each thread-wait threads)
]

@;{The pattern of waiting on a semaphore, working, and posting to the
semaphore can also be expressed using
@racket[call-with-semaphore],which has the advantage of posting to the
semaphore if control escapes (e.g., due to an exception):}
等待信号、工作和张贴到信号的模式也可以用@racket[call-with-semaphore]来表示，它的优势是能够张贴到信号即使控制中断（例如由于异常）：

@racketblock[
(define output-semaphore (make-semaphore 1))
(define (make-thread name)
  (thread (lambda ()
            (for [(i 10)]
              (call-with-semaphore
               output-semaphore
               (lambda ()
                (printf "thread ~a: ~a~n" name i)))))))
(define threads
  (map make-thread '(A B C)))
(for-each thread-wait threads)
]

@;{Semaphores are a low-level technique.  Often, a better solution is to restrict
resource access to a single thread.  For example, synchronizing access to
standard output might be better accomplished by having a dedicated thread for
printing output.}
信号是一个底层技术。通常，一个更好的解决方案是限制对资源的访问，使其只能被单个线程访问。
例如，通过为打印输出提供专用线程，可以更好地实现对标准输出的同步访问。

@;{@section{Channels}}
@section[#:tag "Channels"]{通道（Channels）}

@;{Channels synchronize two threads while a value is passed from one thread to the
other.  Unlike a thread mailbox, multiple threads can get items from a single
channel, so channels should be used when multiple threads need to consume items
from a single work queue.}
当一个值从一个线程传递给另一个线程时，通道同步两个线程。与线程邮箱不同，多个线程可以从单个通道获得条目，因此当多个线程需要从单个工作队列中接受条目时，应该使用通道。

@;{In the following example, the main thread adds items to a channel using
@racket[channel-put], while multiple worker threads consume those items using
@racket[channel-get].  Each call to either procedure blocks until another
thread calls the other procedure with the same channel.  The workers process
the items and then pass their results to the result thread via the @racket[result-channel].}
在下面的示例中，主线程使用@racket[channel-put]将条目添加到通道，而多个工作线程使用@racket[channel-get]来接受这些条目。对任一过程的每个调用都会阻塞，直到另一个线程使用相同的通道调用另一个过程。工作进程处理这些条目，然后通过@racket[result-channel]传递它们的结果到结果线程。

@racketblock[
(define result-channel (make-channel))
(define result-thread
        (thread (lambda ()
                  (let loop ()
                    (displayln (channel-get result-channel))
                    (loop)))))

(define work-channel (make-channel))
(define (make-worker thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (channel-get work-channel))
       (case item
         [(DONE)
          (channel-put result-channel
                       (format "Thread ~a done" thread-id))]
         [else
          (channel-put result-channel
                       (format "Thread ~a processed ~a"
                               thread-id
                               item))
          (loop)])))))
(define work-threads (map make-worker '(1 2)))
(for ([item '(A B C D E F G H DONE DONE)])
  (channel-put work-channel item))
(for-each thread-wait work-threads)
]

@;{@section{Buffered Asynchronous Channels}}
@section[#:tag "Buffered_Asynchronous_Channels"]{缓冲异步通道}

@;{Buffered asynchronous channels are similar to the channels described above, but
the ``put'' operation of asynchronous channels does not block---unless the given
channel was created with a buffer limit and the limit has been reached.  The
asynchronous-put operation is therefore somewhat similar to
@racket[thread-send], but unlike thread mailboxes, asynchronous channels allow
multiple threads to consume items from a single channel.}
缓冲异步通道类似于上面描述的通道，但是异步通道的“放置（put）”操作不会阻塞除非给定的通道创建时有缓冲区大小限制，
并且达到了极限。因此，异步放置（asynchronous-put）操作类似于@racket[thread-send]，
但与线程邮箱不同，异步通道允许多个线程从单个通道中接受条目。

@;{In the following
example, the main thread adds items to the work channel, which holds a maximum
of three items at a time.  The worker threads process items from this channel and
then send results to the print thread.}
在下面的示例中，主线程将条目添加到工作通道，该通道每次最多容纳三个条目。工作线程从该通道处理条目，然后将结果发送到打印线程。

@racketblock[
(require racket/async-channel)

(define print-thread
  (thread (lambda ()
            (let loop ()
              (displayln (thread-receive))
              (loop)))))
(define (safer-printf . items)
  (thread-send print-thread
               (apply format items)))

(define work-channel (make-async-channel 3))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (async-channel-get work-channel))
       (safer-printf "Thread ~a processing item: ~a" thread-id item)
       (loop)))))

(for-each make-worker-thread '(1 2 3))
(for ([item '(a b c d e f g h i j k l m)])
  (async-channel-put work-channel item))
]

@;{Note the above example lacks any synchronization to verify that all items were
processed.  If the main thread were to exit without such synchronization, it is
possible that the worker threads will not finish processing some items or the
print thread will not print all items.}
注意，上面的例子缺少任何同步来验证所有的条目都被处理了。如果主线程没有同步地退出，则工作线程可能无法完成某些条目的处理，否则打印线程将不会打印所有条目。

@;{@section{Synchronizable Events and @racket[sync]}}
@section[#:tag "Synchronizable_Events_and_sync"]{同步事件和@racket[sync]}

@;{There are other ways to synchronize threads.  The @racket[sync] function allows
threads to coordinate via @tech[#:doc reference-doc]{synchronizable events}.
Many values double as events, allowing a uniform way to synchronize threads
using different types.  Examples of events include channels, ports, threads,
and alarms.}
还有其他方法来同步线程。@racket[sync]函数允许线程通过同步事件@tech[#:doc reference-doc]{同步事件（synchronizable events）}进行协调。
许多值随事件翻倍，线程得以以一种不同类型的通用方式同步。事件的例子包括通道、端口、线程和警报。

@;{In the next example, a channel and an alarm are used as synchronizable events.
The workers @racket[sync] on both so that they can process channel items until the
alarm is activated.  The channel items are processed, and then results are sent back
to the main thread.}
在下面的例子中，一个通道和一个警报被作为同步事件使用。工作线程@racket[sync]在通道和警报两者上，
以便它们能处理通道条目直到警报被激活.通道条目被处理后，结果将发送回主线程。

@racketblock[
(define main-thread (current-thread))
(define alarm (alarm-evt (+ 3000 (current-inexact-milliseconds))))
(define channel (make-channel))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (define evt (sync channel alarm))
     (cond
       [(equal? evt alarm)
        (thread-send main-thread 'alarm)]
       [else
        (thread-send main-thread
                     (format "Thread ~a received ~a"
                             thread-id
                             evt))]))))
(make-worker-thread 1)
(make-worker-thread 2)
(make-worker-thread 3)
(channel-put channel 'A)
(channel-put channel 'B)
(let loop ()
  (match (thread-receive)
    ['alarm
     (displayln "Done")]
    [result
     (displayln result)
     (loop)]))
]

@;{The next example shows a function for use in a simple TCP echo server.  The
function uses @racket[sync/timeout] to synchronize on input from the given port
or a message in the thread's mailbox.  The first argument to @racket[sync/timeout]
specifies the maximum number of seconds it should wait on the given events. The
@racket[read-line-evt] function returns an event that is ready when a line of input is
available in the given input port.  The result of @racket[thread-receive-evt] is ready when
@racket[thread-receive] would not block.  In a real application, the messages
received in the thread mailbox could be used for control messages, etc.}
下一个示例展示了在简单的TCP回声服务器中使用的函数。该函数使用@racket[sync/timeout]对给定端口的输入或线程邮箱中的消息进行同步。
@racket[sync/timeout]的第一个参数指定在给定事件中应该等待的最大秒数。
@racket[read-line-evt]函数会返回一个事件。当指定输入端口里的一行输入可获得时，这个事件会被准备好。
当调用@racket[thread-receive]不会阻塞时，@racket[thread-receive-evt]的结果会被准备好。
在实际应用中，线程邮箱中接收到的消息可用于控制消息，等等。

@racketblock[
(define (serve in-port out-port)
  (let loop []
    (define evt (sync/timeout 2
                              (read-line-evt in-port 'any)
                              (thread-receive-evt)))
    (cond
      [(not evt)
       (displayln "Timed out, exiting")
       (tcp-abandon-port in-port)
       (tcp-abandon-port out-port)]
      [(string? evt)
       (fprintf out-port "~a~n" evt)
       (flush-output out-port)
       (loop)]
      [else
       (printf "Received a message in mailbox: ~a~n"
               (thread-receive))
       (loop)])))
]

@;{The @racket[serve] function is used in the following example, which
starts a server thread and a client thread that communicate over TCP.  The
client prints three lines to the server, which echoes them back.  The client's
@racket[copy-port] call blocks until EOF is received.  The server times out after
two seconds, closing the ports, which allows @racket[copy-port] to finish and the
client to exit.  The main thread uses @racket[thread-wait] to wait for the
client thread to exit (since, without @racket[thread-wait], the main thread might
exit before the other threads are finished).}
下面的例子中使用了@racket[serve]函数，它启动一个服务端线程和一个通过 TCP 通信的客户端线程。
客户端向服务端输出三行消息，服务端会将它们直接返回。客户端的@racket[copy-port]调用阻塞直到接收到EOF为止。
服务端在两秒钟后超时，关闭端口，使@racket[copy-port]完成，然后客户端退出。
主线程使用@racket[thread-wait]来等待客户端线程退出（主线程因为没有@racket[thread-wait]，它可能在其它线程完成之前退出）。

@racketblock[
(define port-num 4321)
(define (start-server)
  (define listener (tcp-listen port-num))
  (thread
    (lambda ()
      (define-values [in-port out-port] (tcp-accept listener))
      (serve in-port out-port))))

(start-server)

(define client-thread
  (thread
   (lambda ()
     (define-values [in-port out-port] (tcp-connect "localhost" port-num))
     (display "first\nsecond\nthird\n" out-port)
     (flush-output out-port)
     (code:comment "copy-port will block until EOF is read from in-port")
     (copy-port in-port (current-output-port)))))

(thread-wait client-thread)
]

@;{Sometimes, you want to attach result behavior directly to the event passed to
@racket[sync].  In the following example, the worker thread synchronizes on three
channels, but each channel must be handled differently.  Using
@racket[handle-evt] associates a callback with the given event.  When
@racket[sync] selects the given event, it calls the callback to generate the
synchronization result, rather than using the event's normal synchronization
result.  Since the event is handled in the callback, there is no need to
dispatch on the return value of @racket[sync].}
有时，你希望直接将结果行为附加到传递给@racket[sync]的事件。在下面的示例中，工作线程在三个通道上同步，但每个通道必须不同地处理。使用@racket[handle-evt]关联到一个给定的事件回调。当@racket[sync]选择给定事件时，它调用回调来生成同步结果，而不是使用事件的正常同步结果。由于事件是在回调中处理的，所以不需要在@racket[sync]的返回值上进行调度。

@racketblock[
(define add-channel (make-channel))
(define multiply-channel (make-channel))
(define append-channel (make-channel))

(define (work)
  (let loop ()
    (sync (handle-evt add-channel
                      (lambda (list-of-numbers)
                        (printf "Sum of ~a is ~a~n"
                                list-of-numbers
                                (apply + list-of-numbers))))
          (handle-evt multiply-channel
                      (lambda (list-of-numbers)
                        (printf "Product of ~a is ~a~n"
                                list-of-numbers
                                (apply * list-of-numbers))))
          (handle-evt append-channel
                      (lambda (list-of-strings)
                        (printf "Concatenation of ~s is ~s~n"
                                list-of-strings
                                (apply string-append list-of-strings)))))
    (loop)))

(define worker (thread work))
(channel-put add-channel '(1 2))
(channel-put multiply-channel '(3 4))
(channel-put multiply-channel '(5 6))
(channel-put add-channel '(7 8))
(channel-put append-channel '("a" "b"))
]

@;{The result of @racket[handle-evt] invokes its callback in tail position
with respect to @racket[sync], so it is safe to
use recursion as in the following example.}
@racket[handle-evt]的结果在相对于@racket[sync]的尾部调用其回调，所以如下例所示，使用递归是安全的。

@racketblock[
(define control-channel (make-channel))
(define add-channel (make-channel))
(define subtract-channel (make-channel))
(define (work state)
  (printf "Current state: ~a~n" state)
  (sync (handle-evt add-channel
                    (lambda (number)
                      (printf "Adding: ~a~n" number)
                      (work (+ state number))))
        (handle-evt subtract-channel
                    (lambda (number)
                      (printf "Subtracting: ~a~n" number)
                      (work (- state number))))
        (handle-evt control-channel
                    (lambda (kill-message)
                      (printf "Done~n")))))

(define worker (thread (lambda () (work 0))))
(channel-put add-channel 2)
(channel-put subtract-channel 3)
(channel-put add-channel 4)
(channel-put add-channel 5)
(channel-put subtract-channel 1)
(channel-put control-channel 'done)
(thread-wait worker)
]

@;{The @racket[wrap-evt] function is like @racket[handle-evt], except
that its handler is not called in tail position with respect to
@racket[sync]. At the same time, @racket[wrap-evt] disables break
exceptions during its handler's invocation.}
@racket[wrap-evt]函数类似于@racket[handle-evt]，除了它的处理器不在相对于@racket[sync]的尾部被调用。
同时，@racket[wrap-evt]在处理程序调用期间禁用中断异常。
