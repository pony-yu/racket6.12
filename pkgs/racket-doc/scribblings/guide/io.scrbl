#lang scribble/doc
@(require scribble/manual scribble/struct scribble/eval racket/system
          "guide-utils.rkt"
          (for-label racket/tcp racket/serialize racket/port))

@(define io-eval (make-base-eval))

@(define (threecolumn a b c)
   (make-table #f
     (list (list (make-flow (list a))
                 (make-flow (list (make-paragraph (list (hspace 1)))))
                 (make-flow (list b))
                 (make-flow (list (make-paragraph (list (hspace 1)))))
                 (make-flow (list c))))))
@(interaction-eval #:eval io-eval (print-hash-table #t))

@;{@title[#:tag "i/o" #:style 'toc]{Input and Output}}
@title[#:tag "i/o" #:style 'toc]{输入和输出}

@margin-note{
 @;{A Racket port corresponds to the Unix notion of a stream
(not to be confused with @racketmodname[racket/stream]'s streams).}
   一个Racket端口对应一个流的Unix概念（不要与@racketmodname[racket/stream]的流混淆）。
}

@;{A Racket @deftech{port} represents a source or sink of data, such as a
file, a terminal, a TCP connection, or an in-memory string.  Ports
provide sequential access in which data can be read or written a piece
of a time, without requiring the data to be consumed or produced all
at once.  More specifically, an @defterm{input port} represents a
source from which a program can read data, and an @defterm{output
port} represents a sink to which a program can write data.}
一个Racket@deftech{端口（port）}代表一个数据源或数据池，诸如一个文件、一个终端、一个TCP连接或者一个内存字符串。端口提供顺序的访问，在那里数据能够被分批次地读或写，而不需要数据被一次性接受或生成。更具体地，一个@defterm{输入端口（input port）}代表一个程序能从中读取数据的一个源，一个@defterm{输出端口（output
port）}代表一个程序能够向其中写入数据的一个池。

@local-table-of-contents[]

@;------------------------------------------------------------------------
@;{@section[#:tag "ports"]{Varieties of Ports}}
@section[#:tag "ports"]{端口的种类}

@;{Various functions create various kinds of ports. Here are a few
examples:}
不同的函数创建不同类型的端口，这里有一些例子：

@itemize[

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{
  @;{@bold{Files:} The @racket[open-output-file] function opens a
  file for writing, and @racket[open-input-file] opens a file for
  reading.}
@bold{文件（Files）：}@racket[open-output-file]函数打开供写入的一个文件，而@racket[open-input-file]打开供读取的一个文件。

@(interaction-eval #:eval io-eval (define old-dir (current-directory)))
@(interaction-eval #:eval io-eval (current-directory (find-system-path 'temp-dir)))
@(interaction-eval #:eval io-eval (when (file-exists? "data") (delete-file "data")))

@examples[
#:eval io-eval
(define out (open-output-file "data"))
(display "hello" out)
(close-output-port out)
(define in (open-input-file "data"))
(read-line in)
(close-input-port in)
]

@;{If a file exists already, then @racket[open-output-file] raises an
exception by default. Supply an option like @racket[#:exists
'truncate] or @racket[#:exists 'update] to re-write or update the
file:}
  如果一个文件已经存在，那@racket[open-output-file]默认情况下引发一个异常。提供一个如@racket[#:exists
'truncate]或@racket[#:exists 'update]的选项来重写或更新这个文件。

@examples[
#:eval io-eval
(define out (open-output-file "data" #:exists 'truncate))
(display "howdy" out)
(close-output-port out)
]

@;{Instead of having to match the open calls with close calls, most Racket
programmers will use the @racket[call-with-input-file] and
@racket[call-with-output-file] functions which take a function to call to carry
out the desired operation. This function gets as its only argument the port,
which is automatically opened and closed for the operation.}
而不是不得不用关闭调用去匹配打开调用，绝大多数Racket程序员会使用@racket[call-with-input-file]和@racket[call-with-output-file]函数接收一个函数去调用以实施预期的操作。这个函数作为端口的唯一参数，它为操作被自动打开与关闭。

@examples[
        #:eval io-eval
(call-with-output-file "data"
                        #:exists 'truncate
                        (lambda (out)
                          (display "hello" out)))
(call-with-input-file "data"
                      (lambda (in)
                        (read-line in)))
]

@(interaction-eval #:eval io-eval (when (file-exists? "data") (delete-file "data")))
@(interaction-eval #:eval io-eval (current-directory old-dir))}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 @item{
  @;{@bold{Strings:} The @racket[open-output-string] function creates
 a port that accumulates data into a string, and @racket[get-output-string]
 extracts the accumulated string. The @racket[open-input-string] function
 creates a port to read from a string.}
    @bold{字符串（Strings）：}@racket[open-output-string]函数创建一个将数据堆入一个字符串的一个端口，并且@racket[get-output-string]提取累加字符串。@racket[open-input-string]函数创建一个端口用于从字符串读取。

  @examples[
  #:eval io-eval
  (define p (open-output-string))
  (display "hello" p)
  (get-output-string p)
  (read-line (open-input-string "goodbye\nfarewell"))
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{
  @;{@bold{TCP Connections:} The @racket[tcp-connect] function
 creates both an input port and an output port for the client side of
 a TCP communication. The @racket[tcp-listen] function creates a
 server, which accepts connections via @racket[tcp-accept].}
    @bold{TCP连接（TCP Connections）：}@racket[tcp-connect]函数为一个TCP通信的客户端一侧既创建了一个输入端口也创建了一个输出端口。@racket[tcp-listen]函数创建一个服务器，它通过@racket[tcp-accept]接受连接。

  @examples[
  #:eval io-eval
  (eval:alts (define server (tcp-listen 12345)) (void))
  (eval:alts (define-values (c-in c-out) (tcp-connect "localhost" 12345)) (void))
  (eval:alts (define-values (s-in s-out) (tcp-accept server))
             (begin (define-values (s-in c-out) (make-pipe))
                    (define-values (c-in s-out) (make-pipe))))
  (display "hello\n" c-out)
  (close-output-port c-out)
  (read-line s-in)
  (read-line s-in)
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{
  @;{@bold{Process Pipes:} The @racket[subprocess] function runs a new
  process at the OS level and returns ports that correspond to the
  subprocess's stdin, stdout, and stderr. (The first three arguments
  can be certain kinds of existing ports to connect directly to the
  subprocess, instead of creating new ports.)}
    @bold{进程管道（Process Pipes}）：@racket[subprocess]函数运行在操作系统级的一个新的进程并返回与对应子进程的stdin、stdout和stderr的端口。（这首先的三个参数可以将某些现有端口直接连接到子进程，而不是创建新端口。）

  @examples[
  #:eval io-eval
  (eval:alts
   (define-values (p stdout stdin stderr)
     (subprocess #f #f #f "/usr/bin/wc" "-w"))
   (define-values (p stdout stdin stderr)
     (values #f (open-input-string "       3") (open-output-string) (open-input-string ""))))
  (display "a b c\n" stdin)
  (close-output-port stdin)
  (read-line stdout)
  (close-input-port stdout)
  (close-input-port stderr)
  ]}

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 @item{
  @;{@bold{Internal Pipes:} The @racket[make-pipe] function returns
 two ports that are ends of a pipe. This kind of pipe is internal to
 Racket, and not related to OS-level pipes for communicating between
 different processes.}
    @bold{内部管道（Internal Pipes）：}@racket[make-pipe]函数返回作为管道末端的两个端口。这种类型的管道属于Racket内部的，并且与用于不同进程之间通信的OS级管道无关。

 @examples[
  #:eval io-eval
  (define-values (in out) (make-pipe))
  (display "garbage" out)
  (close-output-port out)
  (read-line in)
 ]}

]

@;------------------------------------------------------------------------
@;{@section[#:tag "default-ports"]{Default Ports}}
@section[#:tag "default-ports"]{默认端口}

@;{For most simple I/O functions, the target port is an optional
argument, and the default is the @defterm{current input port} or
@defterm{current output port}. Furthermore, error messages are written
to the @defterm{current error port}, which is an output port. The
@racket[current-input-port], @racket[current-output-port], and
@racket[current-error-port] functions return the corresponding current
ports.}
对于大多数简单的I/O函数，这个目标端口是一个可选参数，并且这个默认值是@defterm{当前输入端口（current input port）}。此外，错误信息被写入@defterm{当前错误端口（current error port）}，这是一个输出端口。@racket[current-input-port]、@racket[current-output-port]和@racket[current-error-port]函数返回对应的当前端口。

@examples[
#:eval io-eval
(display "Hi")
(code:line (display "Hi" (current-output-port)) (code:comment @#,t{@;{the same}同样}))
]

@;{If you start the @exec{racket} program in a terminal, then the
current input, output, and error ports are all connected to the
terminal. More generally, they are connected to the OS-level stdin,
stdout, and stderr. In this guide, the examples show output written to
stdout in purple, and output written to stderr in red italics.}
如果你在终端启动@exec{racket}程序，那么当前输入、输出及错误端口都被连接到终端。更一般地，它们被连接到系统级的stdin、stdout和stderr。在本指南中，示例用紫色显示写入stdout的输出，用红色斜体显示写入stderr的输出。

@defexamples[
#:eval io-eval
(define (swing-hammer)
  (display "Ouch!" (current-error-port)))
(swing-hammer)
]

@;{The current-port functions are actually @tech{parameters}, which means
that their values can be set with @racket[parameterize].}
当前端口函数实际上是@tech{参数（parameters）}，它代表它们的值能够用@racket[parameterize]设置。

@;{@margin-note{See @secref["parameterize"] for an introduction to parameters.}}
@margin-note{参见《@secref["parameterize"]》以获得对parameters的一个说明。}

@examples[
#:eval io-eval
(let ([s (open-output-string)])
  (parameterize ([current-error-port s])
    (swing-hammer)
    (swing-hammer)
    (swing-hammer))
  (get-output-string s))
]

@; ----------------------------------------------------------------------
@;{@section[#:tag "read-write"]{Reading and Writing Racket Data}}
@section[#:tag "read-write"]{读写Racket数据}

@;{As noted throughout @secref["datatypes"], Racket provides three
ways to print an instance of a built-in value:}
就像贯穿始终的《@secref["datatypes"]》，Racket提供三种方式打印一个内建值的一个实例：

@itemize[

 @item{
  @;{@racket[print], which prints a value in the same way that is it
       printed for a @tech{REPL} result; and }
    @racket[print], 它以用于打印一个@tech{REPL}结果的相同方式打印一个值；以及
    }

 @item{
  @;{@racket[write], which prints a value in such a way that
       @racket[read] on the output produces the value back; and }
 @racket[write], 它以在输出上的@racket[read]产生值的这样一种方式打印一个值；以及
 }

 @item{
  @;{@racket[display], which tends to reduce a value to just its
       character or byte content---at least for those datatypes that
       are primarily about characters or bytes, otherwise it falls
       back to the same output as @racket[write].}
    @racket[display], 它趋向于将一个值缩小到它的字符或字节内容——至少对于那些主要关于字符或字节的数据类型——否则它会回到与@racket[write]相同的输出。
    }

]

@;{Here are some examples using each:}
这里有一些每个使用的例子：

@threecolumn[

@interaction[
(print 1/2)
(print #\x)
(print "hello")
(print #"goodbye")
(print '|pea pod|)
(print '("i" pod))
(print write)
]

@interaction[
(write 1/2)
(write #\x)
(write "hello")
(write #"goodbye")
(write '|pea pod|)
(write '("i" pod))
(write write)
]

@interaction[
(display 1/2)
(display #\x)
(display "hello")
(display #"goodbye")
(display '|pea pod|)
(display '("i" pod))
(display write)
]

]

@;{Overall, @racket[print] corresponds to the expression layer of
Racket syntax, @racket[write] corresponds to the reader layer, and
@racket[display] roughly corresponds to the character layer.}
总的来说，@racket[print]对应Racket语法的表达层，@racket[write]对应阅读层，@racket[display]大致对应字符层。

@;{The @racket[printf] function supports simple formatting of data and
text. In the format string supplied to @racket[printf], @litchar{~a}
@racket[display]s the next argument, @litchar{~s}
@racket[write]s the next argument, and @litchar{~v}
@racket[print]s the next argument.}
@racket[printf]函数支持数据与文本的简单格式。在@racket[printf]支持的格式字符串中，@litchar{~a}  @racket[display]下一个参数，@litchar{~s} @racket[write]下一个参数，而@litchar{~v} @racket[print]下一个参数。

@defexamples[
#:eval io-eval
(define (deliver who when what)
  (printf "Items ~a for shopper ~s: ~v" who when what))
(deliver '("list") '("John") '("milk"))
]

@;{After using @racket[write], as opposed to @racket[display] or
@racket[print], many forms of data can be read back in using
@racket[read]. The same values @racket[print]ed can also be parsed by
@racket[read], but the result may have extra quote forms, since a
@racket[print]ed form is meant to be read like an expression.}
使用@racket[write]后，与@racket[display]或@racket[print]不同的是，许多数据的表可以通过@racket[read]重新读入。被@racket[print]的相同值也能被@racket[read]解析，但是这个结果也许有额外的引号表，因为一个@racket[print]的表意味着类似于一个表达式那样被读入。

@examples[
#:eval io-eval
(define-values (in out) (make-pipe))
(write "hello" out)
(read in)
(write '("alphabet" soup) out)
(read in)
(write #hash((a . "apple") (b . "banana")) out)
(read in)
(print '("alphabet" soup) out)
(read in)
(display '("alphabet" soup) out)
(read in)
]

@; ----------------------------------------------------------------------
@;{@section[#:tag "serialization"]{Datatypes and Serialization}}
@section[#:tag "serialization"]{数据类型和序列化}

@;{@tech{Prefab} structure types (see @secref["prefab-struct"])
automatically support @deftech{serialization}: they can be written to
an output stream, and a copy can be read back in from an input stream:}
@tech{预制的（prefab）}结构类型（查看《@secref["prefab-struct"]》）自动支持@deftech{序列化（serialization）}：它们可被写入一个输出流同时一个副本可以从输入流中读回：

@interaction[
(define-values (in out) (make-pipe))
(write #s(sprout bean) out)
(read in)
]

@;{Other structure types created by @racket[struct], which offer
more abstraction than @tech{prefab} structure types, normally
@racket[write] either using @racketresultfont{#<....>} notation (for
opaque structure types) or using @racketresultfont{#(....)} vector
notation (for transparent structure types). In neither case can the
result be read back in as an instance of the structure type:}
被@racket[struct]创建的其它结构类型，提供较@tech{预制的}结构类型更多的抽象，通常@racket[write]既使用@racketresultfont{#<....>}记号（对于不透明结构类型）也使用@racketresultfont{#(....)}矢量记号（对于透明结构类型）。不论在那种情况下这个结果都不能作为结构类型的一个实例被读回。

@interaction[
(struct posn (x y))
(write (posn 1 2))
(define-values (in out) (make-pipe))
(write (posn 1 2) out)
(read in)
]

@interaction[
(struct posn (x y) #:transparent)
(write (posn 1 2))
(define-values (in out) (make-pipe))
(write (posn 1 2) out)
(define v (read in))
v
(posn? v)
(vector? v)
]

@;{The @racket[serializable-struct] form defines a structure type
that can be @racket[serialize]d to a value that can be printed using
@racket[write] and restored via @racket[read]. The @racket[serialize]d
result can be @racket[deserialize]d to get back an instance of the
original structure type. The serialization form and functions are
provided by the @racketmodname[racket/serialize] library.}
@racket[serializable-struct]表定义一个结构类型，它能够被@racket[serialize]为一个值，这个值可使用@racket[write]被打印并通过@racket[read]读入。@racket[serialize]的结果可被@racket[deserialize]为原始结构类的一个实例。序列化表和函数通过@racketmodname[racket/serialize]库提供。


@examples[
(require racket/serialize)
(serializable-struct posn (x y) #:transparent)
(deserialize (serialize (posn 1 2)))
(write (serialize (posn 1 2)))
(define-values (in out) (make-pipe))
(write (serialize (posn 1 2)) out)
(deserialize (read in))
]

@;{In addition to the names bound by @racket[struct],
@racket[serializable-struct] binds an identifier with deserialization
information, and it automatically @racket[provide]s the
deserialization identifier from a module context. This deserialization
identifier is accessed reflectively when a value is deserialized.}
除了被@racket[struct]绑定的名字外，@racket[serializable-struct]绑定一个具有反序列化信息的标识符，并且它会自动从一个模块上下文@racket[provide]这个反序列化标识符。当一个值被反序列化时这个反序列化标识符被反射地访问。

@; ----------------------------------------------------------------------
@;{@section[#:tag "encodings"]{Bytes, Characters, and Encodings}}
@section[#:tag "encodings"]{字节、字符和编码}

@;{Functions like @racket[read-line], @racket[read], @racket[display],
and @racket[write] all work in terms of @tech{characters} (which
correspond to Unicode scalar values). Conceptually, they are
implemented in terms of @racket[read-char] and @racket[write-char].}
类似@racket[read-line]、@racket[read]、@racket[display]和@racket[write]的函数都根据@tech{字符（character）}（它对应于Unicode标量值）工作。概念上来说，它们根据@racket[read-char]和@racket[write-char]被实现。

@;{More primitively, ports read and write @tech{bytes}, instead of
@tech{characters}. The functions @racket[read-byte] and
@racket[write-byte] read and write raw bytes. Other functions, such as
@racket[read-bytes-line], build on top of byte operations instead of
character operations.}
更初级一点，端口读和写@tech{字节（byte）}而不是@tech{字符（character）}。函数@racket[read-byte]与@racket[write-byte]读和写原始字节。其它函数，比如@racket[read-bytes-line]，建立在字节操作的顶层而不是字符操作。

@;{In fact, the @racket[read-char] and @racket[write-char] functions are
conceptually implemented in terms of @racket[read-byte] and
@racket[write-byte]. When a single byte's value is less than 128, then
it corresponds to an ASCII character. Any other byte is treated as
part of a UTF-8 sequence, where UTF-8 is a particular standard way of
encoding Unicode scalar values in bytes (which has the nice property
that ASCII characters are encoded as themselves). Thus, a single
@racket[read-char] may call @racket[read-byte] multiple times, and a
single @racket[write-char] may generate multiple output bytes.}
事实上，@racket[read-char]和@racket[write-char]函数概念上根据@racket[read-byte]和@racket[write-byte]被实现。当一个单一字节的值小于128时，那么它对应于一个ASCII字符。任何其它的字节被视为一个UTF-8序列的一部分，其中UTF-8是以字节为单位的编码Unicode标量值的一个特殊标准方式（它具有ASCII字符作为它们自身编码的优良属性）。此外，一个单个@racket[read-char]可能调用@racket[read-byte]多次，并且一个单个@racket[write-char]可能生成多个输出字节。

@;{The @racket[read-char] and @racket[write-char] operations
@emph{always} use a UTF-8 encoding. If you have a text stream that
uses a different encoding, or if you want to generate a text stream in
a different encoding, use @racket[reencode-input-port] or
@racket[reencode-output-port]. The @racket[reencode-input-port]
function converts an input stream from an encoding that you specify
into a UTF-8 stream; that way, @racket[read-char] sees UTF-8
encodings, even though the original used a different encoding. Beware,
however, that @racket[read-byte] also sees the re-encoded data,
instead of the original byte stream.}
@racket[read-char]和@racket[write-char]操作@emph{总}使用一个UTF-8编码。如果你有一个使用一个不同编码的文本流，或者如果你想在一个不同编码中生成一个文本流，使用@racket[reencode-input-port]或@racket[reencode-output-port]。@racket[reencode-input-port]函数从一个你指定为一个UTF-8流的编码中转换一个输入流；以这种方式，@racket[read-char]明白UTF-8编码，即使这个源文件使用了一个不同的编码。但要小心，那个@racket[read-byte]也明白这个重编码数据，而不是原始字节流。

@; ----------------------------------------------------------------------
@;{@section[#:tag "io-patterns"]{I/O Patterns}}
@section[#:tag "io-patterns"]{I/O模式}

@(begin
  (define port-eval (make-base-eval))
  (interaction-eval #:eval port-eval (require racket/port)))

@;{If you want to process individual lines of a file, then you can use
@racket[for] with @racket[in-lines]:}
如果你想处理一个文件的单个行，那么你可以用@racket[in-lines]使用@racket[for]：

@interaction[
(define (upcase-all in)
  (for ([l (in-lines in)])
    (display (string-upcase l))
    (newline)))
(upcase-all (open-input-string
             (string-append
              "Hello, World!\n"
              "Can you hear me, now?")))
]

@;{If you want to determine whether ``hello'' appears in a file，then you
could search separate lines, but it's even easier to simply apply a
regular expression (see @secref["regexp"]) to the stream:}
如果你想确定是否“hello”出现在一个文件中，那你可以搜索独立的行，但是更简便的方法是对这个流简单应用一个正则表达式（参见《@secref["regexp"]》）：

@interaction[
(define (has-hello? in)
  (regexp-match? #rx"hello" in))
(has-hello? (open-input-string "hello"))
(has-hello? (open-input-string "goodbye"))
]

@;{If you want to copy one port into another, use @racket[copy-port] from
@racketmodname[racket/port], which efficiently transfers large blocks
when lots of data is available, but also transfers small blocks
immediately if that's all that is available:}
如果你想拷贝一个端口至另一个，使用来自@racketmodname[racket/port]的@racket[copy-port]，它能够在大量数据可用时有效转移大的块，但如果可以的话也立即转移小数据块。：

@interaction[
#:eval port-eval
(define o (open-output-string))
(copy-port (open-input-string "broom") o)
(get-output-string o)
]

@close-eval[port-eval]

@; ----------------------------------------------------------------------

@close-eval[io-eval]
