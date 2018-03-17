#lang scribble/doc
@(require scribble/manual
          (except-in "guide-utils.rkt" log-message)
          scribble/eval
          scriblib/figure
          racket/port
          racket/contract
          (for-label racket/place/distributed
                     racket/match
                     racket/place/define-remote-server))

@(define (codeblockfromfile filename)
   (call-with-input-file
     filename
     (lambda (i)
       (codeblock0 (port->string i)))))

@;{@title[#:tag "distributed-places"]{Distributed Places}}
@title[#:tag "distributed-places"]{分布式现场}

@;{The @racketmodname[racket/place/distributed] library provides support for
distributed programming.}
@racketmodname[racket/place/distributed]库为分布式编程提供了支持。

@;{The example bellow demonstrates how to launch a remote racket node instance,
launch remote places on the new remote node instance, and start an
event loop that monitors the remote node instance.}
该示例演示了如何启动一个远程racket节点实例，在新的远程节点实例上启动远程现场，以及启动一个监视远程节点实例的事件循环。

@;{The example code can also be found in
@filepath{racket/distributed/examples/named/master.rkt}.}
示例代码也可以在@filepath{racket/distributed/examples/named/master.rkt}中找到。


@figure["named-example-master" "examples/named/master.rkt"]{
@codeblockfromfile[(path->string (collection-file-path "master.rkt" "racket/place/distributed/examples/named"))]
}


@;{The @racket[spawn-remote-racket-node] primitive connects to
@tt{"localhost"} and starts a racloud node there that listens on port
6344 for further instructions.  The handle to the new racloud node is
assigned to the @racket[remote-node] variable. Localhost is used so that
the example can be run using only a single machine.  However localhost
can be replaced by any host with ssh publickey access and racket.  The
@racket[supervise-named-dynamic-place-at] creates a new place on the
@racket[remote-node].  The new place will be identified in the future by
its name symbol @racket['tuple-server].  A place descriptor is
expected to be returned by invoking @racket[dynamic-place] with the
@racket[tuple-path] module path and the @racket['make-tuple-server]
symbol.}
@racket[spawn-remote-racket-node]最初连接到@tt{"本地主机（localhost）"}并开始一个在端口6344侦听的racloud节点以做进一步说明。对新racloud节点的处理被分配给@racket[remote-node]变量。本地主机被使用以便这个例子可以只使用一个单一的机器来运行。然而本地主机可以通过用ssh公钥访问任何的主机和racket更换。@racket[supervise-named-dynamic-place-at]在@racket[remote-node]上创建一个新现场。新的现场将由它的名称符号@racket['tuple-server]在前景中标记。一个现场描述符被要求通过使用@racket[tuple-path]模块路径和@racket['make-tuple-server]的@racket[dynamic-place]返回。

@;{The code for the tuple-server place exists in the file
@filepath{tuple.rkt}.  The @filepath{tuple.rkt} file contains the use of
@racket[define-named-remote-server] form, which defines a RPC server
suitiable for invocation by @racket[supervise-named-dynamic-place-at].}
元组服务器现场的代码存在于文件@filepath{tuple.rkt}中。@filepath{tuple.rkt}文件包含@racket[define-named-remote-server]表的使用，为了调用它通过@racket[supervise-named-dynamic-place-at]恰当地定义了一个实际的RPC服务器。


@figure["named-example" "examples/named/tuple.rkt"]{
@codeblockfromfile[(path->string (collection-file-path "tuple.rkt" "racket/place/distributed/examples/named"))]
}



@;{The @racket[define-named-remote-server] form takes an identifier and a
list of custom expressions as its arguments.  From the identifier a
place-thunk function is created by prepending the @tt{make-} prefix.
In this case @racket[make-tuple-server].  The
@racket[make-tuple-server] identifier is the
@racket[place-function-name] given to the
@racket[supervise-named-dynamic-place-at] form above. The
@racket[define-state] custom form translates into a simple
@racket[define] form, which is closed over by the @racket[define-rpc]
form.}
@racket[define-named-remote-server]表接受一个标识符和一个自定义表达式列表作为它的参数。从一个place-thunk函数标识符通过预先计划这个@tt{make-}前缀来被创建。在这种情况下@racket[make-tuple-server]。@racket[make-tuple-server]标识符是@racket[place-function-name]给到上边的@racket[supervise-named-dynamic-place-at]表。@racket[define-state]定制表转换成一个简单的@racket[define]表，它通过@racket[define-rpc]表关闭。

@;{The @racket[define-rpc] form is expanded into two parts. The first
part is the client stubs that call the rpc functions. The client
function name is formed by concatenating the
@racket[define-named-remote-server] identifier, @tt{tuple-server},
with the RPC function name @tt{set} to form @racket[tuple-server-set].
The RPC client functions take a destination argument which is a
@racket[remote-connection%] descriptor and then the RPC function
arguments. The RPC client function sends the RPC function name,
@racket[set], and the RPC arguments to the destination by calling an
internal function @racket[named-place-channel-put]. The RPC client
then calls @racket[named-place-channel-get] to wait for the RPC
response.}
@racket[define-rpc]表扩展为两部分。第一部分是调用rpc函数的客户机存根。客户机函数名字是通过连接@racket[define-named-remote-server]标识符产生的，@tt{元组服务器（tuple-server）}，用RPC函数名称@tt{设置}以产生@racket[tuple-server-set]。RPC客户机函数获取一个目标参数，它是一个@racket[remote-connection%]描述符，进而是RPC函数参数。这个RPC客户机函数通过调用一个内部函数@racket[named-place-channel-put]将RPC函数名、@racket[set]和RPC参数发送到目标。RPC客户机接下来调用@racket[named-place-channel-get]以等待RPC响应。

@;{The second expansion part of @racket[define-rpc] is the server
implementation of the RPC call.  The server is implemented by a match
expression inside the @racket[make-tuple-server] function.  The match
clause for @racket[tuple-server-set] matches on messages beginning
with the @racket['set] symbol. The server executes the RPC call with
the communicated arguments and sends the result back to the RPC
client.}
@racket[define-rpc]的第二个扩展部分是RPC调用的服务器实现。服务器由@racket[make-tuple-server]函数内的一个匹配表达式实现。@racket[tuple-server-set]的匹配子句匹配以用@racket['set]符号开头的消息。服务器通过通信参数执行RPC调用，并将结果发送回RPC客户机。

@;{The @racket[define-cast] form is similar to the @racket[define-rpc] form
except there is no reply message from the server to client}
除了没有从服务器到客户机的应答消息外， @racket[define-cast]表类似于@racket[define-rpc]表。

@figure["define-named-remote-server-expansion" "Expansion of define-named-remote-server"]{
@codeblock0{
(module tuple racket/base
  (require racket/place
           racket/match)
  (define/provide
   (tuple-server-set dest k v)
   (named-place-channel-put dest (list 'set k v))
   (named-place-channel-get dest))
  (define/provide
   (tuple-server-get dest k)
   (named-place-channel-put dest (list 'get k))
   (named-place-channel-get dest))
  (define/provide
   (tuple-server-hello dest)
   (named-place-channel-put dest (list 'hello)))
  (define/provide
   (make-tuple-server ch)
    (let ()
      (define h (make-hash))
      (let loop ()
        (define msg (place-channel-get ch))
        (define (log-to-parent-real 
                  msg 
                  #:severity (severity 'info))
          (place-channel-put 
            ch 
            (log-message severity msg)))
        (syntax-parameterize
         ((log-to-parent (make-rename-transformer 
                           #'log-to-parent-real)))
         (match
          msg
          ((list (list 'set k v) src)
           (define result (let () (hash-set! h k v) v))
           (place-channel-put src result)
           (loop))
          ((list (list 'get k) src)
           (define result (let () (hash-ref h k #f)))
           (place-channel-put src result)
           (loop))
          ((list (list 'hello) src)
           (define result
             (let () 
               (printf "Hello from define-cast\n") 
               (flush-output)))
           (loop))))
        loop))))
}
}





