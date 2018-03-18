#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@;{@title{More Libraries}}
@title[#:tag "More_Libraries"]{更多的库}

@;{This guide covers only the Racket language and libraries that are
documented in @|Racket|. The Racket distribution includes many
additional libraries.}
本指南只涵盖记录在@|Racket|的Racket语言和库。Racket分配包括许多额外的库。

@include-section["graphics.scrbl"]

@;{@section{The Web Server}}
@section[#:tag "The_Web_Server"]{Web服务器}

@;{@other-manual['(lib "web-server/scribblings/web-server.scrbl")]
describes the Racket web server, which supports servlets implemented
in Racket.}
@other-manual['(lib "web-server/scribblings/web-server.scrbl")]介绍了Racket的Web服务器，它支持Racket的servlet实现。

@;{@section{Using Foreign Libraries}}
@section[#:tag "Using_Foreign_Libraries"]{使用外部库}

@;{@other-manual['(lib "scribblings/foreign/foreign.scrbl")] describes
tools for using Racket to access libraries that are normally used by C
programs.}
@other-manual['(lib "scribblings/foreign/foreign.scrbl")]描述使用Racket访问C程序的工具。

@;{@section{And More}}
@section[#:tag "And_More"]{其它更多的库}

@;{@link["../index.html"]{Racket Documentation} lists documentation for
many other installed libraries. Run @exec{raco docs} to find
documentation for libraries that are installed on your system and
specific to your user account.}
@link["../index.html"]{Racket文档}列出了许多其它安装库的文档。同时发现，运行@exec{raco docs}去寻找文档以获取库，它们安装在你的系统上并特定于你的用户帐户。

@;{@link["https://pkgs.racket-lang.org/"]{The Racket package repository}
offer even more downloadable packages that are contributed by
Racketeers.}
@link["https://pkgs.racket-lang.org/"]{Racket包库}提供更多的可下载的包，它们由Racket使用者贡献。

@;{The legacy @link["http://planet.racket-lang.org/"]{@|PLaneT|} site
offers additional packages, although maintained packages have
generally migrated to the newer package repository.}
虽然遗留的包通常迁移到较新的包存储库，但遗留的@link["http://planet.racket-lang.org/"]{@|PLaneT|}站点提供了额外的包。