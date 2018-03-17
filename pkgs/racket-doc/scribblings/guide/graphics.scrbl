#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@;{@title[#:tag "graphics"]{Graphics and GUIs}}
@title[#:tag "graphics"]{图形和图形用户界面}

@;{Racket provides many libraries for graphics and graphical user
interfaces (GUIs):}
Racket为图形和图形用户界面（GUI）提供许多库：

@itemlist[

 @;{@item{The @racketmodname[racket/draw] library provides basic drawing
       tools, including drawing contexts such as bitmaps and
       PostScript files.

       See @other-doc['(lib "scribblings/draw/draw.scrbl")]
       for more information.}}
 @item{@racketmodname[racket/draw]提供了基本的绘图工具，包括绘制背景如位图（bitmap）和PostScript文件。
  参见@other-doc['(lib "scribblings/draw/draw.scrbl")]获取更多信息。}

 @;{@item{The @racketmodname[racket/gui] library provides GUI widgets
       such as windows, buttons, checkboxes, and text fields. The
       library also includes a sophisticated and extensible text
       editor.

       See @other-doc['(lib "scribblings/gui/gui.scrbl")]
       for more information.}}
   @item{@racketmodname[racket/gui]库提供的GUI部件，如窗口（window）、按钮（button）、复选框（checkboxe）和文本字段（text field）。该库还包括一个复杂的、可扩展的文本编辑器。
 参见@other-doc['(lib "scribblings/gui/gui.scrbl")]获取更多信息。}

 @;{@item{The @racketmodname[pict] library provides a more
       functional abstraction layer over @racketmodname[racket/draw].
       This layer is especially useful for creating slide
       presentations with @seclink[#:doc '(lib
       "scribblings/slideshow/slideshow.scrbl") "top"]{Slideshow}, but
       it is also useful for creating images for @seclink[#:doc '(lib
       "scribblings/scribble/scribble.scrbl") "top"]{Scribble}
       documents or other drawing tasks. Pictures created with the
       @racketmodname[pict] library can be rendered to any
       drawing context.

       See @other-doc['(lib "scribblings/slideshow/slideshow.scrbl")]
       for more information.}}
   @item{@racketmodname[pict]库一个在@racketmodname[racket/draw]之上的更多功能的抽象层。这一层对用@seclink[#:doc '(lib
       "scribblings/slideshow/slideshow.scrbl") "top"]{Slideshow}创建幻灯片演示特别有用，但它对为@seclink[#:doc '(lib
       "scribblings/scribble/scribble.scrbl") "top"]{Scribble}文档或其它绘图任务创建图像也是有用的。随着图像库创建的图片可以呈现任何绘画语境。
 参见@other-doc['(lib "scribblings/slideshow/slideshow.scrbl")]获取更多信息。}
   

 @;{@item{The @racketmodname[2htdp/image #:indirect] library is similar to
       @racketmodname[pict]. It is more streamlined for
       pedagogical use, but also slightly more specific to screen and
       bitmap drawing.

       See @racketmodname[2htdp/image #:indirect] for more information.}}
   @item{@racketmodname[2htdp/image #:indirect]库类似于@racketmodname[pict]。对于教学使用来说它更精简，但对屏幕和位图绘图也更轻量一些。
 参见@racketmodname[2htdp/image #:indirect]获取更多信息。}

 @;{@item{The @racketmodname[sgl #:indirect] library provides OpenGL for 3-D
       graphics. The context for rendering OpenGL can be a window or
       bitmap created with @racketmodname[racket/gui].

       See @other-doc['(lib "sgl/scribblings/sgl.scrbl") #:indirect "SGL"] for more
       information.}}
  @item{@racketmodname[sgl #:indirect]库为三维（3-D）图形提供OpenGL。渲染OpenGL的上下文可以是用@racketmodname[racket/gui]创建的的一个窗口或位图。
 参见@other-doc['(lib "sgl/scribblings/sgl.scrbl") #:indirect "SGL"]获取更多信息。}

]
