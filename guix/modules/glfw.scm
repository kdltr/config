(define-module (glfw)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl))

(define-public glfw
  (package
   (name "glfw")
   (version "3.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/glfw/glfw/releases/download/"
                         version "/" "glfw-" version ".zip"))
     (sha256
      (base32
       "0lpns1ss2qxgnxd5hygv1kdb2lyj5vpwj9nxd4441g9jjw93m66r"))))
   (build-system cmake-build-system)
   (arguments
    '(#:tests? #f ;; there are no tests
      #:configure-flags (list "-DBUILD_SHARED_LIBS=1")))
   (native-inputs `(("unzip" ,unzip)))
   (inputs `(("libx11" ,libx11)
             ("xproto" ,xproto)
             ("libxrandr" ,libxrandr)
             ("libxinerama" ,libxinerama)
             ("libxcursor" ,libxcursor)))
   (propagated-inputs `(("mesa-headers" ,mesa-headers)))
   (home-page "http://www.glfw.org/")
   (synopsis "Library for managing windows, inputs and graphical contexts")
   (description "GLFW is a free, Open Source, multi-platform library for
OpenGL, OpenGL ES and Vulkan development on the desktop. It provides a
simple API for creating windows, contexts and surfaces, receiving input and
events.")
   (license zlib)))
