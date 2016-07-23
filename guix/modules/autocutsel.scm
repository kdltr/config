(define-module (autocutsel)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg))

(define-public autocutsel
  (package
   (name "autocutsel")
   (version "0.10.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/sigmike/autocutsel/releases/download/"
                         version "/" "autocutsel-" version ".tar.gz"))
     (sha256
      (base32
       "0gsys2dzh4az51ndcsabhlbbrjn2nm75lnjr45kg6r8sm8q66dx2"))))
   (build-system gnu-build-system)
   (inputs `(("libx11" ,libx11)
             ("libxaw" ,libxaw)))
   (home-page "http://www.nongnu.org/autocutsel/")
   (synopsis "Tool to synchronize the different Xorg clipboard buffers")
   (description "Autocutsel tracks changes in the server's cutbuffer and
CLIPBOARD selection. When the CLIPBOARD is changed, it updates the
cutbuffer. When the cutbuffer is changed, it owns the CLIPBOARD selection.
The cutbuffer and CLIPBOARD selection are always synchronized.")
   (license gpl2)))
