(define-module (youtube-dl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages video))

(define-public youtube-dl-newer
  (package (inherit youtube-dl)
    (version "2016.07.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://youtube-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1k7sxwcr4m4mmcdd65h8m987mrb4xarg9cnj7mvm3pqa8f589955"))))))
