(define-module (youtube-dl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system python)
  #:use-module (gnu packages))

(define-public youtube-dl
  (package
    (name "youtube-dl")
    (version "2016.07.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://youtube-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "017x2hqc2bacypjmn9ac9f91y9y6afydl0z7dich5l627494hvfg"))))
    (build-system python-build-system)
    (home-page "https://youtube-dl.org")
    (arguments
     ;; The problem here is that the directory for the man page and completion
     ;; files is relative, and for some reason, setup.py uses the
     ;; auto-detected sys.prefix instead of the user-defined "--prefix=FOO".
     ;; So, we need pass the prefix directly.  In addition, make sure the Bash
     ;; completion file is called 'youtube-dl' rather than
     ;; 'youtube-dl.bash-completion'.
     `(#:phases (modify-phases %standard-phases
                  (add-before 'install 'fix-the-data-directories
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        (mkdir "bash-completion")
                        (rename-file "youtube-dl.bash-completion"
                                     "bash-completion/youtube-dl")
                        (substitute* "setup.py"
                          (("youtube-dl\\.bash-completion")
                           "bash-completion/youtube-dl")
                          (("'etc/")
                           (string-append "'" prefix "/etc/"))
                          (("'share/")
                           (string-append "'" prefix "/share/")))))))))
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "Youtube-dl is a small command-line program to download videos from
YouTube.com and a few more sites.")
    (license public-domain)))
