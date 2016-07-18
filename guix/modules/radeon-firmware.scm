(define-module (radeon-firmware)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial))

(define* (firmware-files)
  (let ((files (list "CEDAR_me.bin"
                     "CEDAR_pfp.bin"
                     "CEDAR_rlc.bin"
                     "CEDAR_smc.bin")))
    (map
     (lambda (f)
       (list f (search-path %load-path f)))
     files)))

(define-public radeon-firmware
  (package
   (name "radeon-firmware")
   (version "1.0")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out    (assoc-ref %outputs "out"))
               (firmware-dir (string-append out "/lib/firmware/radeon")))
          (mkdir-p firmware-dir)
          (for-each (lambda (input)
                      (let ((name (car input))
                            (file (cdr input)))
                        (copy-file file
                                   (string-append firmware-dir "/" name))))
                    %build-inputs)))))
   (native-inputs (firmware-files))
   (synopsis "Radeon GPU firmwares")
   (description "Nope")
   (license (non-copyleft "nope-proprietary"))
   (home-page "https://people.freedesktop.org/~agd5f/radeon_ucode/")))
