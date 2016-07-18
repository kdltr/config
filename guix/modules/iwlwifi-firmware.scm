(define-module (iwlwifi-firmware)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial))

(define* (firmware-files)
  (let ((files (list "iwlwifi-7265-16.ucode"
                     "iwlwifi-7265D-16.ucode")))
    (map
     (lambda (f)
       (list f (search-path %load-path f)))
     files)))

(define-public iwlwifi-firmware
  (package
   (name "iwlwifi-firmware")
   (version "1.0")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out    (assoc-ref %outputs "out"))
               (firmware-dir (string-append out "/lib/firmware/")))
          (mkdir-p firmware-dir)
          (for-each (lambda (input)
                      (let ((name (car input))
                            (file (cdr input)))
                        (copy-file file
                                   (string-append firmware-dir "/" name))))
                    %build-inputs)))))
   (native-inputs (firmware-files))
   (synopsis "Intel WiFi card firmwares")
   (description "Nope")
   (license (non-copyleft "nope-proprietary"))
   (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")))
