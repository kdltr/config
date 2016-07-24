;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Tobias Geerinckx-Rice <tobias.geerinckx.rice@gmail.com>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Raymond Nicholson <rain1@openmailbox.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (linux-nonlibre)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages base)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public (system->linux-architecture arch)
  "Return the Linux architecture name for ARCH, a Guix system name such as
\"x86_64-linux\"."
  (let ((arch (car (string-split arch #\-))))
    (cond ((string=? arch "i686") "i386")
          ((string-prefix? "mips" arch) "mips")
          ((string-prefix? "arm" arch) "arm")
          ((string-prefix? "aarch64" arch) "arm64")
          (else arch))))

(define (linux-nonlibre-urls version)
  "Return a list of URLs for Linux-Libre VERSION."
  (list (string-append
         "mirror://kernel.org/linux/kernel/v4.x/linux-"
         version ".tar.xz")))

(define-public linux-nonlibre-headers
  (let* ((version "3.14.37")
         (build-phase
          (lambda (arch)
            `(lambda _
               (setenv "ARCH" ,(system->linux-architecture arch))
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))

               (and (zero? (system* "make" "defconfig"))
                    (zero? (system* "make" "mrproper" "headers_check"))))))
         (install-phase
          `(lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (and (zero? (system* "make"
                                    (string-append "INSTALL_HDR_PATH=" out)
                                    "headers_install"))
                    (begin
                      (mkdir (string-append out "/include/config"))
                      (call-with-output-file
                          (string-append out
                                         "/include/config/kernel.release")
                        (lambda (p)
                          (format p "~a-default~%" ,version)))

                      ;; Remove the '.install' and '..install.cmd' files; the
                      ;; latter contains store paths, which pulls in bootstrap
                      ;; binaries in the build environment, and prevents bit
                      ;; reproducibility for the bootstrap binaries.
                      (for-each delete-file (find-files out "\\.install"))

                      #t))))))
   (package
    (name "linux-nonlibre-headers")
    (version version)
    (source (origin
             (method url-fetch)
             (uri (linux-nonlibre-urls version))
             (sha256
              (base32
               "1blxr2bsvfqi9khj4cpspv434bmx252zak2wsbi2mgl60zh77gza"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases (alist-replace
                 'build ,(build-phase (or (%current-target-system)
                                          (%current-system)))
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:allowed-references ()
       #:tests? #f))
    (synopsis "GNU Linux kernel headers")
    (description "Headers of the Linux kernel.")
    (license license:gpl2)
    (home-page "http://kernel.org"))))

(define* (kernel-config system #:key variant)
  "Return the absolute file name of the Linux-Libre build configuration file
for SYSTEM and optionally VARIANT, or #f if there is no such configuration."
  (and-let* ((arch (match system
                     ("i686-linux"
                      "i686")
                     ("x86_64-linux"
                      "x86_64")
                     (_
                      #f)))
             (name (string-append "linux-libre-"
                                  (if variant
                                      (string-append variant "-")
                                      "")
                                  arch
                                  ".conf"))
             (file (string-append "gnu/packages/" name)))
    (search-path %load-path file)))

(define %boot-logo-patch
  ;; Linux-Libre boot logo featuring Freedo and a gnu.
  (origin
    (method url-fetch)
    (uri (string-append "http://www.fsfla.org/svn/fsfla/software/linux-libre/"
                        "lemote/gnewsense/branches/3.16/100gnu+freedo.patch"))
    (sha256
     (base32
      "1hk9swxxc80bmn2zd2qr5ccrjrk28xkypwhl4z0qx4hbivj7qm06"))))


(define-public linux-nonlibre
  (let* ((version "4.6.4")
         (build-phase
          '(lambda* (#:key system inputs #:allow-other-keys #:rest args)
             ;; Avoid introducing timestamps
             (setenv "KCONFIG_NOTIMESTAMP" "1")
             (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))

             ;; Apply the neat patch.
             (system* "patch" "-p1" "--force"
                      "-i" (assoc-ref inputs "patch/freedo+gnu"))

             (let ((arch (car (string-split system #\-))))
               (setenv "ARCH"
                       (cond ((string=? arch "i686") "i386")
                             ((string=? arch "mips64el") "mips")
                             (else arch)))
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH")))

             (let ((build  (assoc-ref %standard-phases 'build))
                   (config (assoc-ref inputs "kconfig")))

               ;; Use the architecture-specific config if available, and
               ;; 'defconfig' otherwise.
               (if config
                   (begin
                     (copy-file config ".config")
                     (chmod ".config" #o666))
                   (system* "make" "defconfig"))

               ;; Appending works even when the option wasn't in the
               ;; file.  The last one prevails if duplicated.
               (let ((port (open-file ".config" "a")))
                 (display (string-append "CONFIG_NET_9P=m\n"
                                         "CONFIG_NET_9P_VIRTIO=m\n"
                                         "CONFIG_VIRTIO_BLK=m\n"
                                         "CONFIG_VIRTIO_NET=m\n"
                                         ;; https://lists.gnu.org/archive/html/guix-devel/2014-04/msg00039.html
                                         "CONFIG_DEVPTS_MULTIPLE_INSTANCES=y\n"
                                         "CONFIG_VIRTIO_PCI=m\n"
                                         "CONFIG_VIRTIO_BALLOON=m\n"
                                         "CONFIG_VIRTIO_MMIO=m\n"
                                         "CONFIG_VIRTIO_CONSOLE=m\n"
                                         "CONFIG_FUSE_FS=m\n"
                                         "CONFIG_CIFS=m\n"
                                         "CONFIG_9P_FS=m\n")
                          port)
                 (close-port port))

               (zero? (system* "make" "oldconfig"))

               ;; Call the default `build' phase so `-j' is correctly
               ;; passed.
               (apply build #:make-flags "all" args))))
         (install-phase
          `(lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (full   (assoc-ref outputs "full"))
                    (moddir (string-append out "/lib/modules"))
                    (mit    (assoc-ref inputs "module-init-tools")))
               (mkdir-p moddir)
               (for-each (lambda (file)
                           (copy-file file
                                      (string-append out "/" (basename file))))
                         (find-files "." "^(bzImage|vmlinuz|System\\.map)$"))
               (copy-file ".config" (string-append out "/config"))
               (system* "cp" "-r" "." full)
               (zero? (system* "make"
                               (string-append "DEPMOD=" mit "/sbin/depmod")
                               (string-append "MODULE_DIR=" moddir)
                               (string-append "INSTALL_PATH=" out)
                               (string-append "INSTALL_MOD_PATH=" out)
                               "INSTALL_MOD_STRIP=1"
                               "modules_install"))
               ))))
   (package
    (name "linux-nonlibre")
    (version version)
    (source (origin
             (method url-fetch)
             (uri (linux-nonlibre-urls version))
             (sha256
              (base32
               "0zpz29hgwdwkil6rakn08bdq77qjcz8q18qlkfc43s84f4fd8s45"))))
    (build-system gnu-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (outputs '("out" "full"))
    (native-inputs `(("perl" ,perl)
                     ("bc" ,bc)
                     ("openssl" ,openssl)
                     ("module-init-tools" ,module-init-tools)
                     ("patch/freedo+gnu" ,%boot-logo-patch)

                     ,@(let ((conf (kernel-config
                                    (or (%current-target-system)
                                        (%current-system))
                                    #:variant (version-major+minor version))))
                         (if conf
                             `(("kconfig" ,conf))
                             '()))))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases (alist-replace
                 'build ,build-phase
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f))
    (synopsis "100% free redistribution of a cleaned Linux kernel")
    (description
     "GNU Linux is an operating system kernel.")
    (license license:gpl2)
    (home-page "http://kernel.org"))))
