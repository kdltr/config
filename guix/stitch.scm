(use-modules (gnu) (gnu system nss) (linux-nonlibre) (iwlwifi-firmware))
(use-service-modules networking shepherd)
(use-package-modules certs xorg xdisorg fontutils fonts admin zsh suckless linux)

(define powertop-tuning-service-type
  (shepherd-service-type
   'powertop-tuning
   (lambda _
     (shepherd-service
      (documentation "Auto-tune powertop tunables to increase battery life")
      (provision '(powertop-tuning))
      (start #~(lambda _
                 (zero? (system* (string-append #$powertop "/sbin/powertop")
                                 "--auto-tune"))))
      (respawn? #f)))))

(define (powertop-tuning-service)
  (service powertop-tuning-service-type '()))


(operating-system
 (host-name "stitch")
 (timezone "Europe/Paris")
 (locale "en_US.UTF-8")

 (bootloader (grub-configuration (device "/dev/sda")))

 (initrd (lambda (file-systems . rest)
           (apply base-initrd file-systems
                  #:extra-modules '("fuse")
                  rest)))

 (kernel linux-nonlibre)

 (firmware (cons* iwlwifi-firmware
                  %base-firmware))

 (file-systems (cons* (file-system
                       (title 'label)
                       (device "root")
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device "tmpfs")
                       (mount-point "/tmp")
                       (type "tmpfs")
                       (check? #f)
                       (create-mount-point? #t))
                      ;; %fuse-control-file-system
                      %base-file-systems))

 (users (cons (user-account
               (name "kooda")
               (comment "The otter user")
               (group "users")
               (supplementary-groups '("wheel" "netdev"
                                       "audio" "video"))
               (home-directory "/home/kooda")
               (shell #~(string-append #$zsh "/bin/zsh")))
              %base-user-accounts))

 ;; This is where we specify system-wide packages.
 (packages (cons* nss-certs             ;for HTTPS access
                  xorg-server xf86-video-intel xf86-input-libinput
                  fontconfig font-alias font-adobe75dpi font-terminus font-dejavu font-misc-misc
                  slock
                  %base-packages))

 (setuid-programs (cons* #~(string-append #$xorg-server "/bin/Xorg")
                         #~(string-append #$slock "/bin/slock")
                         %setuid-programs))

 (services (cons* (console-keymap-service "fr-bepo")
                  (powertop-tuning-service)
                  (dhcp-client-service)
                  %base-services)))
