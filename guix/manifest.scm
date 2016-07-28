(use-modules (glfw) (autocutsel))

(use-package-modules
 admin
 animation
 base
 bittorrent
 commencement
 conkeror
 curl
 dns
 elf
 emacs
 feh
 file
 fonts
 gcc
 ghostscript
 gimp
 gl
 gnupg
 gnuzilla
 graphics
 graphviz
 inkscape
 linux
 m4
 mpd
 music
 pdf
 pkg-config
 rsync
 scribus
 ssh
 suckless
 sxiv
 tex
 version-control
 video
 wine
 wm
 xdisorg
 xorg
 youtube-dl
 zip
 zsh)

(packages->manifest
 (list acpi
       alsa-utils
       autocutsel
       bind-utils
       blender
       conkeror
       cpufrequtils
       curl
       dmenu
       emacs
       emacs-with-editor
       feh
       ffmpeg
       file
       font-dejavu
       font-gnu-freefont-ttf
       font-liberation
       font-terminus
       ;; gcc-toolchain-6
       gimp
       git
       glew
       glfw
       gnupg
       graphviz
       gs-fonts
       htop
       hydrogen
       i3status
       i3-wm
       icecat
       inkscape
       ;; (make-libstdc++ gcc-6)
       libx11
       libxcursor
       libxinerama
       libxrandr
       livestreamer
       lm-sensors
       m4
       gnu-make
       mesa
       mesa-utils
       milkytracker
       mpd-mpc
       mpv
       mupdf
       ncmpcpp
       obs
       openssh
       patchelf
       pd
       pkg-config
       redshift
       rsync
       rxvt-unicode
       scribus
       scrot
       setxkbmap
       soil
       sshfs-fuse
       strace
       subversion
       sxiv
       ;; synfigstudio
       taskwarrior
       texlive
       transmission
       unzip
       wine
       xbacklight
       xclip
       xinit
       xmodmap
       xproto
       xrandr
       xrdb
       xsensors
       xset
       xterm
       youtube-dl-newer
       zsh))
