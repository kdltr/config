(use-modules (glfw))

(use-package-modules
 admin
 animation
 base
 bittorrent
 commencement
 conkeror
 curl
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
 version-control
 video
 wine
 wm
 xdisorg
 xorg
 zip
 zsh)

(packages->manifest
 (list alsa-utils
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
       gcc-toolchain-6
       gimp
       git
       glew
       glfw
       gnupg
       graphviz
       gs-fonts
       htop
       i3status
       i3-wm
       icecat
       inkscape
       (make-libstdc++ gcc-6)
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
       pkg-config
       rsync
       rxvt-unicode
       scribus
       scrot
       setxkbmap
       soil
       sshfs-fuse
       strace
       sxiv
       synfigstudio
       transmission
       unzip
       wine
       xclip
       xinit
       xmodmap
       xproto
       xrandr
       xrdb
       xsensors
       xset
       youtube-dl
       zsh))