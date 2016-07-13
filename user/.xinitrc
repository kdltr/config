# Terminal config
xrdb $HOME/.Xresources

# Clipboard management
autocutsel -fork
autocutsel -selection PRIMARY -fork

# keyboard config
setxkbmap -option compose:caps
xmodmap ~/.Xmodmap

# background
$HOME/.fehbg

# currently playing music
current_music &

exec i3
