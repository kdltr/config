# Terminal config
xrdb $HOME/.Xresources

# Color temperature
redshift -l 48.114722:-1.679444 &

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
