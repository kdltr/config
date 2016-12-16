# Terminal config
xrdb $HOME/.Xresources

# Color temperature
# redshift -l 48.114722:-1.679444 &

# Clipboard management
autocutsel -fork
autocutsel -selection PRIMARY -fork

# keyboard config
setxkbmap -option compose:caps

# background and bar
$HOME/.fehbg
while true; do
    xsetroot -name "`date` `uptime|awk -F, '{print $2}'`"
    sleep 15
done &

# currently playing music
current_music &

# MA editor registry
ma -registry &

exec dwm
