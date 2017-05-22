# Terminal config
xrdb $HOME/.Xresources

# Clipboard management
#autocutsel -fork
#autocutsel -selection PRIMARY -fork

# keyboard config
setxkbmap -option compose:caps
test -f $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# background and bar
$HOME/.fehbg
while true; do
    xsetroot -name "`date` `uptime|awk -F, '{print $2}'`"
    test -f /tmp/no_redshift || redshift -l 48.114722:-1.679444 -t5500:3000 -m randr -o
    sleep 15
done &

# currently playing music
current_music &

# MA editor registry
ma -registry &

exec dwm
