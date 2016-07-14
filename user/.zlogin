alsactl -f ~/.alsa_state restore

pgrep ssh-agent || exec ssh-agent $SHELL -l
test `tty` = /dev/tty1 && exec xinit
