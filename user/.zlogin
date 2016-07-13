alsactl -f ~/.alsa_state restore

pgrep ssh-agent || exec ssh-agent zsh -l
test `tty` = /dev/tty1 && exec xinit
