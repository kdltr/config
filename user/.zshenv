# Honor system-wide environment variables
source /etc/profile

export XDG_CACHE_HOME=/tmp/cache.$UID
mkdir -p $XDG_CACHE_HOME

export GUIX_LD_WRAPPER_ALLOW_IMPURITIES=$HOME/.local

export MPD_HOST=192.168.0.50

export CHICKEN_DOC_REPOSITORY=$HOME/.local/share/chicken/doc
export CHICKEN_DOC_COLORS=auto
export CHICKEN_DOC_WRAP=76

export PATH=$HOME/bin:$HOME/code/config/scripts:$HOME/.local/bin:$PATH

if [ `hostname` = "stitch" ]; then
    export ALSA_CARD=PCH
fi
