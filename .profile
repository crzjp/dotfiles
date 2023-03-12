export LANG=en_US.UTF-8
export QT_QPA_PLATFORMTHEME=qt5ct
export GDK_CORE_DEVICE_EVENTS=1

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export CARGO_HOME="$XDG_DATA_HOME/cargo"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:$CARGO_HOME/bin"

export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/settings.ini"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RXVT_SOCKET="/tmp/urxvt-socket"
export XAUTHORITY="/tmp/Xauthority"

export LESS='-FRJMWX'
export LESSHISTFILE=/dev/null
export LESS_TERMCAP_mb=$'\033[1;31m'
export LESS_TERMCAP_md=$'\033[1;36m'
export LESS_TERMCAP_me=$'\033[0m'
export LESS_TERMCAP_so=$'\033[01;7;34m'
export LESS_TERMCAP_se=$'\033[0m'
export LESS_TERMCAP_us=$'\033[1;32m'
export LESS_TERMCAP_ue=$'\033[0m'
export PAGER=less
export MANPAGER=less

export VISUAL="emacsclient -c -a ''"
export EDITOR="emacsclient -nw -a ''"
export BROWSER=chrome
export GPG_TTY=$(tty)

eval "$(dircolors)"

[ -f "$HOME/.bashrc" ] && source $HOME/.bashrc
[ "$(tty)" = "/dev/tty1" ] && startx > $XDG_DATA_HOME/xorg/${USER}-xorg.log 2>&1
