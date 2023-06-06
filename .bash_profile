export LANG=en_US.UTF-8
export GDK_CORE_DEVICE_EVENTS=1

export XDG_RUNTIME_DIR=/tmp/$(id -u)
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/settings.ini"

eval "$(guix package --search-paths \
-p $HOME/.config/guix/current \
-p $HOME/.guix-profile \
-p $HOME/.guix-home/profile \
-p /run/current-system/profile)"

export PATH=/run/setuid-programs:$PATH
export PATH="$HOME/.local/bin:$PATH"

#source $HOME/.profile

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
export BROWSER=chromium
export GPG_TTY=$(tty)

eval "$(dircolors)"

[ -f "$HOME/.bashrc" ] && source $HOME/.bashrc
[ "$(tty)" = "/dev/tty1" ] && sx
