[[ $- != *i* ]] && exit

set -o emacs

stty -ixon -ixoff

HISTFILE="$HOME/.bash_history"
HISTSIZE=1000
HISTCONTROL=ignoredups:erasedups
shopt -s histappend

shopt -s checkwinsize
shopt -s no_empty_cmd_completion
shopt -s autocd

PS1="\[\e[1;7;34m\] \w \[\e[0m\] "

function repeat {
    while :; do
        "$@"
    done
}

function trash {
    for file in "$@"; do
        mv -v "$file" /tmp
    done
}

function ls {
    if [ $TERM == "eterm-color" ] || [ $TERM == "dumb" ] ; then
        command ls -AC --color=always --group-directories-first "$@"
    else
        export COLUMNS
        command ls -AC --color=always --group-directories-first "$@" | less
    fi
}

function ll {
    if [ $TERM == "eterm-color" ] || [ $TERM == "dumb" ] ; then
        command ls -lhA --color=always --group-directories-first "$@"
    else
        command ls -lhA --color=always --group-directories-first "$@" | less
    fi
}

alias cp='cp -rv'
alias mv='mv -v'
alias rm='rm -rfvI'
alias mkdir='mkdir -pv'

alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'

alias e="emacsclient -nw -a ''"

alias reboot="sudo reboot"
alias poweroff="sudo poweroff"

alias pi='sudo pacman -S'
alias ps='pacman -Ss'
alias pr='sudo pacman -Rsn'
alias pu='sudo pacman -Syu && sudo pacman -Fy && paru -Sua'
alias pc='sudo pacman -Rsn $(pacman -Qtdq)'
alias pf='pacman -F'
alias ai='paru -S'
alias as='paru -Ss'
alias ar='paru -Rsn'

alias cat='cat -n'
alias cpu="\ps -A --sort -rsz -o pid,comm,pmem,pcpu | awk 'NR<=20'"
alias ping="ping -c 3 gnu.org"
alias rice="curl -L http://git.io/rice"
alias wttr="curl http://wttr.in"
