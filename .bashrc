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

function h { curl -s cheat.sh/$1?style=stata-light ;}

function vterm_printf { printf "\e]%s\e\\" "$1" ;}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear {
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    }
fi

alias ls='ls -AC --color=always --group-directories-first'
alias ll='ls -lhA --color=always --group-directories-first'
alias tree='tree -C'

alias cp='cp -rv'
alias mv='mv -v'
alias rm='rm -rfvI'
alias mkdir='mkdir -pv'

alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'

alias e="emacsclient -nw -a ''"
alias ek="emacsclient -e '(kill-emacs)'"

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
alias ping='ping -c 3 gnu.org'
alias rice='curl -sL http://git.io/rice'
alias wttr='curl -s wttr.in'
alias qttr='curl -s wttr.in/?0Q'
