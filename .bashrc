shopt -s histappend

HISTCONTROL="ignoreboth:erasedups"
HISTFILESIZE=10000
HISTSIZE=$HISTFILESIZE
HISTFILE="$HOME/.history"

PS1="\[\e[35;1m\]\w $ \[\e[0m\]"

alias ls='ls -AC --color=auto --group-directories-first'
alias ll='ls -lhA --color=auto --group-directories-first'
alias tree='tree -C'

alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -rfvI'
alias mkdir='mkdir -pv'

alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'

alias e="emacsclient -nwa ''"
alias ed="emacs --daemon"
alias edk="emacsclient -e '(kill-emacs)'"

alias reboot="sudo reboot"
alias poweroff="sudo poweroff"

alias ping='ping -c 3 gnu.org'
alias wttr='curl -s wttr.in'
alias qttr='curl -s wttr.in/?0Q'
alias which='command -v'
alias free='top -d1 | head -n5'
