shopt -s histappend

HISTCONTROL="ignoreboth:erasedups"
HISTFILESIZE=10000
HISTSIZE=$HISTFILESIZE
HISTFILE="$HOME/.history"

PS1="[\w] "

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
       && [[ -n ${EMACS_VTERM_PATH} ]] \
       && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

alias xi='sudo xbps-install'
alias xr='sudo xbps-remove -Ro'
alias xu='sudo xbps-install -Su'
alias xqs='xbps-query -Rs'
alias xf='xlocate'

alias reboot="sudo reboot"
alias poweroff="sudo poweroff"

alias ls='ls -AC --color=auto --group-directories-first'
alias ll='ls -AgGh --color=auto --group-directories-first'
alias tree='tree -C'

alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -rfvI'
alias mkdir='mkdir -pv'

alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'
alias zgrep='zgrep --color=always'

alias e="emacsclient -nwa ''"
alias ed="emacs --daemon"
alias edk="emacsclient -e '(kill-emacs)'"
alias mg='mg -n'

alias ping='ping -c 3 gnu.org'
alias wttr='curl -s wttr.in'
alias qttr='curl -s wttr.in/?0Q'
alias which='command -v'
alias free='free -h'
alias mime='file -b --mime-type'
