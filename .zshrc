# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle :compinstall filename '/home/japhir/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh/history
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install

# manual changes
# overwrite beam in insert, I like it blocky everywhere
function zvm_config() {
    ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLOCK
}

# plugins/extensions
source /usr/share/zsh/plugins/zsh-vi-mode/zsh-vi-mode.plugin.zsh
# make sure fzf comes after vi-mode
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

# User configuration
typeset -U path PATH
path=(~/.cargo/bin ~/.local/bin ~/bin ~/SurfDrive/Postdoc1/prj/2024-10-28_HNBody/hnbody-1.0.10-linux-x86_64/bin $path)
export PATH

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR="emacsclient -t"
    export SUDO_EDITOR='vim'
    export VISUAL="emacsclient -cn"
fi
export ALTERNATE_EDITOR=""
# Compilation flags
# export ARCHFLAGS="-arch x86_64"
export JULIA_NUM_THREADS=auto

alias R='R --quiet --no-save --no-restore'
alias o='xdg-open'
alias e="emacsclient -t"
alias en="emacsclient -nw"
alias pb='curl -F c=@- https://ptpb.pw\?u\=1' # neat pastebin
alias pg="cd ~/SurfDrive/PhD/programming"
alias prj="cd ~/SurfDrive/Postdoc1/prj"
alias pacsize="expac -H M '%m\t%n' | sort -h"
alias killorphans="sudo pacman -Rnsc $(pacman -Qtdq)"
alias pi="pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'"

alias ssh="TERM=xterm-256color ssh"
alias der="ssh -Y derecho"

# oxidize
alias ls="eza --icons -a --group-directories-first"
# alias cat="bat"
# alias cd="z"
alias zz="z -"
alias weather='curl "wttr.in/Utrecht?format=v2"'
alias fetch="fastfetch"
alias neofetch="fastfetch"

eval "$(zoxide init zsh)"
# my new custom prompt
eval "$(starship init zsh)"

# countdown in seconds
# useful to countdown block_distractions
function countdown(){
   date1=$((`date +%s` + $1));
   while [ "$date1" -ge `date +%s` ]; do
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
     sleep 0.1
   done
}

# the remainder is vterm stuff for emacs!
# https://github.com/akermu/emacs-libvterm/blob/master/etc/emacs-vterm-zsh.sh
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# enable clearing the full buffer from emacs vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# escape elisp directly in vterm
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# change the title of the buffer based on information provided by the shell.
# See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the various
# symbols.
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# communicate prompt location to vterm
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@worktop:$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

# open yazi with y, then cd to final directory before quit
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
	rm -f -- "$tmp"
}

# moved to end of file so that above config is available within sway maybe?
# start sway if on tty1
if [ "$(tty)" = "/dev/tty1" ]; then
    #sway
    hyprland
    exit 0
fi
