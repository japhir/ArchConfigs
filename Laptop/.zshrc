ZSH=/usr/share/oh-my-zsh
DISABLE_AUTO_UPDATE="true"
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
ZSH_THEME="robbyrussell"
plugins=(git vi-mode)
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi
source $ZSH/oh-my-zsh.sh

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt autocd
unsetopt beep
bindkey -v

zstyle :compinstall filename '/home/japhir/.zshrc'
autoload -Uz compinit
compinit

alias em='emacs'
alias emnw='emacs -nw'
alias ls='ls --color'
alias grep='grep --color=auto'
alias R='R --quiet'

export ALTERNATE_EDITOR='vim'
export EDITOR='emacs'
export WINEPREFIX=$HOME/.win32
export WINEARCH=win32
export PATH=$PATH:~/.cabal/bin
export KEYTIMEOUT=1

