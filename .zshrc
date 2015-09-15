ZSH=/usr/share/oh-my-zsh
DISABLE_AUTO_UPDATE="true"
ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
ZSH_THEME="robbyrussell"
plugins=(git)
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi
source $ZSH/oh-my-zsh.sh

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt autocd
unsetopt beep
bindkey -e

zstyle :compinstall filename '/home/japhir/.zshrc'
autoload -Uz compinit
compinit

alias MRP='cd /mnt/HDD/Dropbox/UU/MS/Major\ Research\ Project/'
alias em='emacs -nw'
alias ls='ls --color'
export EDITOR='emacs -nw'

PATH=$PATH:~/.cabal/bin


