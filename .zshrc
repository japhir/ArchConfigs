# moo!
# fortune showerthoughts | cowthink

# reload pywal colorscheme
(cat /home/japhir/.cache/wal/sequences &)
# Path to your oh-my-zsh installation.
export ZSH=/usr/share/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
#ZSH_THEME="agnoster"
#ZSH_THEME="refined"
#ZSH_THEME="random"
ZSH_THEME="norm"
DEFAULT_USER="japhir"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(vi-mode)

# User configuration
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/japhir/.cabal/bin"
# export MANPATH="/usr/local/man:$MANPATH"
export BROWSER=firefox-beta

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='vim'
    export SUDO_EDITOR='vim'
    export VISUAL='vim'
    # export WINEPREFIX=$HOME/.win32
    # export WINARCH=win32
    # export PATH=$PATH/.cabal/bin
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias R='R --quiet'
alias o='xdg-open'
alias pb='curl -F c=@- https://ptpb.pw\?u\=1' # neat pastebin
# DO NOT DELETE LMAO
alias d="cd ~/Documents && ls -a"
alias D="cd ~/Downloads && ls -a"
alias pp="cd ~/Pictures && ls -a"
alias ch="cd ~/SurfDrive/PhD/chapters && ls -a"
alias c="cd ~/SurfDrive/PhD/conferences && ls -a"
alias cn="cd ~/SurfDrive/PhD/conferences/NAC_Veldhoven_2018 && ls -a"
alias r="cd ~/SurfDrive/PhD/courses && ls -a"
alias m="cd ~/SurfDrive/PhD/declarations && ls -a"
alias e="cd ~/SurfDrive/PhD/easotope && ls -a"
alias f="cd ~/SurfDrive/PhD/forms && ls -a"
alias g="cd ~/SurfDrive/PhD/grants && ls -a"
alias l="cd ~/SurfDrive/PhD/literature && ls -a"
alias lp="cd ~/Dropbox/MinorRP/LOSCAR/paper && ls -a"
alias pt="cd ~/SurfDrive/PhD/presentations && ls -a"
alias p="cd ~/SurfDrive/PhD/projects && ls -a"
alias pi="cd ~/SurfDrive/PhD/projects/iodp_read_info && ls -a"
alias pr="cd ~/SurfDrive/PhD/projects/isoread && ls -a"
alias petf="cd ~/SurfDrive/PhD/projects/long-term-etf && ls -a"
alias pn="cd ~/SurfDrive/PhD/projects/newfoundland && ls -a"
alias pc="cd ~/SurfDrive/PhD/projects/nico_cruise && ls -a"
alias ppd="cd ~/SurfDrive/PhD/projects/processdata && ls -a"
alias ps="cd ~/SurfDrive/PhD/projects/standardstats && ls -a"
alias pU="cd ~/SurfDrive/PhD/projects/urbino && ls -a"
alias pu="cd ~/SurfDrive/PhD/projects/utilities && ls -a"
alias pw="cd ~/SurfDrive/PhD/projects/walvisridge && ls -a"
alias pa="cd ~/SurfDrive/PhD/projects/washing && ls -a"
alias pZ="cd ~/SurfDrive/PhD/projects/zachos && ls -a"
alias r="cd ~/SurfDrive/PhD/reference_material && ls -a"
alias S="cd ~/SurfDrive/PhD/spreadsheets && ls -a"
alias s="cd ~/SurfDrive/PhD/students && ls -a"
alias sr="cd ~/SurfDrive/PhD/students/Robin_Vorsselmans && ls -a"
alias cff="vim ~/.config/Scripts/folders"
alias cfc="vim ~/.config/Scripts/configs"
alias cfb="vim ~/.bashrc"
alias cfz="vim ~/.zshrc"
alias cfv="vim ~/.vimrc"
alias cfr="vim ~/.config/ranger/rc.conf"
alias cfi="vim ~/.config/i3/config"
alias cfq="vim ~/.config/qutebrowser/config.py"
alias cfm="vim ~/.config/mutt/muttrc"
alias cft="vim ~/.config/termite/config"
alias eb="vim ~/Documents/LaTeX/uni.bib"
alias cv="vim ~/Documents/LaTeX/cv.tex"
alias cfp="vim ~/.config/polybar/config"
alias cfd="vim ~/.Xdefaults"
alias cfn="vim ~/.newsboat/urls"
alias cfA="vim ~/.asoundrc"
alias cfmb="vim ~/.ncmpcpp/bindings"
alias cfmc="vim ~/.ncmpcpp/config"
alias cfmpd="vim ~/.config/mpd/mpd.conf"
alias cfM="vim ~/.config/mpd/mpd.conf"
# DO NOT DELETE LMAO
