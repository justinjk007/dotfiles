# Path to your oh-my-zsh installation.
export ZSH=/home/justin/.oh-my-zsh
export LANG=en_US.UTF-8
export UPDATE_ZSH_DAYS=30
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd/mm/yyyy"
ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh

export EDITOR='/usr/bin/emacs'
export VISUAL='/usr/bin/emacs'

source $HOME/.aliases

export DROPBOX_DIR="~/Dropbox"
