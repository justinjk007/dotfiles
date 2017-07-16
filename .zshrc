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

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='emacs'
else
    export EDITOR='vi'
fi
# export SSH_KEY_PATH="~/.ssh/rsa_id"
source $HOME/.aliases
export JAVA_HOME="/etc/environment :$JAVA_HOME"
export PATH="/home/username/anaconda2/bin:$PATH"
export PATH=$PATH:/usr/local/go/bin
