
#Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="af-magic"
#ZSH_THEME="bira"
#ZSH_THEME="dst"
ZSH_THEME="mh"
DEFAULT_USER="fenstamaker"
#export JAVA_HOME=/usr

# Example aliases
alias zshconfig="emacs ~/.zshrc"
alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
alias ssh_aws="ssh -i ~/.ssh/fenstamaker-aws.pem ubuntu@ec2-184-73-239-94.compute-1.amazonaws.com"

# EDITORS
alias sublime="st"
#alias emx="emacsclient -t"
#alias em="emacsclient -c -a emacs"

export ALTERNATE_EDITOR=""
export GIT_EDITOR="vim"
export EDITOR="emacs"
export VISUAL="emacs"

# alias ohmyzsh="mate ~/.oh-my-zsh"

# ssh aliases
alias ssh_hss1="ssh -i ~/.ssh/highschoolsports.pem ec2-user@10.128.136.40"
alias ssh_hss2="ssh -i ~/.ssh/highschoolsports.pem ec2-user@10.128.136.150"
alias ssh_ds="ssh ds.wpprivate.com"
alias ssh_clavis="ssh -i ~/.ssh/Clavis.pem ec2-user@10.128.136.175"
alias ssh_clavis2="ssh -i ~/.ssh/Clavis.pem ec2-user@10.128.132.213"
alias start_mongo="mongod --config /usr/local/etc/mongod.conf"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

DISABLE_CORRECTION="true"

plugins=(git history history-substring-search terminalapp sublime osx bower brew lein npm)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=~/Developer:/usr/local/go/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:~/Developer/maven/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/Developer/adt/sdk/platform-tools:~/Developer/adt/sdk/tools:/usr/local/mysql/bin:/usr/local/sbin

#source /usr/local/bin/virtualenvwrapper.sh
source ~/.oh-my-zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#eval "$(rbenv init -)"
