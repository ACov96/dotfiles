#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. .profile
wal -R -q

alias ls='ls --color=auto'
alias colors='wal --preview'
export PS1="\u@[1;32m\h[m: \w\`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'\`\n\\$ "
