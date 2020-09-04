#! /bin/bash
# .bashrc is executed for interactive, login shells

if [[ "$SHELL" != *fish && -x "$HOME/.nix-profile/bin/fish" ]]
then
    exec "$HOME/.nix-profile/bin/fish"
elif [[ "$SHELL" != *fish ]] && command -v fish > /dev/null
then
    exec fish
fi

# Source global definitions
if [ -f /etc/bashrc ]
then
	. /etc/bashrc
fi

if [[ $(uname) == "Darwin" ]]
then
	alias ls='ls -G'
else
	export LS_OPTIONS="--color=auto"
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

if command -v dircolors > /dev/null
then
	eval "$(dircolors)"
fi

export LC_CTYPE=en_US.UTF-8

alias alia_shell='sudo bash -c "source ~/ssh-agent-data && $(which fish)"'

complete -C /usr/local/bin/vault vault
