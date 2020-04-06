#!/bin/sh

DOTFILES_REPO=~/.dotfiles

pacman -Qqen > $DOTFILES_REPO/pacman.txt
yay -Qqen > $DOTFILES_REPO/yay.txt
pip freeze > $DOTFILES_REPO/user_python_requirements.txt
