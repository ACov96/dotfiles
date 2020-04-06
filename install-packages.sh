#!/bin/sh

DOTFILES_REPO=~/.dotfiles

sudo pacman -S - < $DOTFILES_REPO/pacman.txt
yay -S - < $DOTFILES_REPO/yay.txt
pip install --user -r $DOTFILES_REPO/user_python_requirements.txt
