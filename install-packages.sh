#!/bin/sh

DOTFILES_REPO=~/.dotfiles

# Update the system and make sure Yay is installed
sudo pacman -Syu
git clone https://aur.archlinux.org/yay.git ~/.local
cd ~/.local/yay
makepkg -si
yay -Syu

sudo pacman -S - < $DOTFILES_REPO/pacman.txt
yay -S - < $DOTFILES_REPO/yay.txt
pip install --user -r $DOTFILES_REPO/user_python_requirements.txt
