#!/bin/sh

DOTFILES_REPO=~/.dotfiles

install_yay() {
    git clone https://aur.archlinux.org/yay.git ~/.local
    cd ~/.local/yay
    makepkg -si
}

# Update the system and make sure Yay is installed
sudo pacman -Syu
command -v yay >/dev/null 2>&1 || install_yay
yay -Syu

sudo pacman -S - < $DOTFILES_REPO/pacman.txt
yay -S - < $DOTFILES_REPO/yay.txt
pip install --user -r $DOTFILES_REPO/user_python_requirements.txt
