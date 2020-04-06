#!/bin/sh

# Remove and symlink existing configs
DOTFILE_REPO=~/.dotfiles

link_dotfile() {
    rm -rf $1
    ln -s $2 $1
}

for d in $DOTFILE_REPO/.config/*/; do
    link_dotfile ~/.config/$(basename $d) $d
done

for f in $DOTFILE_REPO/.*; do
    [ $(basename $f) != ".config" \
                     -a $(basename $f) != ".git" \
                     -a $(basename $f) != "." \
                     -a $(basename $f) != ".." ] && link_dotfile ~/$(basename $f) $f
done
