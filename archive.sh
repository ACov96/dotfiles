#!/bin/sh

# Archives existing dotfiles in ~/.local/.archive

ARCHIVE_PATH=~/.local/.archive
QUIET_ARCHIVE=false
DOTFILE_REPO=~/.dotfiles

msg() {
    [ QUIET_ARCHIVE == "false" ] && echo $1
}

archive_if_exists() {
    if [ -d $1 ]; then
        msg "Archiving directory $1"
        cp -r $1 $ARCHIVE_PATH
    elif [ -f $1 ]; then
        msg "Archiving dotfile $1"
        cp $1 $ARCHIVE_PATH
    fi
}

msg "Archiving..."

# Make archive folder
rm -rf $ARCHIVE_PATH
mkdir -p $ARCHIVE_PATH

# Folders in the .config directory need to be archived differently
for d in $DOTFILE_REPO/.config/*; do
    [ -d ~/.config/$(basename $d) ] && archive_if_exists ~/.config/$(basename $d)
done

for f in $DOTFILE_REPO/.*; do
    [ $(basename $f) != ".config" \
                     -a $(basename $f) != ".git" \
                     -a $(basename $f) != "." \
                     -a $(basename $f) != ".." ] && archive_if_exists ~/$(basename $f)
done
