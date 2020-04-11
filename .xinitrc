[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
. ~/.profile
xcompmgr -c &
xss-lock -- slock -m "$(cowsay "$(fortune)")" & 
wal -a 85 -i ~/.wallpapers/bliss.jpg
emacs --daemon &
dunst &
exec bspwm
