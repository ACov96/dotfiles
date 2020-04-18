[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
. ~/.profile
xcompmgr -C &
xss-lock -- slock -m "$(cowsay "$(fortune)")" & 
wal -a 85 -i "$(ls ~/.wallpapers/*.{jpg,png,jpeg} 2>/dev/null | shuf -n 1)"
emacs --daemon &
dunst &
exec bspwm
