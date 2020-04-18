[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
. ~/.profile
compton \
    -C \
    --backend glx \
    -f \
    -D 2 \
    --blur-background \
    --blur-background-fixed \
    --blur-method kawase \
    --blur-strength 5 &
xss-lock -- slock -m "$(cowsay "$(fortune)")" & 
wal -a 85 -i "$(ls ~/.wallpapers/*.{jpg,png,jpeg} 2>/dev/null | shuf -n 1)"
emacs --daemon &
dunst &
exec bspwm
