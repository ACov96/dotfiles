[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
. ~/.profile
xcompmgr -c &
xss-lock -- slock -m "$(cowsay "$(fortune)")" & 
# xwinwrap -ov -g 1366x768+0+0 -- gifview -w WID /home/alex/.wallpapers/ds.gif -a &
wal -a 85 -i ~/.wallpapers/bliss.jpg
slstatus &
emacs --daemon
exec bspwm
