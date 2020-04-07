export PATH=~/.scripts:~/.local/bin:$PATH
export LIBRARY_PATH=/usr/lib:/usr/local/lib:/usr/lib64:$HOME/.local/lib/
export EDITOR="emacs"
export TERMINAL='st'

if [ "$(tty)" == "/dev/tty1" -a  $(xset q >/dev/null 2>&1; echo $?) -eq 1 ]; then
    exec startx
fi

