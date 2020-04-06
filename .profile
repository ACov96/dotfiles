export PATH=~/.scripts:~/.local/bin:$PATH
export LIBRARY_PATH=/usr/lib:/usr/local/lib:/usr/lib64:$HOME/.local/lib/
export EDITOR="emacs"
export TERMINAL='st'

if ! xset q &>/dev/null; then
	exec startx
fi
