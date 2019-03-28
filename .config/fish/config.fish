set -x TERMINAL termite
set -x PATH /home/alex/.local/bin /home/alex/.local/pavolume $PATH
set -x GOPATH /home/alex/go

# Bob the Fish
set -x theme_display_date no
set -x theme_display_cmd_duration no
set -x theme_display_hostname yes
set -x theme_nerd_fonts no

# Start X at login
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
        exec startx -- -keeptty
    end
end
