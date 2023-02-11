#!/bin/sh

#numlockx on
#xsetroot -cursor_name left_ptr
#xset r rate 200 70
#xrdb -load $HOME/.Xresources
#hsetroot -fill $HOME/.dotfiles/wallpaper.jpg

exec dbus-launch --exit-with-session i3
#exec dbus-launch --exit-with-session awesome
