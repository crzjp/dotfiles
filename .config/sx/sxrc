#!/bin/sh

xhost +SI:localuser:$USER
xrandr --output eDP-1 --mode 1280x720
exec dbus-run-session -- stumpwm
