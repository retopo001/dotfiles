#!/bin/sh

# Environment
export XDG_SESSION_TYPE=x11
export XDG_CURRENT_DESKTOP=i3
export XDG_SESSION_DESKTOP=i3

# Force X11 for apps that might try Wayland
export GDK_BACKEND=x11
export QT_QPA_PLATFORM=xcb
export ELECTRON_OZONE_PLATFORM_HINT=x11

# XCompose support for GTK/Qt
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS=@im=none

# Load X resources
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# Set cursor
xsetroot -cursor_name left_ptr

# Compose key (CapsLock)
setxkbmap -option compose:caps

# Start audio (pipewire)
pipewire &
pipewire-pulse &
wireplumber &

# Start i3 (handles the rest via autostart)
exec i3
