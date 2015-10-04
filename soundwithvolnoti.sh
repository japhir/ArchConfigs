#!/bin/bash
# from https://bbs.archlinux.org/viewtopic.php?id=132218
# needs volnoti to be run on startup

VOLUME=$(pamixer --get-volume)
VALUE=5

case "$1" in
    "up")
	[[ "$VOLUME" -eq 100 ]] && VALUE=0
	pamixer --increase $VALUE
	;;
    "down")
	pamixer --decrease $VALUE
	;;
    "mute")
	pamixer --toggle-mute
	;;
esac

# notification
VOLUME=$(pamixer --get-volume)
MUTE=$(pamixer --get-mute)

if [ "$MUTE" == "false" ]; then
    volnoti-show $VOLUME
else
    volnoti-show -m $VOLUME
fi
