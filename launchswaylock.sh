#!/bin/bash
grim /tmp/screen_locked.png
convert /tmp/screen_locked.png -scale 8% -scale 1250% /tmp/screen_locked2.png
swaylock -i /tmp/screen_locked2.png -s tile
