#!/bin/bash

until /usr/bin/emacsclient -a false -e 't' &> /dev/null
do
    echo "waiting for emacsclient to start up"
    sleep 1
done

/usr/bin/emacsclient -n -c \
 "~/org/clumpednotes.org" \
 --frame-parameters '((name . "TODO"))'
