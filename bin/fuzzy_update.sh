#!/bin/bash

while true; do
	fuzzy_time=$(fuzzy_clock)
	# current_time=$(date +%R)
	echo "{\"text\":\"$fuzzy_time\",\"alt\":\"$fuzzy_time\"}"
	sleep 5
done
