#!/bin/bash

while true; do
	fuzzy_time=$(fuzzy_clock)
	current_time=$(date +%R)

	hour=${current_time:0:2}
	min=${current_time:3:2}

	# round hour to numbered nearest 12
	hour_num=$((10#$hour))
	hour_12=$(( (hour_num % 12) == 0 ? 12 : hour_num % 12))

	# round min to nearest half hour
	if (( min < 15 )); then
	    alt_text=$(printf "%02d:00" "$hour_12")
	elif (( min < 45 )); then
	    alt_text=$(printf "%02d:30" "$hour_12")
	else
	    next_hour=$(( (hour_12 % 12) + 1 ))
	    alt_text=$(printf "%02d:00" "$next_hour")
	fi

	echo "{\"text\":\"$fuzzy_time\",\"alt\":\"$alt_text\"}"
	sleep 60
done
