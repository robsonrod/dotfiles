#!/bin/sh

case $1 in
	"status")
  		MUTED=$(amixer sget Capture | awk -F"[][]" '/Left:/ { print $4 }')
		echo $MUTED
		if [ $MUTED = "on" ]; then
			echo ''
		else
  			echo ''
		fi
		;;
	"toggle")
		#ID=$(pacmd list-sources | grep "*\ index:" | cut -d' ' -f5)
		#pactl set-source-mute $ID toggle
		amixer set Capture toggle
		;;
esac
