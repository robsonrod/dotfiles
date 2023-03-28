#!/bin/env bash

ID1=$(wpctl status | awk '/ Built-in Audio Analog Stereo/ {sub(/.$/,"",$2); print $2 }' | head -n 1)
ID2=$(wpctl status | awk '/ S10 Bluetooth Speaker/ {sub(/.$/,"",$2); print $2 }' | sed -n 2p)

HEAD=$(wpctl status | grep "*" | awk '/ Built-in Audio Analog Stereo/ { print $2 }' | sed -n 2p)
SPEAK=$(wpctl status | grep "*"| awk '/ S10 Bluetooth Speaker/ { print $2 }' | head -n 1)

case $1 in
	"status")
		if [[ $HEAD = "*" ]]; then
			echo ''
		elif [[ $SPEAK = "*" ]]; then
			echo '>>>> 蓼'
		fi
	;;
	"toggle")
		if [[ $HEAD = "*" ]]; then
			wpctl set-default $ID2
		elif [[ $SPEAK = "*" ]]; then
			wpctl set-default $ID1
		fi
	;;
esac

