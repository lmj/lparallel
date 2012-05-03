#!/bin/bash

govs=`cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors`

echo "available governors:"
echo $govs
echo

echo "current governor on each CPU:"
cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
echo

valid="false"

for gov in $govs ; do
    if [ "$1" = "$gov" ] ; then
	valid="true"
    fi
done

if [ "$valid" = "true" ] ; then
    echo "changing to $1..."
    echo
    for cpu in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor ; do
	[ -f $cpu ] || continue
	echo -n $1 > $cpu
    done
    echo "new governor on each CPU:"
    cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
elif [ "$1" != "" ] ; then
    echo "invalid governor: $1"
fi
