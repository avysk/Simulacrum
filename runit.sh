#!/bin/bash
for ((i = 1; i <= $1; i++))
do
	echo "Step $i"
	./simulacrum >> /tmp/results
done
