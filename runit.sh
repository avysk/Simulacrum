#!/bin/bash
for i in `seq 1 $1`; do echo "Step $i"; ./simulacrum >> /tmp/results; done
