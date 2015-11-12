#!/usr/bin/env bash

for file in `seq 1 $1`
do
    clear
    cat ./data/${file}.txt
    sleep 0.5
done