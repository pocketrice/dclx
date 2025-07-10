#!/usr/bin/env bash

for i in $(seq -f "%03g" 1 16)
do
  cargo run -- data/$i/HR_$i.csv > logs/HR_$i.log 
done
