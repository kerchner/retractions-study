#!/bin/bash

NUM_RESULTS=2475637
for ((i=1; i<=$NUM_RESULTS; i+=10000))
do
  start_time=$(date +%s)
  echo Collecting $i through $((i+9999))...
  cat cancer_query.txt | efetch -start $i -stop $((i+9999)) -format uid > uids_$((i))_$((i+9999)).txt

  end_time=$(date +%s)

  # Calculate elapsed time
  elapsed=$(( end_time - start_time ))

  # Convert to minutes and seconds
  minutes=$(( elapsed / 60 ))
  seconds=$(( elapsed % 60 ))
  echo    efetch took $minutes minutes $seconds seconds
  echo 
done