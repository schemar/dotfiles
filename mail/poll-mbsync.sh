#!/bin/bash

# Endless loop to keep polling
while :
do
  echo "$(date) polling..."
  mbsync -a

  echo "$(date) sleeping..."
  # 10 minutes
  sleep 600
done
