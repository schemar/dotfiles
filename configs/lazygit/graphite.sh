#!/bin/bash

echo "l: list"
echo "o: checkout"
echo "u: up"
echo "d: down"
echo "t: top"
echo "b: bottom"
echo "c: create"
echo "m: modify"
echo "s: sync"
echo "p: submit"

# Disable echo and wait for one keypress
stty -echo
read -r -n 1 key
stty echo
echo ""

case "$key" in
  l)
    # Always doing gt ls at the end
    ;;
  o)
    gt checkout
    ;;
  u)
    gt up
    ;;
  d)
    gt down
    ;;
  t)
    gt top
    ;;
  b)
    gt bottom
    ;;
  c)
    gt create
    ;;
  m)
    gt modify
    ;;
  s)
    gt sync
    ;;
  p)
    gt submit
    ;;
  *)
    echo "Unknown command key: '$key'"
    ;;
esac

gt ls
