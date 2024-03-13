#!/bin/bash
set -euo pipefail

PROG="${1:-"./1brc.ros"}"

COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[0;32m'
COLOR_RESET='\033[0m'

function passfail () {
  expected="$1"
  actual="$2"
  if [[ "$(< "$expected")" != "$actual" ]]; then
    printf "${COLOR_RED}FAIL${COLOR_RESET}"
  else
    printf "${COLOR_GREEN}PASS${COLOR_RESET}"
  fi
}

function test () {
  start="$(date +%s)"
  result="$("$PROG" "$1")"
  end="$(date +%s)"
  status="$(passfail "$2" "$result")"
  echo $status $1 completed in $((end - start)) seconds
}

ls -1 test/*.txt | while read line; do
  test "$line" "$(echo "$line" | cut -f1 -d\.).out"
done
