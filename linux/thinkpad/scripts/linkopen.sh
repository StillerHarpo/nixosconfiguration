#!/usr/bin/env bash
# a script to handle different links, when open from terminal
link=${1}
if echo "$link" | grep -e \)$ -e \)\.$
then
  link=$(echo "$link" | cut -d')' -f 1)
fi
if [ "${DISPLAY:-'UNDEFINED_VARIABLE'}" != 'UNDEFINED_VARIABLE' ]
then
  ./linkopenwithx.sh "${link}" 2>&1 /dev/null
else
  ./linkopenwithoutx.sh "${link}" 2>&1 /dev/null
fi

