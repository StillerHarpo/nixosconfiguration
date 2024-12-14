#!/usr/bin/env bash
# a script to handle different links, when open from terminal
link=${1}
date > /tmp/linkopen.log
echo "$link" >> /tmp/linkopen.log
if echo "$link" | grep -e \)$ -e \)\.$
then
  link=$(echo "$link" | cut -d')' -f 1)
fi
if [ "${DISPLAY:-'UNDEFINED_VARIABLE'}" != 'UNDEFINED_VARIABLE' ]
then
  ./linkopenwithx.sh "${link}" >> /tmp/linkopen.log 2>&1
else
  ./linkopenwithoutx.sh "${link}" >> /tmp/linkope.log 2>&1
fi

