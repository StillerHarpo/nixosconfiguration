#!/usr/bin/env bash
# a script to handle different links, when open from terminal without X
if echo "$1" | grep -e \.mp3$ -e \.mp4$ -e \.m4v$
then
    mpv --save-position-on-quit --vo=drm --no-osc -no-audio-display "$1"
elif echo "$1" | grep -e youtube
then
    mpv --save-position-on-quit --vo=drm --no-osc "$1"
elif echo "$1" | grep -e youtu.be
then
    LINK="$(curl -Is "$1" | sed -n 's#Location: ##p' )"
    echo "$LINK"
    mpv --save-position-on-quit --vo=drm --no-osc "$LINK"
elif echo "$1" | grep -e \.jpg$ -e \.JPG$ -e \.jpeg$ -e \.png$
then
    DIR="/tmp/newspics"
    if [ ! -e $DIR ]
    then
        mkdir $DIR
        chmod 0755 $DIR
    else
        rm -f $DIR/*
    fi
    DOWN="$1 -P $DIR"
    wget "$DOWN"
    BILD="${1##*/}"
    fbv "$DIR"/"$BILD"
elif echo "$1"| grep -e \.gif$
then
    DIR="/tmp/newgifs"
    if [ ! -e $DIR ]
    then
        mkdir $DIR
        chmod 0755 $DIR
    else
        rm -f $DIR/*
    fi
    DOWN="$1 -P $DIR"
    wget "$DOWN"
    BILD="${1##*/}"
    mpv --save-position-on-quit --vo=drm --loop -a "$DIR"/"$BILD"
else
    w3m "$1"
fi

