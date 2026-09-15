#!/usr/bin/env bash
# a script to handle different links, when open from terminal in X or sway 
if [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]
then
    printf %s "${1}" | wl-copy
elif [[ "$XDG_SESSION_TYPE"  == "x11" ]]
then
    printf %s "${1}" | xsel -b
fi
color=$(cat ~/.var/bgcolor)
checkYoutubeStartpoint() {
   if [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]
   then
      maxRes=$(swaymsg -r -t get_outputs | jq 'map(select(.focused)).[0].current_mode.height')
   elif [[ "$XDG_SESSION_TYPE"  == "x11" ]]
   then
      maxRes=$(xrandr -q | grep "\*\+" | cut -d'x' -f2 | cut -d' ' -f1 | sort -n | tail -n1)
   fi
   if [ -z ${maxRes+x} ]
   then
       mpv --no-osc --ytdl-format="best[height<=${maxRes}]" "$@"
   else
       mpv --no-osc "$@"
   fi
}
if echo "${1}" | grep -q -e \.mp3$ -e \.mp4$ -e \.m4v$ -e v\.redd\.it
then
    mpv --really-quiet --no-osc -no-audio-display "${1}"
elif echo "${1}" | grep -q -e v\.redd\.it
then
    redirect=$(curl -s -I "${1}" | grep ^location: | cut -d" " -f2)
    mpv --really-quiet --no-osc -no-audio-display "${redirect}"
elif echo "${1}" | grep -q -e m\.youtube\. 
then
    checkYoutubeStartpoint "$@"
elif echo "${1}" | grep -q -e youtube\. 
then
    checkYoutubeStartpoint "$@"
elif echo "${1}" | grep -q -e youtu\.be
then
    checkYoutubeStartpoint "$@"
elif echo "${1}" | grep -q -e dailymotion\. -e streamable\.com -e liveleak\.com -e vimeo\.com
then
    mpv --really-quiet --no-osc "${1}"
elif echo "${1}" | grep -q -e "\.\(jpg\|JPG\|jpeg\|png\|PNG\)\(?[^\.]*\)\?$"
then
    if echo "${1}" | grep -q -e wikipedia.org # use the best quality of the picture
    then
        feh -. --image-bg "$color" "$(w3m -dump -o display_link_number=1 "${1}"| grep -q -e \.jpg$ -e \.JPG$ -e \.jpeg$ -e \.png$ | cut -c6- | grep -q -v /thumb/ | grep -q /commons/ | grep -q -e ^https)" || feh -. --image-bg "$color" "${1}"
    else
        feh -. --image-bg "$color" "${1}"
    fi
elif echo "${1}" | grep -q -e \.svg
then
    if echo "${1}" | grep -q -e wikipedia\.org
    then
        display "$(w3m -dump -o display_link_number=1 "${1}" | grep -q -e \.svg$ | cut -c6- | grep -q -v /thumb/ | grep -q /commons/ | grep -q -e ^https)"
    else
        display "${1}"
    fi
elif echo "${1}" | grep -q -e \.gif -e \.gifv$ 
then
    mpv --save-position-on-quit --really-quiet --loop --no-osc -no-audio-display "${1}"
elif echo "${1}"| grep -q -e \.pdf 
then
    DIR="/tmp/newpdfs"
    if [ ! -e $DIR ]
    then
        mkdir $DIR
        chmod 0755 $DIR
    else
        rm -f $DIR/*
    fi
    wget "${1}" -P "${DIR}"
    BILD="${1##*/}"
    zathura "$DIR"/"$BILD"
elif echo "${1}" | grep -q -e imgur\.com -e flickr\.com -e i\.reddit 
then
    you-get -p "feh -. --image-bg $color" "${1}"
elif echo "${1}" | grep -q -e gfycat.com 
then
    mpv --save-position-on-quit --really-quiet --loop --no-osc "$(you-get -u "${1}" | grep -q \.gif$ | head -1)"
elif echo "${1}" | grep -q -e medilyse -e factisresearch -e docs\.google -e meqo
then
    librewolf -P work --new-window "${1}"
else
    /run/wrappers/bin/sudo -En librewolf-airvpn "${1}"
fi || /run/wrappers/bin/sudo -En librewolf-airvpn "${1}"
