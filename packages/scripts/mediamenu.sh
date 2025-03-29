#!/usr/bin/env bash
my_find() {
    selected=$(fd -E '*.torrent' -E '*.aria2' -t f | sort | fzf --layout=reverse)
    if [ -f "$selected" ]; then
	mpv "$selected"
    fi
}
my_find_show() {
    selected=$(fd -E '*.torrent' -E '*.aria2' -d 1 | sort | fzf --layout=reverse)
    if [ -d "$selected" ]; then
	cd "$selected" || exit
	my_find_show
    fi
    if [ -f "$selected" ]; then
	mpv "$selected"
        my_find_show
    fi
    if [ "$(pwd)" =  /home/florian/server/shows ]; then
	return
    fi
    cd ..
    my_find_show
}
while true; do
    res=$(printf "movies\nshows\njellyfin\njellyfin(t)\nexit" | fzf --layout=reverse)
    case $res in
	"movies")
	    cd /home/florian/movies || exit 1
	    my_find;;
	"shows")
	    cd /home/florian/shows || exit 1
	    my_find_show;;
	"jellyfin") @jellytui@;;
	"jellyfin(t)") @jellytui@ -b /home/florian/.config/jellytui-t/;;
	"exit") exit;;
    esac
done
