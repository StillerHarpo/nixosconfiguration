#!/usr/bin/env bash
my_find() {
    selected=$(fd -E '*.torrent' -E '*.aria2' -t f | sort | fzf --layout=reverse)
    if [ -f "$selected" ]; then
	mpv "$selected"
    fi
}
my_find_show() {
    selected=$(fd -E '*.torrent' -E '*.aria2' -d 1 | sort | fzf --layout=reverse)
    echo "Debug: " "$selected"
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
    res=$(printf "movies\nshows\nmusic\njellyfin\nexit" | fzf --layout=reverse)
    case $res in
	"movies")
	    cd /home/florian/server/movies || exit 1
	    my_find;;
	"shows")
	    cd /home/florian/server/shows || exit 1
	    my_find_show;;
	"music")
	    cd /home/florian/server/music || exit 1
	    my_find;;
	"jellyfin") jellytui;;
	"exit") exit;;
    esac
done
