#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

execute_server() {
    erl -name server -pa ./ebin ${CECHO_EBIN} -eval "application:start(pongerl)" -setcookie pongerl
}

execute_client() {
    profile=$(gconftool --get /apps/gnome-terminal/global/default_profile)
    old_font=$(gconftool --get /apps/gnome-terminal/profiles/${profile}/font)
    profileAtom="'"${profile}"'"
    font="\""$old_font"\""
    gconftool --set /apps/gnome-terminal/profiles/${profile}/font --type string "Monospace 8"
    # change the server host for the one you are using
    erl -noinput -pa ./ebin ${CECHO_EBIN} -name $1 -setcookie pongerl -eval "pongerl_client:start($profileAtom, $font, server@ardilla.lan)" +A 200
}


if [ $# -eq 0 ]; then
    execute_server
else
    opt=$(echo $1 | sed 's/=.*//')
    case $opt in
	"--server")
	    execute_server
	    ;;
	"--c1")
	    execute_client "c1"
	    ;;
	"--c2")
	    execute_client "c2"
	    ;;
    esac
fi

