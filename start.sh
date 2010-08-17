#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

execute_server() {
    erl -sname server -pa ./ebin ${CECHO_EBIN} -eval "application:start(pongerl)" -setcookie pongerl
}

execute_client() {
    erl -noinput -pa ./ebin ${CECHO_EBIN} -sname $1 -setcookie pongerl -eval 'pongerl_client:start()' +A 200
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

