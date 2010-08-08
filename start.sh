#!/usr/bin/env sh
cd `dirname $0`

. ./dep.inc

echo "Starting Pongerl..."
erl \
    -sname ${NAME} \
    -pa ./ebin ${CECHO_EBIN} \
    -eval "application:start(pongerl)"