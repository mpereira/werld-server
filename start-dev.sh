#!/usr/bin/env sh

cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -mnesia dir '"db"' -s werld
