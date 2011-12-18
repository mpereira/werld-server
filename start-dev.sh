#!/usr/bin/env sh

cd `dirname $0`
exec erl -pa ./ebin -s werld
