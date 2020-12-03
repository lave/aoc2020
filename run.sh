#!/bin/sh

unset _JAVA_OPTIONS
clojure-1.10 -i ${1:?Clojure file must be specified as the first argumen} -m aoc
