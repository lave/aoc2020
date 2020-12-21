#!/bin/sh

unset _JAVA_OPTIONS
clojure -i ${1:?Clojure file must be specified as the first argumen} -m aoc
