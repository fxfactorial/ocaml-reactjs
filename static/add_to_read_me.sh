#!/bin/bash

set -e

example_to_readme () {
    local read_me=README.md
    printf '\n# %s\n' $(dirname $1) >> $read_me
    printf '\n```ocaml\n' >> $read_me
    cat $1 >> $read_me
    printf '```\n' >> $read_me
}

example_to_readme "$1"
