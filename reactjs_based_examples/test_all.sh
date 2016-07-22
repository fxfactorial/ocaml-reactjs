#!/bin/bash

do_all() {
    for i in 'basic' 'basic-click-counter'; do
	printf '\tBuilding example: %s\n\n' ${i}
	make -C ${i}
    done
}

do_all
