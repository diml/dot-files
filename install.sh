#!/bin/bash

cd config-files

for f in $(find . -type f); do
    dst=$HOME/$f
    mkdir -p $(dirname $dst)
    rm -f $dst
    ln -s $(readlink -f $PWD/$f) $dst
done
