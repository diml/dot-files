#!/bin/bash

cd config-files

for f in $(find . -type f); do
    dst=$HOME/$f
    mkdir -p $(dirname $dst)
    rm -f $dst
    ln -s $(readlink -f $PWD/$f) $dst
done

mkdir -p ~/.config/regolith/compton

rm -f ~/.config/regolith/compton/config
sed 's/.*no-fading-openclose.*/no-fading-openclose = true;/' \
    /etc/regolith/compton/config  \
    > ~/.config/regolith/compton/config
