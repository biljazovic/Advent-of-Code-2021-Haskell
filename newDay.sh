#! /bin/sh
#
# newDay.sh
# Copyright (C) 2021 bruno <bruno@bruno-pc>
#
# Distributed under terms of the MIT license.
#

if [ -f "src/Day$1.hs" ]; then
  nvim src/Day$1.hs 
  exit
fi
cp template.hs src/Day$1.hs
sed -i "s/10/$1/g" src/Day$1.hs
sed -i "s/$(expr $1 - 1)/$1/g" Main.hs
nvim src/Day$1.hs
