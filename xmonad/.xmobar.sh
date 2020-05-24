#!/usr/bin/env bash

source $HOME/.cache/wal/colors.sh
sed -i $HOME/.xmobarrc -e "s/bgColor = \".*\"/bgColor = \"$background\"/"
sed -i $HOME/.xmobarrc -e "s/fgColor = \".*\"/fgColor = \"$foreground\"/"
