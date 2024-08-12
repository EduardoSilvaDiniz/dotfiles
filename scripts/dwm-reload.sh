#!/bin/env bash

dirs=( "dwm" "dwmblocks" )
cd ~/.config/suckless/

for a in ${dirs[@]}; do 
	cd $a
	rm -rf .config.h .blocks.h
	sudo make clean install
	cd ~/.config/suckless/
done


