#!/usr/bin/env sh
# Packages needs
# redshift dusnt brightnessctl feh xcompmgr xorg-server xorg-xinit xorg-xrandr xorg-xsetroot base-devel

dirs=("dwm", "dwmblocks", "st", "dmenu")
dirGitHome="/home/edu/temp/universal-linux/dotfiles/home"

ln -s ~/Sync/home/.config/suckless ~/.config/
ln -s ~/Sync/home/.dwm/ ~/

for dir in ${dirs[@]}; do
    cd ~/.config/suckless/$dir
    make clean install
done && echo "dwm install"

cd ~/
mkdir temp && cd temp && git clone https://github.com/EduardoSilvaDiniz/universal-linux.git && \
mv universal-linux/dotfiles/etc/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/  && \
mv $dirGitHome/.aliases $dirGitHome/.zshenv ~/ &&  printf "if [-f ~/.aliases ]; then\n  source ~/.aliases;/nfi\n" \
						>> ~/.zshrc && echo 'install zsh and move configs for home' && sleep 2
cd ~/ && rm -rf temp && echo 'clean files...'
