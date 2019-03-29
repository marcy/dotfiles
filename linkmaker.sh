#!/bin/bash

echo 'link maker start'

ln -fns $(PWD)/.config ~/.config
ln -fns $(PWD)/.emacs.d ~/.emacs.d
ln -fns $(PWD)/.hammerspoon ~/.hammerspoon

ln -fs $(PWD)/.gemrc ~/.gemrc
ln -fs $(PWD)/.gitconfig_global ~/.gitconfig_global
ln -fs $(PWD)/.irbrc ~/.irbrc
ln -fs $(PWD)/.my.cnf ~/.my.cnf
ln -fs $(PWD)/.screenrc ~/.screenrc
ln -fs $(PWD)/.tigrc ~/.tigrc
ln -fs $(PWD)/.zshrc ~/.zshrc

echo "done"
