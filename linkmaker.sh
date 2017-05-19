#!/bin/bash

echo 'link maker start'

ln -fns $(PWD)/dot.config ~/.config
ln -fns $(PWD)/dot.emacs.d ~/.emacs.d

ln -fs $(PWD)/dot.gemrc ~/.gemrc
ln -fs $(PWD)/dot.gitconfig_global ~/.gitconfig_global
ln -fs $(PWD)/dot.irbrc ~/.irbrc
ln -fs $(PWD)/dot.my.cnf ~/.my.cnf
ln -fs $(PWD)/dot.screenrc ~/.screenrc
ln -fs $(PWD)/dot.tigrc ~/.tigrc
ln -fs $(PWD)/dot.zshrc ~/.zshrc

echo "done"
