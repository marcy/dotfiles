#!/bin/bash

echo 'link maker start'

ln -s $(PWD)/../dot.emacs.d ~/.emacs.d

ln -s $(PWD)/Brewfile ~/Brewfile
ln -s $(PWD)/dot.gemrc ~/.gemrc
ln -s $(PWD)/dot.gitconfig ~/.gitconfig.global
ln -s $(PWD)/dot.gitignore_global ~/.gitignore_global
ln -s $(PWD)/dot.home.gitconfig ~/.gitconfig
ln -s $(PWD)/dot.irbrc ~/.irbrc
ln -s $(PWD)/dot.my.cnf ~/.my.cnf
ln -s $(PWD)/dot.rubocop.yml ~/.rubocop.yml
ln -s $(PWD)/dot.screenrc ~/.screenrc
ln -s $(PWD)/dot.tigrc ~/.tigrc

echo "done"
