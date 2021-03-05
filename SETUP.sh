#!/bin/bash -x

mkdir -p ~/.emacs.d/lisp

###########
# Cleanup #
###########

rm -rf ~/.emacs.d/init.el
rm -rf ~/.emacs.d/myhydras.el
rm -rf ~/.emacs.d/custom.el
rm -rf ~/.emacs.d/custom-functions.el
rm -rf ~/.emacs.d/snippets
rm -rf ~/.emacs.d/blackhole.png
rm -rf ~/.emacs.d/meditate.png
rm -rf ~/.emacs.d/lisp/ox-mediawiki.el
rm -rf ~/.emacs.d/lisp/perltidy.el
rm -rf ~/.emacs.d/lisp/evil-markdown.el
rm -rf ~/.clang-format
rm -rf ~/.aliases
rm -rf ~/.bashrc
rm -rf ~/.perltidyrc
rm -rf ~/.bash_profile
rm -rf ~/.config/systemd/user/emacs.service

#################
# Link stuff up #
#################

cwd=`pwd`

ln -s $cwd/lisp/init.el ~/.emacs.d/init.el
ln -s $cwd/lisp/myhydras.el ~/.emacs.d/myhydras.el
ln -s $cwd/lisp/custom.el ~/.emacs.d/custom.el
ln -s $cwd/lisp/custom-functions.el ~/.emacs.d/custom-functions.el
ln -s $cwd/snippets ~/.emacs.d/snippets
ln -s $cwd/fluff/blackhole.png ~/.emacs.d/blackhole.png
ln -s $cwd/fluff/meditate.png ~/.emacs.d/meditate.png
ln -s $cwd/lisp/ox-mediawiki.el ~/.emacs.d/lisp/ox-mediawiki.el
ln -s $cwd/lisp/perltidy.el ~/.emacs.d/lisp/perltidy.el
ln -s $cwd/lisp/evil-markdown.el ~/.emacs.d/lisp/evil-markdown.el
ln -s $cwd/unix/.clang-format ~/.clang-format
ln -s $cwd/unix/.aliases ~/.aliases
ln -s $cwd/unix/.bashrc ~/.bashrc
ln -s $cwd/unix/.perltidyrc ~/.perltidyrc
ln -s $cwd/unix/.bash_profile ~/.bash_profile
ln -s $cwd/hacks/emacs.service ~/.config/systemd/user/emacs.service
