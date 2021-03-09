#!/bin/bash -x

if [[ $1 == 'copy' ]]; then
    function setup_link() {
	rm -rf $2
	cp -r $1 $2
    }
else
    function setup_link() {
	rm -rf $2
	ln -s $1 $2
    }
fi

#################
# Link stuff up #
#################

cwd=`pwd`

mkdir -p ~/.emacs.d/lisp

setup_link $cwd/lisp/init.el ~/.emacs.d/init.el
setup_link $cwd/lisp/myhydras.el ~/.emacs.d/myhydras.el
setup_link $cwd/lisp/custom.el ~/.emacs.d/custom.el
setup_link $cwd/lisp/custom-functions.el ~/.emacs.d/custom-functions.el
setup_link $cwd/snippets ~/.emacs.d/snippets
setup_link $cwd/fluff/blackhole.png ~/.emacs.d/blackhole.png
setup_link $cwd/fluff/meditate.png ~/.emacs.d/meditate.png
setup_link $cwd/lisp/ox-mediawiki.el ~/.emacs.d/lisp/ox-mediawiki.el
setup_link $cwd/lisp/perltidy.el ~/.emacs.d/lisp/perltidy.el
setup_link $cwd/lisp/evil-markdown.el ~/.emacs.d/lisp/evil-markdown.el
setup_link $cwd/unix/.clang-format ~/.clang-format
setup_link $cwd/unix/.aliases ~/.aliases
setup_link $cwd/unix/.bashrc ~/.bashrc
setup_link $cwd/unix/.perltidyrc ~/.perltidyrc
setup_link $cwd/unix/.bash_profile ~/.bash_profile
setup_link $cwd/hacks/emacs.service ~/.config/systemd/user/emacs.service
