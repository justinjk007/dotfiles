#!/bin/bash

copy_verbose () {
    echo "--- Copying     $1"
    cp -rf $1 $2
}


copy_verbose ./init.el ~/.emacs.d/
copy_verbose lisp/myhydras.el ~/.emacs.d/
copy_verbose lisp/custom.el ~/.emacs.d/
copy_verbose fluff/blackhole.png ~/.emacs.d/
copy_verbose lisp/custom-functions.el ~/.emacs.d/
copy_verbose unix/.clang-format ~/
copy_verbose snippets ~/.emacs.d/

# Lisp not on melpa
mkdir -p ~/.emacs.d/lisp
copy_verbose lisp/ox-mediawiki.el ~/.emacs.d/lisp/ox-mediawiki.el
copy_verbose lisp/perltidy.el ~/.emacs.d/lisp/perltidy.el
copy_verbose lisp/evil-markdown.el ~/.emacs.d/lisp/evil-markdown.el

# Shell stuff
copy_verbose unix/.aliases ~/
copy_verbose unix/.bashrc ~/
copy_verbose unix/.perltidyrc ~/
copy_verbose unix/.bash_profile ~/
copy_verbose hacks/emacs.service ~/.config/systemd/user/emacs.service
