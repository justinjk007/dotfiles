#!/bin/bash

copy_verbose () {
    echo "--- Copying     $1"
    cp -rf $1 $2
}

copy_verbose init.el ~/.emacs.d/
copy_verbose lisp/myhydras.el ~/.emacs.d/
copy_verbose lisp/custom.el ~/.emacs.d/
copy_verbose fluff/blackhole.png ~/.emacs.d/
copy_verbose lisp/custom-functions.el ~/.emacs.d/
copy_verbose lisp/ox-mediawiki.el ~/.emacs.d/ox-mediawiki.el
copy_verbose .clang-format ~/
copy_verbose snippets ~/.emacs.d/
copy_verbose unix/.aliases ~/
copy_verbose unix/.bashrc ~/
copy_verbose unix/.bash_profile ~/
