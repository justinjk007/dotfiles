#!/bin/bash

copy_verbose () {
    echo "--- Copying     $1"
    cp -rf $1 $2
}

copy_verbose init.el ~/.emacs.d/
copy_verbose lisp/myhydras.el ~/.emacs.d/
copy_verbose lisp/custom.el ~/.emacs.d/
copy_verbose lisp/custom-functions.el ~/.emacs.d/
copy_verbose lisp/ox-mediawiki.el ~/.emacs.d/ox-mediawiki.el
copy_verbose .ycm_extra_conf.py ~/
copy_verbose .clang-format ~/
copy_verbose snippets ~/.emacs.d/
copy_verbose linux/.aliases ~/
copy_verbose linux/.bashrc ~/
copy_verbose linux/.bash_profile ~/
