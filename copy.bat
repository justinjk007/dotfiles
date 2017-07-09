@echo off
copy init.el %home%\.emacs.d\init.el
copy .gitconfig %home%\.gitconfig
copy .clang_complete %home%\.clang_complete
copy custom.el %home%\.emacs.d\custom.el
copy custom-functions.el %home%\.emacs.d\custom-functions.el
xcopy snippets %home%\.emacs.d\snippets
xcopy lisp %home%\.emacs.d\lisp
