@echo off

rem Xcopy <Source> [<Destination>] [/w]
rem Find more here --> https://msdn.microsoft.com/en-us/library/cc771254.aspx
Xcopy init.el %home%\.emacs.d\init.el /e /y /f /q
Xcopy lisp\myhydras.el %home%\.emacs.d\myhydras.el /e /y /f /q
Xcopy lisp\custom.el %home%\.emacs.d\custom.el /e /y /f /q
Xcopy fluff\blackhole.png %home%\.emacs.d\blackhole.png /e /y /f /q
Xcopy lisp\custom-functions.el %home%\.emacs.d\custom-functions.el /e /y /f /q
Xcopy unix\.clang-format %home%\.clang-format /e /y /f /q
Xcopy snippets %home%\.emacs.d\snippets /e /y /f /q

rem Lisp not on melpa
IF NOT EXIST "%home%\.emacs.d\lisp" md %home%\.emacs.d\lisp
Xcopy lisp\ox-mediawiki.el %home%\.emacs.d\lisp\ox-mediawiki.el /e /y /f /q
Xcopy lisp\perltidy.el %home%\.emacs.d\lisp\perltidy.el /e /y /f /q
Xcopy lisp\evil-markdown.el %home%\.emacs.d\lisp\evil-markdown.el /e /y /f /q
