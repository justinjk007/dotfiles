@echo off
rem Xcopy <Source> [<Destination>] [/w]
rem Find more here --> https://msdn.microsoft.com/en-us/library/cc771254.aspx
Xcopy init.el %home%\.emacs.d\init.el /e /y /f /q
echo f | Xcopy .clang_complete %home%\.clang_complete /e /y /f /q
echo f | Xcopy .clang-format %home%\.clang-format /e /y /f /q
Xcopy custom.el %home%\.emacs.d\custom.el /e /y /f /q
Xcopy custom-functions.el %home%\.emacs.d\custom-functions.el /e /y /f /q
Xcopy snippets %home%\.emacs.d\snippets /e /y /f /q
Xcopy lisp %home%\.emacs.d\lisp /e /y /f /q
