Set objShell = WScript.CreateObject("WScript.Shell")
objShell.Run "cmd /c emacs --daemon", 0, True

'* To enable this put this file in your startup folder which can be
'* located if you type "shell:startup" in your RUN window.