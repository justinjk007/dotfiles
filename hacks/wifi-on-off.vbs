Set objShell = WScript.CreateObject("WScript.Shell")
objShell.Run "cmd /c netsh wlan set autoconfig enabled=yes interface=""Wi-fi""", 0, True
WScript.Sleep(10000)
objShell.Run "cmd /c netsh wlan set autoconfig enabled=no interface=""Wi-fi""", 0, True
