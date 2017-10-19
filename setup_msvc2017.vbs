'* Setup Microsoft visual studio environment, this is done to help put
'* irony-mode for emacs, also this is generally good. lol

Set objShell = WScript.CreateObject("WScript.Shell")
objShell.Run "cmd /c call ""C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build/vcvarsall.bat"" amd64", 0, True

'* The original file to do this was setup_msvc17.bat. But this does it in the background. Ugly looking code thou.