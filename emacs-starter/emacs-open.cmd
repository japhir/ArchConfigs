@echo off
set "BIN=C:\Program Files\Emacs\emacs-31.1\bin"
"%BIN%\emacsclient.exe" -n -q %* 2>nul || start "" "%BIN%\runemacs.exe" %*
