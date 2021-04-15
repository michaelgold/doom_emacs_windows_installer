:: Forward the ./doom script to Emacs

@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
SET HOME=%USERPROFILE%
SET PATH=%PATH%;C:\Program Files\Emacs\x86_64\bin

PUSHD "%~dp0" >NUL

SET args=
SET command=%1

:LOOP
SHIFT /1
IF NOT [%1]==[] (
    SET args=%args% %1
    GOTO :LOOP
)

IF [%command%]==[run] (
   start runemacs -Q %args% -l ..\init.el -f "doom-run-all-startup-hooks-h"
) ELSE (
   emacs --quick --script .\doom -- %*
)

POPD >NUL
ECHO ON
