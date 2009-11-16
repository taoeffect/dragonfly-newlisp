@ECHO OFF
SET NEWLISP_REDIRECTION=.\dragonfly-framework\newlisp-redirection.lsp

IF NOT EXIST "%NEWLISP_REDIRECTION%" GOTO NOFILE

ECHO If all goes well visit http://localhost:8080 in your browser
newlisp "%NEWLISP_REDIRECTION%" -http -d 8080 -w . %*

GOTO END

:NOFILE
ECHO ERROR: cannot find file: %NEWLISP_REDIRECTION%

:END
