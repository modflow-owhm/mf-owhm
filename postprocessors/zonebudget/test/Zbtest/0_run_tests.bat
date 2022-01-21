@set CWD_BACKUP=%CD%
@CD /D %~dp0

..\..\bin\zonebudget.exe  <Zone_CMD_Answers.txt

..\..\bin\zonebudget.exe  <Zone_CMD_Answers_INTERNAL1.txt
..\..\bin\zonebudget.exe  <Zone_CMD_Answers_INTERNAL2.txt

..\..\bin\zonebudget.exe  <Zone_CMD_Answers_EXTERNAL1.txt
..\..\bin\zonebudget.exe  <Zone_CMD_Answers_EXTERNAL2.txt

..\..\bin\zonebudget.exe  <Zone_CMD_Answers_OPENCLOSE1.txt
..\..\bin\zonebudget.exe  <Zone_CMD_Answers_OPENCLOSE2.txt

@echo.
@echo.
@CD /D %CWD_BACKUP%
@set "CWD_BACKUP="
@pause