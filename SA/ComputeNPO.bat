@echo Debut script : %date:~-4%_%date:~3,2%_%date:~0,2%__%time:~0,2%_%time:~3,2%_%time:~6,2%
@R-3.2.3\bin\x64\rscript.exe Algo.R NPO.csv Parameters
@echo Fin script : %date:~-4%_%date:~3,2%_%date:~0,2%__%time:~0,2%_%time:~3,2%_%time:~6,2%
pause
