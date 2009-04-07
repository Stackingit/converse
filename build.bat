@echo off

echo Cleaning up beam files....
del *.beam

echo Compiling modules....
for %%E IN (*.erl) DO ( 
 echo compiling %%E :
 erlc %%E +debug_info
 echo testing %%E :
 erl -noshell -s eunit test %%~nE -s init stop 
 echo. )

rem TODO figure out how to get the xref going
rem erl -noshell -s xref:d('.') -s init stop

pause

