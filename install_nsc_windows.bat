@echo on


stack >nul 2>&1 && (
    SET stack_found=true
) || (
    SET stack_found=false
)

if %stack_found%==true (echo stack was not found || exit)

stack path --local-bin > temp.txt
set /p install_path=<temp.txt
del temp.txt

stack install

move %install_path%\no-syn-exe.exe %install_path%\nsc.exe
