for /r "%cd%" %%i in (*.pas) do (
    "d:\program files (x86)\borland\delphi7\bin\dcc32.exe" -O+ -Q -M -Y -Z -$D- -$L-   "%%i"
)
