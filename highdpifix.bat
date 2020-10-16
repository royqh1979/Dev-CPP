@echo off title This is to set HIGHDPI Scaling

REG ADD "HKCU\Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers" /V "%cd%\devcpp.exe" /T REG_SZ /D ~HIGHDPIAWARE /F