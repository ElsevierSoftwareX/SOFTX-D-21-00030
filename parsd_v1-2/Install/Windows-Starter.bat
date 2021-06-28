::---
::ParSD
::Tool to design and analyze particle size distributions
::Copyright (C) 2020 Jens Fruhstorfer
::
::
::This file is part of ParSD.
::
::ParSD is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
::
::ParSD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
::
::See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with ParSD. If not, see <http://www.gnu.org/licenses/>.
::---
mshta.exe vbscript:Execute("msgbox ""In the next step, select Rscript.exe from your computer. Usually found under: C:\Program files\R\R-version\bin"",0,""Info"":close")
@echo off
set dialog="about:<input type=file id=FILE><script>FILE.click();new ActiveXObject
set dialog=%dialog%('Scripting.FileSystemObject').GetStandardStream(1).WriteLine(FILE.value);
set dialog=%dialog%close();resizeTo(0,0);</script>"

for /f "tokens=* delims=" %%p in ('mshta.exe %dialog%') do set "file=%%p"

@echo "%file%" main.R > ..\ParSD.bat

copy ParSD.vbs ..

mshta.exe vbscript:Execute("msgbox ""Installation complete."",0,""Info"":close")
