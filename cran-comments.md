## Update
Update from version 0.5.3 to 0.7.1. In this update, I have fixed some bugs, added a new type of inference and added a new structure learning algorithm maintaining backward compatibility with older versions.

## Test environments
* Windows 10 (x64), R 4.1.1 (local)
* Ubuntu 18.04 (x64), R 4.1.1 (local)
* Rhub
* win-builder (devel, release and oldrelease)

## R CMD check results
0 ERRORs | 0 WARNINGs | 2 NOTEs

(On my local Ubuntu machine)
checking installed package size ... NOTE
  installed size is  5.3Mb
  sub-directories of 1Mb or more:
    libs   4.5Mb
    
* My package uses C++ code with Rcpp, and in linux systems this seems to cause the 'libs' folder to get bloated.

(On my local Windows 10 machine)
Note: information on .o files for i386 is not available
  Note: information on .o files for x64 is not available
  File 'C:/Users/dkesada/Documents/dbnR.Rcheck/dbnR/libs/i386/dbnR.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  File 'C:/Users/dkesada/Documents/dbnR.Rcheck/dbnR/libs/x64/dbnR.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)

* I only get this note on my local win10 machine with R versions >4.0.2. My code includes no such Fortran or C code, and after looking for solutions to this, it seems like it has to do with some kind of incompatibility between rtools and windows in recent R versions.

## Downstream dependencies
There are no downstream dependencies
