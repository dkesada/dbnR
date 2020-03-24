## Resubmission
This is a resubmission. In this version I have:

* Fixed the dependency on R (>= 3.5.0) rather than R (>= 3.6.0). Check results on r-oldrel-osx-x86_64 failed because it wasn't allowed to install the package for a version prior to 3.6.0. This was due to the package visualization tool depending on grDevices (>= 3.6.0). I made an alternative that runs on grDevices (>= 3.5.0)

## Test environments
* local OS X install, R 3.6.3
* Windows 10 (x64), R 3.6.3 (local)
* Ubuntu 16.04 (x64), R 3.6.3 (local)
* Rhub
* win-builder (devel, release and oldrelease)

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Quesada <dkesada@gmail.com>'
Days since last update: 4

* checking dependencies in R code ... NOTE (win_oldrelease)
Missing or unexported object: 'grDevices::hcl.colors'

### Comments
The function 'grDevices::hcl.colors' was added in 'grDevices' version 3.6.0. In order to make the package compatible with R versions (>= 3.5.0) and not get an error, I added a fix for versions prior to 3.6.0 using a different palette. This function is now never called if the 'grDevices' package is not (>= 3.6.0).

## Downstream dependencies
There are no downstream dependencies
