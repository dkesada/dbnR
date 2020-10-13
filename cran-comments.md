## Resubmission
This is a resubmission, fixing the "Namespace in Imports field not imported from: ‘R6’ All declared Imports should be used" NOTE in CRAN check results. Added an 'importFrom' for the 'R6Class' function so that it appears in the NAMESPACE file. 

Update from version 0.4.5 to 0.5.3. In this update, I have fixed some bugs and added a new structure learning algorithm mantaining backward compatibility with older versions.

## Test environments
* Windows 10 (x64), R 4.0.2 (local)
* Ubuntu 16.04 (x64), R 4.0.2 (local)
* Rhub
* win-builder (devel, release and oldrelease)

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Downstream dependencies
There are no downstream dependencies
