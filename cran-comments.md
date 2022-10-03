## New submission after archivation
Update from version 0.7.5 to 0.7.8. The package was archived due to an incompatibility issue introduced in a recent update of the package bnlearn, which my package depends on. My sincerest apologies, I was mistaken about the due date of the fix and missed the deadline. This incompatibility has been fixed and no further errors are present.

Apart from this, in this update I added generic methods for my S3 objects "dbn" and "dbn.fit". I also extended the generic methods for bnlearn's S3 objects "bn" and "bn.fit" to work with "dbn" and "dbn.fit" objects. All available generic methods are now shown with methods(class="dbn") and methods(class="dbn.fit"). The documentation has been improved for exported functions and hidden for internal functions when calling help(package="dbnR"). Additionally, regular prints for low-level functions have been exchanged with cats and inverse matrix calculation is now performed with MASS::ginv(). This version maintains backwards compatibility with older versions.

## Test environments
* Windows 10 (x64), R 4.2.1 (local)
* Ubuntu 18.04 (x64), R 4.2.1 (local)
* Rhub
* win-builder (devel, release and oldrelease)

## R CMD check results
0 ERRORs | 0 WARNINGs | 3 NOTEs

checking CRAN incoming feasibility ... NOTE
Maintainer: ‘David Quesada <dkesada@gmail.com>’

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2022-10-02 as issues were not corrected
    in time.
    
* I apologize again for this, I missed the deadline because I mistook the due date with another one. The error fix just involved switching the method "mle" with "mle-g" in a couple examples and a function when calling bnlearn::bn.fit.

(On win-builder)
Possibly mis-spelled words in DESCRIPTION:
  Bielza (13:49)
  Larrañaga (13:63)
  Maciel (12:76)
  Quesada (13:37)
  Trabelsi (12:5)
  
* Those are the names of the authors of some of the papers I cite in the description, they are not mis-spelled words.

(On my local Ubuntu machine)
checking installed package size ... NOTE
  installed size is  5.4Mb
  sub-directories of 1Mb or more:
    libs   4.5Mb
    
* My package uses C++ code with Rcpp, and in linux systems this seems to cause the 'libs' folder to get bloated.

## Downstream dependencies
There are no downstream dependencies
