## Update resubmission
Update from version 0.7.8 to 0.7.9.  In this update, I fixed unstable behaviour of regex in C++ on both the psoho and natPsoho algorithms that broke them completely. A new parameter has been added to the DMMHC algorithm to allow forcing arcs in the transition network. There are also some additional small fixes and now the C++11 specification has been removed. Additionally, I fixed all the NOTEs that appeared on the CRAN checks. In this resubmission I fixed the only NOTE that showed up about link targets missing the bnlearn anchor.

## Test environments
* Windows 10 (x64), R 4.4.1 (local)
* Rhub
* win-builder (devel, release and oldrelease)
* macOS builder

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

(On win-builder)
Found the following (possibly) invalid URLs:
  URL: https://kaggle.com/datasets/wkirgsn/electric-motor-temperature
    From: README.md
    Status: 404
    Message: Not Found
  
* The URL is not invalid, it redirects to the dataset source correctly.

## Downstream dependencies
There are no downstream dependencies
