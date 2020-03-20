## Resubmission
This is a resubmission. In this version I have:

* Replaced \dontrun{} by \donttest{}

* Replaced cat() calls by print(x, quote = FALSE)

## Test environments
* local OS X install, R 3.6.3
* Windows 10 (x64), R 3.6.3 (local)
* Ubuntu 16.04 (x64), R 3.6.3 (local)
* Rhub
* win-builder (devel and release)

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Quesada <dkesada@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Markovian (9:5)
  Trabelsi (11:33)

### Comments
Both are proper nouns, 'Markovian order' refers to the auto-regressive order of the model and Trabelsi is the author of the cited paper in the description.

## Downstream dependencies
There are no downstream dependencies
