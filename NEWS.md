# dbnR 0.3.4

* Fixed the dependency on R (>= 3.5.0) rather than R (>= 3.6.0). CRAN checks for old released R versions failed because of this. The only difference is that running the package with R (< 3.6.0) displays the plot_dynamic_network() with a different color palette.

# dbnR 0.3.3

* Fixed \dontrun{} into \donttest{} and cat() into print(x, quote=FALSE)

# dbnR 0.3.2

* Redid and added new examples to exported functions
* Fixing spell checking and successful builds
* Added NEWS.md file. Should have been added earlier.