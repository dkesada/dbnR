# dbnR 0.5.2

* Fixed the visualization of dynamic networks with no intra-slice arcs. The nodes were plotted in a horizontal line, now they are shown in vertical lines in each time slice

# dbnR 0.5.1

* More extensive examples in markdowns/usage_example.Rmd
* Fixed the test dataset
* New learning algorithm present in the docs and readme

# dbnR 0.5.0

* Refractored the structure learning to accommodate multiple algorithms
* Added a new particle swarm optimization structure learning algorithm (psoho)

# dbnR 0.4.6

* Dropped std:vector in favor of Rcpp::Numeric/StringVector in gauss_transform.cpp
* Fixed a security check typo

# dbnR 0.4.5

* Fixed .Rbuildignore markdown and media folders

# dbnR 0.4.4

* Bug fixes in the evidence providing
* Typo fix in the "approx" mode of forecasting

# dbnR 0.4.0

* Added the possibility to provide evidence in each forecasting step

# dbnR 0.3.4

* Fixed the dependency on R (>= 3.5.0) rather than R (>= 3.6.0). CRAN checks for old released R versions failed because of this. The only difference is that running the package with R (< 3.6.0) displays the plot_dynamic_network() with a different color palette.

# dbnR 0.3.3

* Fixed \dontrun{} into \donttest{} and cat() into print(x, quote=FALSE)

# dbnR 0.3.2

* Redid and added new examples to exported functions
* Fixing spell checking and successful builds
* Added NEWS.md file. Should have been added earlier.