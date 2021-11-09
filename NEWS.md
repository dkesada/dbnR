# dbnR 0.7.3

* Added the possibility of passing a blacklist_tr parameter to the dmmhc algorithm in order to avoid certain inter-slice arcs.

# dbnR 0.7.2

* Added authors and doi of the structure learning algorithms to the description field as per request of CRAN maintainers.

# dbnR 0.7.1

* natPsoho algorithm fixed and working 

* Fixed check results

* Removed unused rand import in C++

# dbnR 0.7.0

* Added new order invariant particle swarm structure learning algorithm: natPSOHO

# dbnR 0.6.2

* Fixed the create_blacklist method for sizes greater than 10. A regex "$" was missing.

# dbnR 0.6.1

* Added variable inertia, global best and local best parameters over time to the PSOHO algorithm

* Now filtered_fold_dt() allows a boolean argument to avoid deleting the id_var column before returning the folded dataset

# dbnR 0.6.0

* Added the possibility of doing smoothing over a time series

* Fixed a bug that returned NaNs when predicting 1 variable in t_0 and providing all other variables as evidence

# dbnR 0.5.7

* Parameter for different bnlearn scores in the PSOHO algorithm

* New Jupyter notebook that shows how to integrate dbnR with Python

* Fixed the 'intra' argument in the mmhc algorithm. The 'if' statements regarding it were wrong.

* Several fixes in compatibility with 'as.data.table' calls

# dbnR 0.5.6

* Hotfix for the 'f_dt' parameter in the 'learn_dbn_struc' function. It was not being passed down to each algorithm.

* New auxiliary function 'filtered_fold_dt'

* Added the possibility of not learning intra-slice arcs in the dmmhc algorithm

# dbnR 0.5.5

* Removed obsolete and time consuming security checks. The psoho initial checks were supposed to secure the separate usage of the 'Position' and 'Velocity' classes, but they are not exported, and consequently they cannot be misused in the sense that the checks prevented. They also were O(n) on the size of the ordering, and were run each time some operation was performed over positions or velocities.

# dbnR 0.5.4

* Additional security checks for the PSOHO algorithm

* Added an auxiliary function to reduce the frequency in a data.table by performing the mean of consecutive rows

* Added an auxiliary function to fold a dataset and not allow instances from different labeled series to be in the same row

* Added the possibility to introduce a previously folded dataset to the structure learning. This helps when the dataset has several iterations of the same process separated by id. If you fold the dataset directly, data from 2 or more different processes will appear in some rows, giving the impression to the model that the ending and the beginning of the processes merge into each other, which most likely is not true

# dbnR 0.5.3

* Fixed the lack of imports from the 'R6' package with @importFrom, NOTE in CRAN checks results

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