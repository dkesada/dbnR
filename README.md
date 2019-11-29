# dbnR

An implementation of Gaussian dynamic Bayesian networks (GDBN) structure learning and inference based on Marco Scutari's package bnlearn (http://www.bnlearn.com/). The structure learning algorithm implemented is a variation on Ghada Trabelsi's dynamic max-min hill climbing (https://tel.archives-ouvertes.fr/tel-00996061/document). The inference is performed either via the particle filtering offered by bnlearn or by doing exact inference over the multivariate Gaussian equivalent of a net implemented in this package. A visualization tool is implemented for GDBNs and bnlearn's BNs via the visNetwork package (https://github.com/datastorm-open/visNetwork).

## Current development news

As of today (29/11/2019), the main functionality of the package is running and working. The main focus now is to refractor the dbn.fit object from the current S3 objects into a more encapsulated ones that contain the transformated multivariate normal and to tidy up the package with the idea of submitting it to CRA.N For now, the migration to R6 objects is not prioritary. I want to try out these objects in a devel branch and see if they meet my expectations. If they do, the structure of the code should become more clean and natural and should allow easier further extensions that are planned down the line.
Successful implementation of the R6 objects will result in a non retro-compatible 1.x.x version.

## Getting Started

### Prerequisites

This package requires R ≥ 3.6.1 to work properly. It's possible to make it work for older versions of R and of each of the packages by fiddling with the [DESCRIPTION](DESCRIPTION) file, although this is not recommended.

The __bnlearn__ and __data.table__ packages, among others, are required for this package to work. They will be installed automatically when installing this package. They can also be installed manually via CRAN with the command

```
install.packages(c("bnlearn", "data.table"))
```
The packages __visNetwork__, __magrittr__ and __grDevices__ are optional for the visualization tool. They will only be required if you want to use it.

### Installing
As of today, the easiest way of installing dbnR is via the _install_github_ function in the __devtools__ package. The command you need to run is
```
devtools::install_github("dkesada/dbnR")
```
This will install the required dependencies if they are not available. After this, you will be ready to use the package.

## Basic examples

To get the structure of a GDBN from a dataset, you need to use the function _learn_dbn_struc_

```
library(dbnR)
data(motor)
l_res <- learn_dbn_struc(motor, 3)
```
The dt argument has to be either a data.frame or a data.table of numeric columns, in the example we use the sample dataset included in the package. The size argument determines the number of time slices that your net is gona have, that is, the Markovian order of the net. A Markovian order of 2 means that your data in the present is independent of the past given the previous time slice. If your case doesn't meet this criteria, the size of the net can be increased, to take into account more past time slices in the inference. In our function, Markovian order = size - 1. The function returns a list with the learned structure and the folded dataset with the extended rows.

Once the structure is learnt, it can be ploted and used to learn the parameters
```
plot_dynamic_network(l_res$net)
```
![alt text](media/dbn_plot.png)

```
dbn_fit <- fit_dbn_params(l_res$net, l_res$f_dt)
```

## License

This project is licensed under the GPL-3 License, following on bnlearn's GPL(≥ 2) license - see the [LICENSE](LICENSE) file for details

## References

* The bnlearn package (http://www.bnlearn.com/).
* The visNetwork package (https://datastorm-open.github.io/visNetwork/)
* Kaggle's dataset repository, where the sample dataset is from (https://www.kaggle.com/wkirgsn/electric-motor-temperature)
