
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- build with rmarkdown::render("README.Rmd") -->

# hierbase

The R-package hierbase offers tools to perform hierarchical inference
for one or multiple data sets based on ready-to-use (group) test
functions or alternatively a user specified (group) test function. The
procedure is based on an efficient hierarchical multiple testing
correction and controls the FWER. The functions can easily be run in
parallel. Hierarchical inference can be applied to (low- or)
high-dimensional data sets to find significant groups or single
variables (depending on the signal strength and correlation structure)
in a data-driven and automated procedure. Possible applications can for
example be found in statistical genetics and statistical genomics.

## Installation

You can install the development version from Github by running

``` r
# install.packages("devtools")
devtools::install_github("crbasel/hierbase")
```

## References

Renaux, C., Bühlmann, P. (2021), Efficient Multiple Testing Adjustment
for Hierarchical Inference. \<arXiv:2104.15028\>
