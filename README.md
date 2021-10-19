
# bnaiaR

<!-- badges: start -->
<!-- badges: end -->

Bayesian Network Analysis of Intracranial Aneurysm.
Data preparation and analysis of risk factors for intracranial aneurysms.

## Installation

You can install the released version of bnaiaR from [Github](https://github.com/matteodelucchi/bnaiaR) with:

``` r
devtools::install_github("matteodelucchi/bnaiaR")
```

In case of installation issues caused by the package `igraph`, check this post:
[github.com/igraph](https://github.com/igraph/rigraph/issues/275#issuecomment-852000182)

## Description

### Data

There are different data sets attached to the package. 
For example, the preprocessed raw data can be accessed with this command.

``` r
data(adb)
```

The data sets description can be found in the respective help page.

``` r
help(adb)
```

To follow the data preparation and more details consider the vignettes in this order:

1. `raw_data_harmonization`: From querying the raw data from an unpublished 
data base to `adb.raw` data set.  
2. `data_preprocessing`: From raw data to the preprocessed `adb` data set.    
3. `data_preparation_for_experiments`: Implementation of prior knowledge 
(blacklist, banned-matrix), specification of variable distributions and 
variable selection for each analysis scenario.  


