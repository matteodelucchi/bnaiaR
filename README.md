
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

To ensure smooth compatibility and to remain your local setup untouched, I recommend to use this package within a virtual environment from `renv`. 
A local installation of `mcmcabn` was used,` which can be installed with `renv` following [this guide](/inst/extdata/mcmcabn/install_local_mcmcabn.R).

## Description

### Data

There are different data sets attached to the package. 
For example, the preprocessed raw data can be accessed with this command.

``` r
data(adb)
```

The data sets description can be accessed in the respective help page.

``` r
help(adb)
```

To follow the data preparation and more details consider the vignettes in this order:

1. [raw_data_harmonization](vignettes/raw_data_harmonization.Rmd): Querying the raw data from a private
data base resulting in the `adb.raw` data set.  
2. [data_preprocessing](vignettes/data_preprocessing.Rmd): From raw data to the preprocessed `adb` data set.    
3. [data_preparation_for_experiments](vignettes/data_preparation_for_experiments.Rmd): Implementation of prior knowledge 
(blacklist, banned-matrix), specification of variable distributions and 
variable selection for each analysis scenario.  
4. [correlation_analysis](vignettes/correlation_analysis.Rmd): Descriptive statistics.
5. [regression_analysis](vignettes/regression_analysis.Rmd): Multivariable logistic regression models.
6. [Discrete BN structure learning](vignettes/discrete_BN_SL.Rmd): Structure learning for discrete BNs.
7. [DBN validation](vignettes/discrete_BN_validation.Rmd): Classification error estimation and inference.
8. [ABN analysis](inst/extdata/inst/extdata/mcmcabn/mcmcabn_result_postproc_analysis.Rmd): [Structure learning](inst/extdata/mcmcabn/mcmcabn_multinomial.R) was performed on a high-performance computing cluster followed by local [postprocessing](inst/extdata/mcmcabn/mcmcabn_result_postproc_analysis.Rmd) Details from additive BNs.  

9. For the publication [additional figures](vignettes/additional_figures.Rmd) and [tables](vignettes/datastructure.Rmd) were produced.


## Citations

If you make use of this R package or one of its data sets in your research we would appreciate a 
citation of the following article:

Matteo Delucchi, Georg R. Spinner, Marco Scutari, Philippe Bijlenga, Sandrine Morel, Christoph M. Friedrich, Reinhard Furrer and Sven Hirsch. 
2022.
**Bayesian network analysis reveals the interplay of intracranial aneurysm rupture risk factors**.
Computers in Biology and Medicine. [10.1016/j.compbiomed.2022.105740](https://doi.org/10.1016/j.compbiomed.2022.105740).

you can get it as BibTeX entry with

``` r
citation("bnaiaR")
``` 


