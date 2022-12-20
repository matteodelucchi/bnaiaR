
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

0. Raw data is harmonized in the private R package [ExplorDataISGC](https://github.zhaw.ch/delt/ExploreDataISGC/tree/dev-multicentre) for data privacy reasons. 
Specifically in the vignettes [geneva2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/geneva2isgc.Rmd), 
[kuopio2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/kuopio2isgc.Rmd), 
[nantes2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/nantes2isgc.Rmd), 
[ucl2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/ucl2isgc.Rmd), 
[utrecht2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/utrecht2isgc.Rmd) 
[tum2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/tum2isgc.Rmd) 
[kssg2isgc](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/kssg2isgc.Rmd) 
and then pooled in 
[combineISGC](https://github.zhaw.ch/delt/ExploreDataISGC/blob/dev-multicentre/vignettes/combineISGC.Rmd).  
The detailed steps from the harmonization can be inspected in [bnaiaR/R/harmonization.R](R/harmonization.R).

1. [data_preprocessing](vignettes/data_preprocessing.Rmd) includes the pooled data `adbisgc` from step 0 in this package in 'inst/extdata/adbisgc.rda'.
Various feature engineering steps are performed and stored in 'data/adbisgc.rda' and 'data/adbisgc_ohe.rda'.  
2. [data_preparation_for_experiments](vignettes/data_preparation_for_experiments.Rmd): Implementation of prior knowledge 
(blacklist, banned-matrix), specification of variable distributions and 
variable selection for each analysis scenario.
The respective data sets are stored in 'data/*.rda'.  
3. [regression_analysis](vignettes/regression_analysis.Rmd): Multivariable logistic regression models, generalized additive regressions and mixed-effect models thereof.  
6. [discrete BN structure learning](vignettes/discrete_BN_SL.Rmd): Structure learning for discrete BNs, additive discrete BNs, additive discrete mixed-effect BNs.  
7. [discrete BN validation](vignettes/discrete_BN_validation.Rmd): Classification error estimation and inference.  
8. [continuous BN structure learning](vignettes/continuous_ABN_SL.Rmd): Structure learning for continuous additive BNs.  

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


