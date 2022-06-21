# Try this
renv::init(bioconductor = TRUE)

# follow the suggestions


# if it doesn't work, continue:

# make sure we have latest version of BiocManager
renv::install("BiocManager")

# read lockfile records
lockfile <- renv:::renv_lockfile_read("renv.lock")
records <- lockfile$Packages

# keep only packages from Bioconductor
bioc <- Filter(function(record) {
  record$Source == "Bioconductor"
}, records)

pkgs_bioc <- names(bioc)

# and this list is for all others
notbioc <- Filter(function(record) {
  record$Source != "Bioconductor"
}, records)

pkgs_notbioc <- names(notbioc)

# install biocs first
options(repos = BiocManager::repositories())
renv::install(pkgs_bioc)

# install non-bioc second: ignore igraph and abn packages as these cause troubles...
renv::install(pkgs_notbioc[which(!(pkgs_notbioc %in% c("igraph", "gRbase", "abn", "mcmcabn")))])

# in the shell, update igraph manually
# sudo apt update
# sudo apt install r-cran-igraph

# link igraph to renv



# manually install packages
# The following required packages are not installed:
#
#   boot        [required by DescTools, lme4]
# class       [required by e1071, ipred]
# codetools   [required by foreach, globals]
# foreign     [required by maptools]
# igraph      [required by gRbase]
# KernSmooth  [required by gplots, prodlim]
# nnet        [required by abn, car, ipred]
# rpart       [required by ipred]

renv::install(packages = c("boot", "class", "codetools", "foreign", "KernSmooth", "nnet", "rpart"))
# create a new snapshot
renv::snapshot()

# manually add igraph to lock file
igraph_record <- list(
  Package= "igraph",
  Version= "1.2.11",
  Source= "Repository",
  Repository= "CRAN"
)
renv::record(igraph_record)
