# Build source and store in "./renv/local/"

# Rebuild from local source
renv::install("./renv/local/mcmcabn_0.4.tar.gz", rebuild = TRUE)

# update renv
renv::snapshot()
