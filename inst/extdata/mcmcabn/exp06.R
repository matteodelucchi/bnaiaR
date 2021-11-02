rm(list = ls())

library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(abn)
library(mcmcabn)
library(ExplorDataISGC)
library(BNstructureLearning)

#####
# Settings
#####
EXPNO <- "6"
FILENAME <- paste0("exp", EXPNO)
FILENAMEbase <- "./results/raw/"
DATPATH <- paste0("./data/")
#create and register cluster
if(amilocal()){
  n.cores <- parallel::detectCores() - 1 # local
} else{
  n.cores <- 25 # on HPC
}
clust <- parallel::makeCluster(n.cores, outfile="multicore.log")

#####
# Parameters
#####
METHOD <- "mle"
SCORE <- "bic"

RETURN.DAGS <- 100000
THINNING <- 0
BURNIN.LEN <- 0
# MCMC.SCHEME <- c(RETURN.DAGS, THINNING, BURNIN.LEN)
MCMC.SCHEME <- c(RETURN.DAGS+BURNIN.LEN, 0, 0) # speed up computation if postprocessed
MCMC.SEEDS <- c(560505, 921213, 352629, 23146)
PROB.REV = 0.03 # REV and MBR are efficient in producing high scoring structure but computationally costly compared to classical MCMC jumps.
PROB.MBR = 0.03
# PROB.MBR = 0
MCMC.PRIOR = 2 # 2: Koivisto; 1: uninformative

THRESHOLD <- 0.5 # arcstrength

#####
# Load data
#####
cat(paste0("\nLoading data from: ", DATPATH, FILENAME, "_data.RData", "\n"))
load(file = paste0(DATPATH, FILENAME, "_data.RData"))

# create empty retain matrix
retain <- matrix(0, ncol(abndata), ncol(abndata))
colnames(retain) <- rownames(retain) <- names(abndata)

#####
# Find optimal no. of parent nodes
#####
cat(paste("\nRun the exact search across incremental parent limits with method:", METHOD))

novars <- ncol(abndata)
tmpscores <- vector(length = novars)
net.scores <- data.frame(npar = NULL, scoretype = NULL, scorevalue = NULL)

doParallel::registerDoParallel(cl = clust)
net.scores <- foreach(i = 1:novars,
                      .combine = 'rbind',
                      .packages = c("abn", "mcmcabn"),
                      .inorder = TRUE) %dopar% {
  max.par <- i
  mycache <- buildScoreCache(data.df = as.data.frame(abndata),
                             data.dists = dist,
                             dag.banned = banned,
                             dag.retained = retain,
                             max.parents = max.par,
                             method = METHOD)

  dag.mP <- mostProbable(score.cache = mycache,
                         score = SCORE)
  fabn.mP <- fitAbn(object = dag.mP,
                    method = METHOD)


  return(c(i,SCORE,fabn.mP[[SCORE]]))

  cat(paste("\nnetwork score for", i, "parents =", fabn.mP[[SCORE]], "\n\n"))
}
stopCluster(clust)

# format net.scores
net.scores <- as.data.frame(net.scores)
colnames(net.scores) <- c("npar", "scoretype", "scorevalue")
rownames(net.scores) <- NULL
net.scores[,1] <- as.integer(net.scores[,1])
net.scores[,3] <- as.numeric(net.scores[,3])

for (i in 1:nrow(net.scores)){
  if(net.scores$scoretype[i] == "bic"){
    net.scores$scorevalue[i] <- -net.scores$scorevalue[i]
  }
}
# Save intermediate raw data
save(abndata,
     dist,
     retain,
     banned,

     net.scores,
     file = paste0(FILENAMEbase, FILENAME, "_intermediate.RData"))
cat("\nIntermediate data saved")

#####
# ABN with max. parents
#####
cat("\nStart ABN with max.par...")
starttime <- Sys.time()

# Find number of parents with highest network score
x <- net.scores %>%
  filter(scoretype == SCORE) %>%
  mutate(npar = as.numeric(npar),
         scorevalue = as.numeric(scorevalue))
max.par <- x$npar[which(x$scorevalue == max(x$scorevalue))][1]

cat(paste("\nusing method:", METHOD,
          "and max. parents:", max.par))

mycache.maxpar <- buildScoreCache(data.df = as.data.frame(abndata),
                                  data.dists = dist,
                                  dag.banned = banned,
                                  dag.retained = retain,
                                  max.parents = max.par,
                                  method = METHOD)

dag.maxpar <- mostProbable(score.cache = mycache.maxpar,
                           score = SCORE)
fabn.maxpar <- fitAbn(object = dag.maxpar,
                      method = METHOD)

endtime <- Sys.time()
cat(paste("\nEnd ABN with max.par. Time used [h]:", round(difftime(endtime,starttime, units = "hours"),2)))

#####
# MCMC ABN
#####
cat("\nStart MCMCabn...")
starttime <- Sys.time()

clust <- parallel::makeCluster(length(MCMC.SEEDS), outfile="multicore2.log")
doParallel::registerDoParallel(cl = clust)
mcmc.out.list <- foreach(SEED = MCMC.SEEDS,
        .packages = c("abn", "mcmcabn"),
        .inorder = TRUE) %dopar% {
          mcmcabn(score.cache = mycache.maxpar,
                              score = SCORE,
                              data.dists = dist,
                              max.parents = max.par,
                              mcmc.scheme = MCMC.SCHEME,
                              seed = SEED,
                              verbose = FALSE,
                              start.dag = dag.maxpar$dag,
                              prob.rev = PROB.REV, # REV and MBR are efficient in producing high scoring structure but computationally costly compared to classical MCMC jumps.
                              prob.mbr = PROB.MBR,
                              prior.choice = MCMC.PRIOR) # Koivisto prior
        }
stopCluster(clust)
#  Error in { : task 2 failed - "object 'new.parent.j' not found"

endtime <- Sys.time()
cat(paste("\nEnd MCMC ABN. Time used [h]:", round(difftime(endtime,starttime, units = "hours"),2)))


### Save raw data
save(list = ls(),
     file = paste0(FILENAMEbase, FILENAME, "_final.RData"))
cat("Final data written to:", paste0(FILENAMEbase, FILENAME, "_final.RData"))


