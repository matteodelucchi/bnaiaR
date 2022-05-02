rm(list = ls())

library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(abn)
library(mcmcabn)
library(bnaiaR)
library(ggplot2)

#####
# Settings
#####
DEBUG <- T
EXPNO <- "ABNmultinomialVesselLoc"
FILENAME <- paste0("exp", EXPNO)
FILENAMEbase <- "./inst/extdata/mcmcabn/results/raw/"
#create and register cluster
if (amilocal()) {
  n.cores <- parallel::detectCores() - 1 # local
} else{
  n.cores <- 25 # on HPC
}

#####
# Parameters
#####
METHOD <- "mle"
SCORE <- "bic"
if (DEBUG) {
  RETURN.DAGS <- 2
} else {
  RETURN.DAGS <- 100000
}
THINNING <- 0
BURNIN.LEN <- 0
# MCMC.SCHEME <- c(RETURN.DAGS, THINNING, BURNIN.LEN)
MCMC.SCHEME <-
  c(RETURN.DAGS + BURNIN.LEN, 0, 0) # speed up computation if post-processed. Meaning: c(RETURN.DAGS, THINNING, BURNIN.LEN)
MCMC.SEEDS <- c(560505, 921213, 352629, 23146)
PROB.REV = 0.03 # REV and MBR are efficient in producing high scoring structure but computationally costly compared to classical MCMC jumps.
PROB.MBR = 0.03
MCMC.PRIOR = 2 # 2: Koivisto; 1: uninformative

THRESHOLD <- 0.5 # arcstrength


#####
# Data Preparation
#####
cat(paste0(
  "\nPrepare data and store it: ",
  FILENAMEbase,
  FILENAME,
  "_data.RData",
  "\n"
))

prep_exp_data(
  dat = adb,
  EXPNO,
  age = "cont",
  location = "byVessel-multinomial",
  size = "log",
  smoking = "mult",
  SAVE = T,
  FILENAMEbase = FILENAMEbase
)

cat(paste0(
  "\nLoading data from: ",
  FILENAMEbase,
  FILENAME,
  "_data.RData",
  "\n"
))

load(file = paste0(FILENAMEbase, FILENAME, "_data.RData"))

# check for outliers in continuous vars
df <- abndata %>%
  mutate(ID = seq(1,nrow(abndata)))%>%
  reshape2::melt(id.vars=c("ID"),
                 measure.vars=c("AgeDiag", "IAsize_log"))

p.outliers <- ggplot(df, aes(x=value))+
  facet_wrap(~variable, scales = "free")+
  geom_boxplot(notch = TRUE) +
  coord_flip() +
  theme_bw() +
  ggtitle("Raw multinomial mixed data",
          subtitle = "Outliers in continuous variables")

ggsave(path = FILENAMEbase, filename = paste0(FILENAME, "_contvars_dist_raw.png"),
       p.outliers)

# Fix outliers
abndata <- abndata %>%
  # Remove IA size outliers
  filter(IAsize_log < 4) %>%
  # Clean up age values
  filter(AgeDiag != 0)

# Fix NAs introduced by coercion.
# Error in abn::build_score_cache_mle for multinomial case:
# Error in { : task 2 failed - "object 'fit' not found"
# For some reason, a strange reformatting for Y occurs just before
# https://git.math.uzh.ch/reinhard.furrer/abn/-/blob/master/R/build_score_cache_mle.R#L306
# The NAs introduced by coercion warning can be reproduced with:
for (c in seq(1:ncol(abndata))){
  print(colnames(abndata)[c])
  print(as.numeric(as.character(abndata[,c])))
}
as.numeric(as.character(abndata$IA_Location))
typeof(abndata$IA_Location)
# maybe a workaround is converting the factors to integers first.
abndata <- abndata %>%
  mutate(across(where(is.factor), function(x){as.factor(as.integer(x))})) # also converting them back to integers still doesn't work...
str(abndata)
# Then the following code for factors has to be commented:
#
# # Reorder Levels
# abndata <- abndata %>%
#   mutate(Gender = forcats::fct_relevel(Gender, c("Male", "Female")))%>%
#   mutate(Positive.famillial.history = forcats::fct_relevel(Positive.famillial.history, c("No", "Yes")))%>%
#   # mutate(AgeDiag.group = fct_relevel(AgeDiag.group, LETTERS[1:length(levels(AgeDiag.group))])) %>%
#   mutate(Hypertension = forcats::fct_relevel(Hypertension, c("Never", "AnyType")))%>%
#   mutate(Smoking_Current_Former_No = forcats::fct_relevel(Smoking_Current_Former_No, c("No", "Former", "Current")))%>%
#   # mutate(location.grouped = forcats::fct_relevel(location.grouped, c( "Low", "Medium",  "High"))) %>%
#   # mutate(IAsize.groups = fct_relevel(IAsize.groups, LETTERS[1:length(levels(IAsize.groups))]))%>%
#   mutate(Multiple.IAs = forcats::fct_relevel(Multiple.IAs, c("No", "Yes")))%>%
#   mutate(Ruptured_IA = forcats::fct_relevel(Ruptured_IA, c("No", "Yes")))
#
# # Plot continuous vars again
# df <- abndata %>%
#   mutate(ID = seq(1,nrow(abndata)))%>%
#   reshape2::melt(id.vars=c("ID"),
#                  measure.vars=c("AgeDiag", "IAsize_log"))
#
# p.nooutliers <- ggplot(df, aes(x=value))+
#   facet_wrap(~variable, scales = "free")+
#   geom_boxplot(notch = TRUE) +
#   coord_flip() +
#   theme_bw() +
#   ggtitle("Raw multinomial mixed data",
#           subtitle = "Outliers removed in continuous variables")
#
# ggsave(path = FILENAMEbase, filename = paste0(FILENAME, "_contvars_dist_noOutliers.png"),
#        p.nooutliers)
#
# p.outliers.comb <- cowplot::plot_grid(p.outliers, p.nooutliers, labels = "AUTO", ncol = 1)+
#   theme(plot.caption = element_blank(),
#         plot.caption.position = "plot",
#         panel.background = element_rect(fill = "transparent", color = NA),
#         plot.background = element_rect(fill = "transparent", colour = NA))
# ggsave(path = FILENAMEbase, filename = paste0(FILENAME, "_contvars_dist_outliers_multiplt.png"),
#        p.outliers.comb)

# create empty retain matrix
retain <- matrix(0, ncol(abndata), ncol(abndata))
colnames(retain) <- rownames(retain) <- names(abndata)



#####
# Find optimal no. of parent nodes
#####
cat(paste(
  "\nRun the exact search across incremental parent limits with method:",
  METHOD
))

novars <- ncol(abndata)
# tmpscores <- vector(length = novars) # TODO: Remove?
net.scores <-
  data.frame(npar = NULL,
             scoretype = NULL,
             scorevalue = NULL)

clust <-
  parallel::makeCluster(n.cores, outfile = paste0(FILENAMEbase, "multicoreABNmaxpar.log"))
doParallel::registerDoParallel(cl = clust)
net.scores <- foreach(
  i = 1:novars,
  .combine = 'rbind',
  .packages = c("abn", "mcmcabn"),
  .inorder = TRUE
) %dopar% {
  max.par <- i
  mycache <- abn::buildScoreCache(
    data.df = as.data.frame(abndata),
    data.dists = dist,
    dag.banned = banned,
    dag.retained = retain,
    max.parents = max.par,
    method = METHOD
  )

  dag.mP <- abn::mostProbable(score.cache = mycache,
                         score = SCORE)
  fabn.mP <- abn::fitAbn(object = dag.mP,
                    method = METHOD)


  return(list(i=list(dag.mP, c(i, SCORE, fabn.mP[[SCORE]]))))

  cat(paste("\nnetwork score for", i, "parents =", fabn.mP[[SCORE]], "\n\n"))
}
stopCluster(clust)

net.scores.dags.prelim <- unlist(net.scores, recursive = F)
net.scores.dags <- net.scores.dags.prelim[seq(1,length(net.scores.dags.prelim), 2)]
net.scores <- net.scores.dags.prelim[seq(2,length(net.scores.dags.prelim), 2)]

# format net.scores
net.scores <- data.frame(npar = unlist(net.scores)[seq(1,length(unlist(net.scores)), 3)],
                            scoretype = unlist(net.scores)[seq(2,length(unlist(net.scores)), 3)],
                            scorevalue = unlist(net.scores)[seq(3,length(unlist(net.scores)), 3)])
rownames(net.scores) <- NULL
net.scores[, 1] <- as.integer(net.scores[, 1])
net.scores[, 3] <- as.numeric(net.scores[, 3])

for (i in 1:nrow(net.scores)) {
  if (net.scores$scoretype[i] == "bic") {
    net.scores$scorevalue[i] <- -net.scores$scorevalue[i]
  }
}

# Save intermediate raw data
save(
  abndata,
  dist,
  retain,
  banned,

  net.scores,
  net.scores.dags,
  file = paste0(FILENAMEbase, FILENAME, "_intermediate.RData")
)
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

mycache.maxpar <- buildScoreCache(
  data.df = as.data.frame(abndata),
  data.dists = dist,
  dag.banned = banned,
  dag.retained = retain,
  max.parents = max.par,
  method = METHOD
)

dag.maxpar <- mostProbable(score.cache = mycache.maxpar,
                           score = SCORE)
fabn.maxpar <- fitAbn(object = dag.maxpar,
                      method = METHOD)

endtime <- Sys.time()
cat(paste("\nEnd ABN with max.par. Time used [h]:", round(
  difftime(endtime, starttime, units = "hours"), 2
)))

# save unpruned ABN DAG
png(filename = paste0(FILENAMEbase, FILENAME, "_intermediateABNDAG.png"))
abn2bnlearn.plot(abndata, dag.maxpar, title = "prelim. ABN DAG with max. parents.")
dev.off()

#####
# MCMC ABN
#####
cat("\nStart MCMCabn...")
starttime <- Sys.time()

clust <-
  parallel::makeCluster(length(MCMC.SEEDS),
                        outfile = paste0(FILENAMEbase, "multicoreMCMCABN.log"))
doParallel::registerDoParallel(cl = clust)
mcmc.out.list <- foreach(
  SEED = MCMC.SEEDS,
  .packages = c("abn", "mcmcabn"),
  .inorder = TRUE
) %dopar% {
  mcmcabn(
    score.cache = mycache.maxpar,
    score = SCORE,
    data.dists = dist,
    max.parents = max.par,
    mcmc.scheme = MCMC.SCHEME,
    seed = SEED,
    verbose = FALSE,
    start.dag = dag.maxpar$dag,
    prob.rev = PROB.REV,
    # REV and MBR are efficient in producing high scoring structure but computationally costly compared to classical MCMC jumps.
    prob.mbr = PROB.MBR,
    prior.choice = MCMC.PRIOR
  ) # Koivisto prior
}
stopCluster(clust)

endtime <- Sys.time()
cat(paste("\nEnd MCMC ABN. Time used [h]:", round(
  difftime(endtime, starttime, units = "hours"), 2
)))


### Save raw data
save(list = ls(),
     file = paste0(FILENAMEbase, FILENAME, "_final.RData"))
cat("Final data written to:",
    paste0(FILENAMEbase, FILENAME, "_final.RData"))
