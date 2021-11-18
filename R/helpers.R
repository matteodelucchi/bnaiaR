#' Prepare Experiment specific data sets
#'
#' @param dat ADB preprocessed
#' @param EXPNO integer for experiment number.
#' @param age Type of variable: "cont", "disc.grouped-multinomial", "disc.grouped-binomial"
#' @param location Type of variable: "byVessel-multinomial", "byVessel-binomial", "byRisk-multinomial", "byRisk-binomial"
#' @param size Type of variable: "grouped-multinomial", "grouped.merged-multinomial", "log", "grouped-binomial","grouped.merged-binomial"
#' @param smoking Type of variable: "mult", "binCFN", "binCnC"
#' @param SAVE if TRUE, it the data sets were saved in FILENAMEbase
#' @param FILENAMEbase Place to store the files.
#'
#' @return Three data sets: Experiment data, banned matrix, list of distributions, Blacklist
#' @export
#'
#' @importFrom stringr str_subset str_which
#' @importFrom purrr is_empty
#'
#' @examples
#' \dontrun{prep_exp_data(dat = adb,
#' EXPNO = 06,
#' age = "cont",
#' location = "byRisk-binomial",
#' size = "log",
#' SAVE = T,
#' FILENAMEbase = "./data/")
#' }
prep_exp_data <- function(dat = adb,
                          EXPNO,
                          age,
                          location,
                          size,
                          smoking = "binCFN",
                          SAVE = T,
                          FILENAMEbase = NULL) {

  if (SAVE){
    if(!is.null(FILENAMEbase)) {
    FILENAME <- paste0(FILENAMEbase, "exp", EXPNO, "_data.RData")
  } else {
    warning("Don't know where to store data. Specify FILENAMEbase.")
  }} else {
    warning("I won't save the output as file.")
  }

  #####
  # Variables of interest
  #####
  cat(paste0("\nSelect variables of interest for experiment: ", EXPNO))
  abndata <- dat %>%
    select(-c(### Always remove:
      ID_2, # not informative
      db, # not informative
      Source, # Always the same for Geneva
      Basis.of.recruitement, # Always the same for Geneva
      # IA_Location, # too many levels. Let multinomial framework crash.
      IAsize # No suitable distribution available.
    ))

  ### non-modifiable risk factors:
  if (age == "cont") {
    nonmod <- names(abndata)[names(abndata) == "AgeDiag"]
  } else if (age == "disc.grouped-multinomial") {
    nonmod <- names(abndata)[names(abndata) == "AgeDiag.group"]
  } else if (age == "disc.grouped-binomial") {
    nonmod <- str_subset(names(abndata), pattern = "AgeDiag.group__")
  } else {
    warning(paste("age is ", age, "but must be one of: ",
                  "cont",
                  "disc.grouped-multinomial",
                  "disc.grouped-binomial"))
  }
  nonmod <- c(str_subset(names(abndata), pattern = "Gender|famillial"),
              nonmod)

  ### modifiable risk factors:
  modfact <-
    str_subset(names(abndata), pattern = "Hypertension")

  if (smoking == "mult") {
    modfact <- c(modfact,
                 names(abndata)[names(abndata) == "Smoking_Current_Former_No"])
  } else if (smoking == "binCFN") {
    modfact <- c(modfact,
                 str_subset(names(abndata), pattern = "Smoking_Current_Former_No__"))
  } else if (smoking == "binCnC") {
    modfact <- c(modfact,
                 str_subset(names(abndata), pattern = "Smoking_CurrentNotCurrent"))
  } else {
    warning(paste(
      "smoking is ", smoking, "but must be one of: ",
      "mult",
      "binCFN",
      "binCnC"
    ))
  }


  ### Aneurysm Properties
  if (location == "byVessel-multinomial") {
    IAprops.loc <- names(abndata)[names(abndata) == "IA_Location"]
  } else if (location == "byVessel-binomial") {
    IAprops.loc <- str_subset(names(abndata), pattern = "IA_Location__")
  } else if (location == "byRisk-multinomial") {
    IAprops.loc <- names(abndata)[names(abndata) == "location.grouped"]
  } else if (location == "byRisk-binomial") {
    IAprops.loc <-
      str_subset(names(abndata), pattern = "location.grouped__")
  } else {
    warning(paste(
      "location is ", location, "but must be one of: ",
      "byVessel-multinomial",
      "byVessel-binomial",
      "byRisk-multinomial",
      "byRisk-binomial"
    )
    )
  }

  if (size == "grouped-multinomial") {
    IAprops.size <- names(abndata)[names(abndata) == "IAsize.groups"]
  } else if (size == "grouped.merged-multinomial") {
    IAprops.size <-
      names(abndata)[names(abndata) == "IAsize.groups.merged"]
  } else if (size == "log") {
    IAprops.size <- names(abndata)[names(abndata) == "IAsize_log"]
  } else if (size == "grouped-binomial") {
    IAprops.size <-
      str_subset(names(abndata), pattern = "IAsize.groups__")
  } else if (size == "grouped.merged-binomial") {
    IAprops.size <-
      str_subset(names(abndata), pattern = "IAsize.groups.merged__")
  } else {
    warning(paste(
      "size is ", size, "but must be one of: ",
      "grouped-multinomial",
      "grouped.merged-multinomial",
      "log",
      "grouped-binomial",
      "grouped.merged-binomial"
    )
    )
  }

  IAprops <-
    c(IAprops.loc, IAprops.size, "Multiple.IAs") # Put them all together in the correct order

  ### Target
  modeltarget <- "Ruptured_IA"

  ### Put them all together in the correct order
  varsofinterest <- c(nonmod, modfact, IAprops, modeltarget)

  #####
  # select variables from data set
  #####
  abndata <- abndata %>%
    select(varsofinterest)

  #####
  # set distributions
  #####
  # all possible distributions
  dists <- list(
    ### non-modifiable risk factors:
    Gender = "binomial",
    Positive.famillial.history = "binomial",
    # Age continuous
    AgeDiag = "gaussian",
    # Age discrete binomial
    AgeDiag.group__A = "binomial",
    AgeDiag.group__B = "binomial",
    AgeDiag.group__C = "binomial",
    AgeDiag.group__D = "binomial",
    AgeDiag.group__E = "binomial",
    AgeDiag.group__F = "binomial",
    AgeDiag.group__G = "binomial",
    # Age discrete multinomial
    AgeDiag.group = "multinomial",

    ### modifiable risk factors
    Smoking_Current_Former_No = "multinomial",
    Smoking_Current_Former_No__Current = "binomial",
    Smoking_Current_Former_No__Former = "binomial",
    Smoking_Current_Former_No__No = "binomial",
    Smoking_CurrentNotCurrent = "binomial",
    Hypertension = "binomial",

    ### Aneurysm Properties
    # Location grouped multinomial
    location.grouped = "multinomial",
    # Location (exp01-03, 07-08)
    IA_Location__A1_segment_ant = "binomial",
    IA_Location__A2 = "binomial",
    IA_Location__Acom = "binomial",
    IA_Location__Basilar = "binomial",
    IA_Location__CavICA = "binomial",
    IA_Location__ICA = "binomial",
    IA_Location__MCA = "binomial",
    IA_Location__OphtICA = "binomial",
    IA_Location__Other = "binomial",
    IA_Location__PC = "binomial",
    IA_Location__Pcom = "binomial",
    IA_Location__VB = "binomial",
    #Location grouped binomial (exp04-63, 10-12)
    location.grouped__High = "binomial",
    location.grouped__Low = "binomial",
    location.grouped__Medium = "binomial",
    # Size grouped binomial
    IAsize.groups__A = "binomial",
    IAsize.groups__B = "binomial",
    IAsize.groups__C = "binomial",
    IAsize.groups__D = "binomial",
    # Size grouped multinomial
    IAsize.groups = "multinomial",
    # Size grouped merged binomial
    IAsize.groups.merged__A = "binomial",
    IAsize.groups.merged__B = "binomial",
    IAsize.groups.merged__C = "binomial",
    # Size grouped merged multinomial
    IAsize.groups.merged = "multinomial",
    # log(size)
    IAsize_log = "gaussian",
    Multiple.IAs = "binomial",

    ### Target
    Ruptured_IA = "binomial"
  )

  # select vars of interest from all possible distributions
  dist <- dists[varsofinterest]

  # Check distributions
  ## Create network matrix
  dag <- matrix(0, ncol(abndata), ncol(abndata))
  colnames(dag) <- rownames(dag) <- names(abndata)

  ## Check names
  cat("\nCheck if all variables have a distribution assigned... ")
  testnames_dist <-
    names(dist)[which(!(names(dist) %in% names(abndata)))]
  testnames_abndata <-
    names(abndata)[which(!(names(abndata) %in% names(dist)))]
  if (is_empty(testnames_dist) & is_empty(testnames_abndata)) {
    cat("ok")
  } else {
    print(paste("Not ok. "))
    if (!is_empty(testnames_dist)) {
      warning(paste(
        "Present in dist list but missing in abndata list:",
        testnames_dist
      ))
    } else {
      warning(paste(
        "Present in abndata but missing in dist list:",
        testnames_abndata
      ))
    }
  }

  #####
  # make banned and blacklist
  #####
  ## Create retain and banned matrixes (empty)
  cat("\nCreate retain and bann matrices.")
  retain <- matrix(0, ncol(abndata), ncol(abndata))
  colnames(retain) <- rownames(retain) <- names(abndata)

  banned <- matrix(0, ncol(abndata), ncol(abndata))
  colnames(banned) <- rownames(banned) <- names(abndata)

  ### Ban some arcs
  gend_idx <- str_which(names(abndata), "Gender")
  fam_idx <-
    str_which(names(abndata), "Positive.famillial.history")
  age_idx <- str_which(names(abndata), "AgeDiag")
  smok_idx <- str_which(names(abndata), "Smoking")
  hyp_idx <- str_which(names(abndata), "Hypertension")
  loc_idx <- str_which(names(abndata), "location|IA_Location")
  size_idx <- str_which(names(abndata), "IAsize")
  multi_idx <- str_which(names(abndata), "Multiple.IAs")
  rupt_idx <- str_which(names(abndata), "Ruptured_IA")

  banned[gend_idx, -gend_idx] <- 1 # Nothing pointing to Gender
  banned[fam_idx, -c(gend_idx, fam_idx, age_idx)] <- 1 # Nothing pointing to fam. history, except age
  banned[age_idx, -c(gend_idx, fam_idx, age_idx)] <- 1 # Nothing pointing to age, except pos.fam and sex
  ##### Nothing from behavioral to hereditary
  banned[c(gend_idx, fam_idx, age_idx), smok_idx] <- 1 # Smoking -> c(Gender, Pos.fam, age)
  banned[c(gend_idx, fam_idx, age_idx), hyp_idx] <- 1 # Hypertension -> c(Gender, pos.fam, age)
  ##### Nothing from aneurysm properties to behavioral or hereditary
  banned[c(gend_idx, fam_idx, age_idx, smok_idx, hyp_idx), loc_idx] <- 1 # Location -> c(smoking, hypertension, gender, pos.fam, age)
  banned[c(gend_idx, fam_idx, age_idx, smok_idx, hyp_idx), size_idx] <- 1 # IA size -> c(smoking, hypertension, gender, pos.fam, age)
  banned[c(gend_idx, fam_idx, age_idx, smok_idx, hyp_idx), multi_idx] <- 1 # IA Mult. -> c(smoking, hypertension, gender, pos.fam, age)
  banned[multi_idx, loc_idx] <- 1 # Location -> Multiplicity
  banned[multi_idx, size_idx] <- 1 # Size -> Multiplicity
  # ##### Block all inter-Smoking
  banned[smok_idx, smok_idx] <- 1
  # ##### Block all inter-location
  banned[loc_idx, loc_idx] <- 1
  # ##### Block all inter-size
  banned[size_idx, size_idx] <- 1
  # ##### Block all inter-age
  banned[age_idx, age_idx] <- 1
  banned[-rupt_idx, rupt_idx] <- 1 # Rupture doesn't point to anything

  diag(banned) <- 0 # allow all variables on them self
  # print(
  #   "Bann-Matrix: if value = 1 then do not allow the arc from column to row. I.e. Gender -> Age is banned."
  # )
  # print(banned)

  ## Test run
  tryCatch(
    mycache <- abn::buildScoreCache(
      data.df = as.data.frame(abndata),
      data.dists = dist,
      dag.banned = banned,
      dag.retained = retain,
      max.parents = 1,
      method = "mle"
    )
  )

  ## banned -> blacklist
  cat("\ntransform banned matrix to blacklist")
  bl <-
    subset(as.data.frame(as.table(t(banned))), Freq > 0)[, -3]
  colnames(bl) <- c("From", "To")
  rownames(bl) <- NULL
  bl

  #####
  # save them all
  #####
  cat("\nFinished data preparation.")
  if (SAVE) {
    save(abndata, dist, banned, bl,
         file = FILENAME)
    cat(paste0("\nSaved data sets at ", FILENAME))
  } else{
    return(list(abndata=abndata, dist=dist, banned=banned, bl=bl))
  }
}

#' Summarise data processing steps required to plot structure learning metric
#'
#' @param eval DataFrame output of network.metrics
#' @param newnames vector of character strings with names to display in plot.
#' The order must match to the order of names(eval)!
#'
#' @return tibble which can be further processed to plot
#' @export
prep.data2plot <- function(eval, newnames = NULL){
  # Prepare named vector with (old-names, new-names)
  # specifying names in input df and returned tibble respectively
  if (is.null(newnames)){
    NEWNAMES <- c("Algorithm", "tabu list size",
                  "bootstrap replicates",
                  "score function", "Network Score", "Network Score Ratio",
                  "prior", "Threshold arc strength", "no. arcs", "no. sign. arcs",
                  # "SHD Th=0", "SHD", "SHD Constantinou",
                  "SHD Th=0", "SHD",
                  "TP arcs", "TP arcs Th=0",
                  "FPR arcs", "TPR arcs",
                  "FPR arcs Th=0", "TPR arcs Th=0",
                  "ACC arcs", "ACC arcs Th=0",
                  "Balanced Scoring Function", "Balanced Scoring Function Th=0",
                  "Balanced Scoring Function norm", "Balanced Scoring Function Th=0 norm",
                  "Bayesian Factor", "Bayesian Factor Th=0")
  } else {
    NEWNAMES <- newnames
  }


  if(length(names(eval)) != length(NEWNAMES)){
    stop(paste("Names of", quote(eval),"and", quote(newnames), "must have the same length but are", length(names(eval)), "and", length(NEWNAMES), "respectively."))
  }

  eval.prep <- eval %>%
    # t() %>%
    # print() %>%
    data.table::as.data.table() %>%
    dplyr::rename(setNames(names(.), NEWNAMES)) %>% # "old-names" = "new-names"
    dplyr::select_all(~gsub("\\s+|\\.", ".", .)) %>% # replace space and period to "."
    dplyr::mutate(
      # prior = case_when(prior == "uniform" ~ 1,
      #                        prior == "marginal" ~ 2,
      #                        prior == "vsp" ~ 3,
      #                        prior == "cs" ~ 4),
      score.function = case_when(score.function == "bic" ~ 1,
                                 score.function == "mutual-information" ~ 2,
                                 score.function == "hybrid" ~ 3))
  return(eval.prep)
}

#' Prediction Error Metrics
#'
#' From cross-validation object, calculate a set of prediction error metrics.
#'
#' @param xval object returned from `bnlearn::bn.cv()`
#'
#' @return list of error metrics.
#' @export
#'
#' @examples
#' \dontrun{xval.rupture.bic <- bn.cv(
#' data = data,
#' bn = avgnet$tabu.th03,
#' method = "hold-out",
#' runs = 50,
#' fit = "mle",
#' loss = "pred",
#' loss.args = list(target = "Ruptured_IA")
#' )
#' cv.metrics(xval.rupture.bic)}
cv.metrics <- function(xval){
  OBS = unlist(lapply(unlist(xval, recursive = F), `[[`, "observed"))
  PRED = unlist(lapply(unlist(xval, recursive = F), `[[`, "predicted"))
  conf.mat <- table(OBS, PRED)
  tpr <-conf.mat[1,1]/(conf.mat[1,1]+conf.mat[1,2])
  fpr <-conf.mat[2,1]/(conf.mat[2,1]+conf.mat[2,2])
  acc <- (conf.mat[1,1]+conf.mat[2,2])/(conf.mat[1,1]+conf.mat[2,1]+conf.mat[1,2]+conf.mat[2,2])
  return(list(conf.mat = conf.mat, tpr = tpr, fpr = fpr, acc = acc))
}

#' Burn-in Phase after MCMCABN
#'
#' Speed up computation time of mcmcabn() and gain flexibility by removing the
#' first n steps after the MCMC run.
#'
#' @param mcmc.out.list output of mcmcabn()
#' @param burnin.length int. of number of steps to remove.
#'
#' @return Burned mcmcabn() output.
#' @export
postBURNin <- function(mcmc.out.list, burnin.length){
  for (d in 1:length(mcmc.out.list)){
    mcmc.out.list[[d]]$burnin <- burnin.length
    mcmc.out.list[[d]]$dags <- mcmc.out.list[[d]]$dags[,,-(1:burnin.length)]
    mcmc.out.list[[d]]$scores <- mcmc.out.list[[d]]$scores[-(1:burnin.length)]
    mcmc.out.list[[d]]$alpha <- mcmc.out.list[[d]]$alpha[-(1:burnin.length)]
    mcmc.out.list[[d]]$method <- mcmc.out.list[[d]]$method[-(1:burnin.length)]
    mcmc.out.list[[d]]$rejection <- mcmc.out.list[[d]]$rejection[-(1:burnin.length)]
    mcmc.out.list[[d]]$heating <- mcmc.out.list[[d]]$heating[-(1:burnin.length)]
  }
  return(mcmc.out.list)
}

#' Thinn MCMC sample after MCMCABN
#'
#' Speed up computation time of mcmcabn() and gain flexibility by thinning
#' after the MCMC run.
#' Thinning keeps all kth step.
#'
#' @param mcmc.out.list output of mcmcabn()
#' @param thinningsteps int. Thinning factor.
#'
#' @return Thinned mcmcabn() output.
#' @export
postTHINN <- function(mcmc.out.list, thinningsteps){
  thin <- c(TRUE, rep(FALSE, thinningsteps-1)) # see recycle rules
  mcmc.out.list.thinned <- mcmc.out.list

  for (d in 1:length(mcmc.out.list.thinned)){
    if(thinningsteps > mcmc.out.list.thinned[[d]]$iterations){
      stop(paste0("More thinning steps (", thinningsteps, ") than mcmc iterations (", mcmc.out.list.thinned[[d]]$iterations, ")."))
    }
    mcmc.out.list.thinned[[d]]$thinning <- thinningsteps
    mcmc.out.list.thinned[[d]]$dags <- mcmc.out.list.thinned[[d]]$dags[,,thin]
    mcmc.out.list.thinned[[d]]$scores <- mcmc.out.list.thinned[[d]]$scores[thin]
    mcmc.out.list.thinned[[d]]$alpha <- mcmc.out.list.thinned[[d]]$alpha[thin]
    mcmc.out.list.thinned[[d]]$method <- mcmc.out.list.thinned[[d]]$method[thin]
    mcmc.out.list.thinned[[d]]$rejection <- mcmc.out.list.thinned[[d]]$rejection[thin]
  }
  return(mcmc.out.list.thinned)
}

#' Am I running local or on some Server?
#'
#' If running on some server where other users than the specified username
#' are logged in, this function returns FALSE.
#' If running on a system with only the specified user, it returns TRUE.
#'
#' @param myuser character string of system user name.
#'
#' @return TRUE if only 'myuser' is logged in the current system. FALSE otherwise.
#' @export
amilocal <- function(myuser="delt"){
  who <- system("who", intern = T)
  users <- c()
  for (i in 1:length(who)){
    users <- c(users, stringr::str_extract(who[i], "^.{4}"))
  }
  if (dim(table(users))==1){
    cat("running script locally.\n")
    return(TRUE)
  } else{
    cat("Other registered users:\n")
    print((as.data.frame(table(users))))
    return(FALSE)
  }
}


#' Arc strength significance threshold
#'
#' Compute the significance threshold for Friedman's confidence.
#' Deliberately copied and adapted from bnlearn source code on CRAN Github
#' https://github.com/cran/bnlearn/blob/0e4d3af6ad579b79bc9959e77385ec6b825ca6fc/R/arc.strength.R#L420
#'
#' @param strength Arc frequency of consensus DAG.
#' @param method Currently only "l1" supported.
#'
#' @return arc strength threshold
#' @export
#'
#' @examples
#' \dontrun{dag.mcmc.boot.stren <- as.vector(round(dag.mcmc.boot, 3))
#' arc.stren.sign.threshold <-arc.stren.threshold(dag.mcmc.boot.stren)
#' # Plot relative arc strength
#' plot(ecdf(dag.mcmc.boot.stren))
#' # Draw arc strength threshold
#' abline(v = arc.stren.sign.threshold, lty=2)
#' # Draw 50% threshold mark
#' abline(v=0.5)
#' }
arc.stren.threshold = function(strength, method = "l1") {

  # do not blow up with graphs with only 1 node.
  if (length(strength) == 0)
    return(0)

  e = ecdf(strength)
  u = knots(e)

  if (method == "l1") {

    norm = function(p)
      sum( diff(unique(c(0, u, 1))) * abs(e(unique(c(0, u[u < 1]))) - p))

  }#THEN

  p0 = optimize(f = norm, interval = c(0, 1))$minimum

  # double-check the boundaries, they are legal solutions but optimize() does
  # not check them.
  if (norm(1) < norm(p0))
    p0 = 1
  if (norm(0) < norm(p0))
    p0 = 0

  quantile(strength, p0, type = 1, names = FALSE)

}

#' Plot ABN DAG with `bnlearn::graphviz.plot`
#'
#' @param data Data frame used to learn abn DAG
#' @param abndag List from output of `abn::mostProbable()`
#' @param title string for title
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' mycache = buildScoreCache(data.df = data, data.dists = dists, max.parents = npar, method = "mle",
#' dag.banned = banned, verbose = FALSE)
#' dag.mP = mostProbable(score.cache = mycache, score = "bic", verbose = FALSE)
#' abn2bnlearn.plot(data, dag.mP)
#' }
abn2bnlearn.plot <- function(data, abndag, title){
  dag = bnlearn::empty.graph(names(data))
  bnlearn::amat(dag) = t(abndag$dag)
  bnlearn::graphviz.plot(dag,
                         shape = "rectangle",
                         main = title)
}

