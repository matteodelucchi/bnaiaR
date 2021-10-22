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

#' Plot and save ROC curve with AUC
#'
#' Plots the ROC Curve with AUC and AUC CI values.
#'
#' @param roc object returned from `pROC::roc`
#' @param aucCI object returned from `pROC::ci.auc`
#' @param FILENAME string of filename
#' @param PLOTPATH string of path
#' @param SAVE If True, the plot will be saved.
#'
#' @return plot or none.
#' @export
#'
#' @examples
#' \dontrun{roc <- pROC::roc(ValidSet$Ruptured_IA, pred_glm)
#' auc <- roc$auc
#' aucCI <- round(pROC::ci.auc(ValidSet$Ruptured_IA, pred_glm), 2)
#' plotROCAUC(roc, aucCI, FILENAME = "logreg_IArupture_roc.png")
#' }
plotROCAUC <- function(roc, aucCI, FILENAME, PLOTPATH=NULL, SAVE=SAVEPLOTS){
  if (SAVE){
    if(is.null(PLOTPATH)){
      stop( "Don't know where to store. Specify PLOTPATH!")
    } else{
      FILE = paste0(PLOTPATH, "/", FILENAME)
      print(paste0("Saving to ", FILE))
    }
    png(filename = FILE)
    plot(roc, main = "ROC Curve")
    text(0.5, 0.01, paste("AUC=", aucCI[2], "(95% CI =", aucCI[1], "-", aucCI[3], ")"))
    dev.off()
  } else {
    plot(roc, main = "ROC Curve", )
    text(0.5, 0.01, paste("AUC=", aucCI[2], "(95% CI =", aucCI[1], "-", aucCI[3], ")"))
  }
}
