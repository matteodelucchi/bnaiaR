#' Balanced Scoring Function (BSF)
#'
#' The BSF score can be used in conjunction with other traditional metrics to
#' provide an alternative and unbiased assessment about the capability of a
#' structure learning algorithm in discovering causal or BN graphs.
#'
#' @param estGraph Object of class "bn" representing the estimated network structure.
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#'
#' @references Constantinou et al. 2020 http://arxiv.org/abs/1905.12666
#' @return integer
#' @export
#'
BSF <- function(estGraph, trueGraph){
  N <- length(bnlearn::nodes(trueGraph))
  a <- nrow(bnlearn::arcs(trueGraph))
  i <- (N*(N-1))/2 -a

  tn <- a - N - bnlearn::compare(trueGraph, estGraph)$fn
  fp <- bnlearn::compare(trueGraph, estGraph)$fp
  tp <- bnlearn::compare(trueGraph, estGraph)$tp
  fn <- bnlearn::compare(trueGraph, estGraph)$fn

  BSF <- ((tp/a)+(tn/i)-(fp/i)-(fn/a))/2
  return(BSF)
}

#' Structural Hamming Distance (Constantinou et al.)
#'
#' EXPERIMENTAL!
#' Doesn't account for WL/BL.
#'
#' @param estGraph Object of class "bn" representing the estimated network structure.
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#'
#' @references Constantinou et al. 2020 http://arxiv.org/abs/1905.12666
#' @return integer
#' @export
SHD.constantinou <- function(estGraph, trueGraph){
  fp <- bnlearn::compare(trueGraph, estGraph)$fp
  fn <- bnlearn::compare(trueGraph, estGraph)$fn
  return(fn+fp)
}

#' DAG Dissimilarity Metric (DDM)
#'
#' @param estGraph Object of class "bn" representing the estimated network structure.
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#'
#' @references Constantinou et al. 2020 http://arxiv.org/abs/1905.12666
#' @return integer
#' @export
DDM.constantinou <- function(estGraph, trueGraph){
  a <- nrow(bnlearn::arcs(trueGraph))
  r <- nrow(dplyr::anti_join(estGraph[,1:2], trueGraph[,1:2]))

  fp <- bnlearn::compare(trueGraph, estGraph)$fp
  tp <- bnlearn::compare(trueGraph, estGraph)$tp
  fn <- bnlearn::compare(trueGraph, estGraph)$fn
  ddm <- (tp+0.5*r-fn-fp)/a
  return(ddm)
}

#' F1 Score of Graphs
#'
#' @param estGraph Object of class "bn" representing the estimated network structure.
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#'
#' @references Constantinou et al. 2020 http://arxiv.org/abs/1905.12666
#' @return integer
#' @export
f1 <- function(estGraph, trueGraph){
  N <- length(bnlearn::nodes(trueGraph))
  a <- nrow(bnlearn::arcs(trueGraph))

  tn <- a - N - bnlearn::compare(trueGraph, estGraph)$fn
  fp <- bnlearn::compare(trueGraph, estGraph)$fp
  tp <- bnlearn::compare(trueGraph, estGraph)$tp
  fn <- bnlearn::compare(trueGraph, estGraph)$fn

  pr <- tp/(tp+fp)
  re <- tp/(tp+fn)
  f1 <- (2*pr*re)/(pr+re)
  return(f1)
}

#' Network Score Ratio
#'
#' Structure learning maximizes the score function.
#' The quality of the learned network is its score.
#' To compare resulting networks from structure learning, on their network size
#' and average degree (measure of sparseness), Perrier et al. 2008 suggested to
#' use the score ratio of the "Optimal-Structure" over the "Learned-Structure".
#' This allows to compare network score across different parameters, as score
#' values heavily vary upon changing parameters.
#'
#' The closer to 1, the better.
#'
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#' @param estGraph Object of class "bn" representing the estimated network structure.
#' @param data DataFrame supplied to the structure learning algorithm.
#' @param crt character string of the criterion/score used by the structure learning algorithm.
#'
#' @references Perrier, Eric, Seiya Imoto, and Satoru Miyano.
#' “Finding Optimal Bayesian Network Given a Super-Structure.”
#' Journal of Machine Learning Research 9, no. 74 (2008): 2251–86.
#' http://jmlr.org/papers/v9/perrier08a.html.
#' @return integer
#' @export
score.ratio <- function(trueGraph, estGraph, data, crt){
  if (nrow(bnlearn::undirected.arcs(trueGraph)) != 0){
    warning("Undirected arcs in trueGraph: Can not compute score.ratio.")
    return(NA)
  } else {
    score.rat <- bnlearn::score(trueGraph, data, type = crt) /  bnlearn::score(estGraph, data, type = crt)
    return(score.rat)
  }
}

#' Calculate Network metrics during parameter analysis
#'
#' See details bewlow for a list of the currently implemented network metrics.
#'
#'
#' @param data DataFrame supplied to the structure learning algorithm.
#' @param estGraph Object of class "bn" representing the estimated network structure on the estimated confidence level.
#' @param estGraph.th0 Object of class "bn" representing the estimated network structure on the estimated confidence level set to zero.
#' @param algo character string of the structure learning algorithm.
#' @param trueGraph Object of class "bn" representing the domain knowledge as graph ("intuition graph" or "ground truth graph").
#' @param tabulistsize integer if tabu algorithm was used, tabu list size.
#' @param b integer. Number of bootstrap replicates
#' @param crt character string of the criterion/score used by the structure learning algorithm.
#' @param priorname charachter string of the prior.
#'
#' @return DataFrame of various network metrics.
#'
#' @details Currently implemented network metrics.
#' \itemize{
#'  \item{Score/Criterion}{Score/Criterion used by SL Algorithm}
#'  \item{Netscore}{Value of Network score}
#'  \item{prior}{Prior applied to network score}
#'  \item{threshold}{Arc strength/confidence threshold}
#'  \item{Number of arcs}{Number of detected arcs including arcs below threshold}
#'  \item{Significant arcs}{Number of significant arcs.}
#'  }
#' If a reference graph is supplied the following metrics are calculated:
#' \itemize{
#'  \item{True negatives}{}
#'  \item{True positives}{}
#'  \item{False negatives}{}
#'  \item{False positives}{}
#'  \item{False positive rate}{}
#'  \item{True positive rate}{}
#'  \item{Accuracy}{}
#'  \item{Structural Hamming Distance (SHD)}{}
#'  \item{Balanced Scoring Function}{}
#'  \item{Bayesian factor}
#' }
#'
#' @importFrom bnlearn BF
#' @export
network.metrics <- function(data, estGraph, estGraph.th0, algo, tabulistsize=NULL, b=NULL, trueGraph=NULL, crt=NULL, priorname=NULL){
  allowed_scores <- c("bic", "aic", "bde", "bds", "bdj", "mbde", "bdla", "k2")
  # Handle argument values for print out
  if(!is.null(crt)){
    scorefct <- crt
  } else{
    scorefct <- NA
  }

  if (!is.null(priorname)){
    prior <- priorname
  } else {
    prior <- NA
  }

  if (!is.null(tabulistsize)){
    tabusize <- tabulistsize
  } else {
    tabusize <- NA
  }

  if (!is.null(b)){
    b <- b
  } else {
    b <- NA
  }

  if (!is.null(algo)){
    algo <- algo
  } else {
    algo <- NA
  }

  # Score-function and prior dependent metrics
  if(!is.null(crt) & (crt %in% allowed_scores)){
    # If value for crt in allowed scores
    if (!is.null(priorname)){
      # if value for priorname
      netscore <- bnlearn::score(estGraph, data,
                                 type = crt,
                                 prior = priorname)
    } else {
      # if no value for priorname
      netscore <- bnlearn::score(estGraph, data,
                                 type = crt)
    }
  } else if(!is.null(crt) & (crt %in% c("hybrid", "mutual-information"))){
    # set crt to default: BIC
    warning("BIC assumed for Network Score.")
    netscore <- bnlearn::score(estGraph, data,
                               type = "bic")
  } else {
    # if no value for crt or not in allowed scores
    netscore <- bnlearn::score(estGraph, data)
  }

  threshold <- estGraph$learning$args$threshold
  noarcs <- bnlearn::narcs(estGraph.th0)
  sigarcs <- bnlearn::narcs(estGraph)

  if(!is.null(trueGraph)){

    tn <- (length(bnlearn::arcs(trueGraph)) - length(bnlearn::nodes(trueGraph))) - bnlearn::compare(trueGraph, estGraph)$fn
    fp <- bnlearn::compare(trueGraph, estGraph)$fp
    tp <- bnlearn::compare(trueGraph, estGraph)$tp
    fn <- bnlearn::compare(trueGraph, estGraph)$fn
    fpr <- fp / (tn + fp)
    tpr <- tp / (tp + fn)
    acc <- (tp+tn)/(tp+tn+fp+fn)
    shd <- shd(estGraph, trueGraph, wlbl = TRUE)
    bsf <- BSF(estGraph, trueGraph)
    bsfn <- (bsf +1)/2

    tn.th0 <- (length(bnlearn::arcs(trueGraph)) - length(bnlearn::nodes(trueGraph))) - bnlearn::compare(trueGraph, estGraph.th0)$fn
    fp.th0 <- bnlearn::compare(trueGraph, estGraph.th0)$fp
    tp.th0 <- bnlearn::compare(trueGraph, estGraph.th0)$tp
    fn.th0 <- bnlearn::compare(trueGraph, estGraph.th0)$fn
    fpr.th0 <- fp.th0 / (tn.th0 + fp.th0)
    tpr.th0 <- tp.th0 / (tp.th0 + fn.th0)
    acc.th0 <- (tp.th0+tn.th0)/(tp.th0+tn.th0+fp.th0+fn.th0)
    shd.th0 <- shd(estGraph.th0, trueGraph, wlbl = TRUE)
    bsf.th0 <- BSF(estGraph.th0, trueGraph)
    bsf.th0n <- (bsf.th0+1)/2

    # Score-function and prior dependent metrics
    if(!is.null(crt) & (crt %in% allowed_scores)){
      # If value for crt in allowed scores
      netscore.ratio <- score.ratio(trueGraph, estGraph, data, crt)


      if (!is.null(priorname)){
        # if value for priorname
        bayesfactor <- tryCatch(
          BF(trueGraph, estGraph, data,
             score = crt,
             prior = priorname),
          error = function(e) {
            print(e)
            print("Returned NA.")
            return(NA)})
        bayesfactor.th0 <- tryCatch(
          BF(trueGraph, estGraph.th0, data,
             score = crt,
             prior = priorname),
          error = function(e) {
            print(e)
            print("Returned NA.")
            return(NA)})
      } else {
        # if no value for priorname
        bayesfactor <- tryCatch(
          BF(trueGraph, estGraph, data,
             score = crt),
          error = function(e) {
            print(e)
            print("Returned NA.")
            return(NA)})
        bayesfactor.th0 <- tryCatch(
          BF(trueGraph, estGraph.th0, data,
             score = crt),
          error = function(e) {
            print(e)
            print("Returned NA.")
            return(NA)})
      }
    } else if(!is.null(crt) & (crt %in% c("hybrid", "mutual-information"))){
      # set crt to default: BIC
      warning("BIC assumed for Network Score Ratio and Bayes Factor.")

      netscore.ratio <- score.ratio(trueGraph, estGraph, data, crt = "bic")

      bayesfactor <- tryCatch(
        BF(trueGraph, estGraph, data,
           score = "bic"),
        error = function(e) {
          print(e)
          print("Returned NA.")
          return(NA)})
      bayesfactor.th0 <- tryCatch(
        BF(trueGraph, estGraph.th0, data,
           score = "bic"),
        error = function(e) {
          print(e)
          print("Returned NA.")
          return(NA)})
    } else {
      # if no value for crt or not in allowed scores
      bayesfactor <-NA
      bayesfactor.th0 <- NA
      netscore.ratio <- NA
    }
    return(data.frame(algo,
                      tabusize,
                      b,
                      scorefct,
                      netscore, netscore.ratio,
                      prior,
                      threshold,
                      noarcs, sigarcs,
                      shd.th0, shd,
                      tp, tp.th0,
                      fpr, tpr, fpr.th0, tpr.th0,
                      acc, acc.th0,
                      bsf, bsf.th0,
                      bsfn, bsf.th0n,
                      bayesfactor, bayesfactor.th0))
  } else{
    # w/o trueGraph
    return(data.frame(algo,
                      tabusize,
                      b,
                      scorefct,
                      netscore,
                      prior,
                      threshold,
                      noarcs, sigarcs))
  }
}
