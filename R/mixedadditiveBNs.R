#' Find Optimal Number of Parent Nodes
#'
#' @param df data.frame with training data
#' @param dist list of variable = distribution type. Details in \code{?abn::buildScoreCache()}.
#' @param banned matrix with column variable as parent of row-variable, that defines which arcs are not permitted. Details in \code{?abn::buildScoreCache()}.
#' @param retain matrix with column variable as parent of row-variable, that defines which arcs are enforced. Details in \code{?abn::buildScoreCache()}.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param method either "Bayes" or "mle" approach. Details in \code{?abn::buildScoreCache()}.
#' @param no.cores integer of number of cores to parallelise.
#' @param score character of score that is used to score the network. Details in \code{?abn::mostprobable()}.
#'
#' @return list of df, dist, retain, banned, net.score, net.score.dags.
#' @export
#'
#' @examples \dontrun{
#' optnoparents42 <-
#' findOptNoParentNodes(
#'   df = df42,
#'   dist = dist42,
#'   banned = banned42,
#'   retain = retain42,
#'   filenamesuffix = "4.2"
#' )
#' }
findOptNoParentNodes <- function(df, dist, banned, retain, filenamesuffix, filenamebase = FILENAMEbase, filename = FILENAME, method = METHOD, no.cores = n.cores, score=SCORE){
  message(paste("\nRun the exact search across incremental parent limits with method:", METHOD))

  novars <- ncol(df)
  tmpscores <- vector(length = novars)
  net.scores <-
    data.frame(npar = NULL,
               scoretype = NULL,
               scorevalue = NULL)

  clust <-
    parallel::makeCluster(no.cores, outfile = paste0(filenamebase, filename, filenamesuffix, "_multicoreABNmaxpar.log"))
  doParallel::registerDoParallel(cl = clust)
  net.scores <- foreach(
    i = 1:novars,
    .combine = 'rbind',
    .packages = c("abn", "mcmcabn"),
    .inorder = TRUE
  ) %dopar% {
    max.par <- i
    mycache <- buildScoreCache(
      data.df = as.data.frame(df),
      data.dists = dist,
      dag.banned = banned,
      dag.retained = retain,
      max.parents = max.par,
      method = method
    )

    dag.mP <- mostProbable(score.cache = mycache,
                           score = score)
    fabn.mP <- fitAbn(object = dag.mP,
                      method = method)


    return(list(i=list(dag.mP, c(i, score, fabn.mP[[score]]))))

    cat(paste("\nnetwork score for", i, "parents =", fabn.mP[[score]], "\n\n"))
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
    df,
    dist,
    retain,
    banned,

    net.scores,
    net.scores.dags,
    file = paste0(filenamebase, filename, filenamesuffix, "_intermediate.RData")
  )
  cat("\nIntermediate data saved")
  return(list("df" = df,
              "dist" = dist,
              "retain" = retain,
              "banned" = banned,

              "net.scores" = net.scores,
              "net.scores.dags" = net.scores.dags))
}


#' Learn ABN with max parents.
#'
#' Requires output from \code{bnaiaR::findOptNoParentNodes()}.
#'
#' @param net.scores output from \code{bnaiaR::findOptNoParentNodes()$net.scores}
#' @param df data.frame with training data
#' @param dist list of variable = distribution type. Details in \code{?abn::buildScoreCache()}.
#' @param banned matrix with column variable as parent of row-variable, that defines which arcs are not permitted. Details in \code{?abn::buildScoreCache()}.
#' @param retain matrix with column variable as parent of row-variable, that defines which arcs are enforced. Details in \code{?abn::buildScoreCache()}.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param method either "Bayes" or "mle" approach. Details in \code{?abn::buildScoreCache()}.
#' @param no.cores integer of number of cores to parallelise.
#' @param score character of score that is used to score the network. Details in \code{?abn::mostprobable()}.
#'
#' @return list of df, dist, retain, banned, mycache.maxpar, max.par, dag.maxpar, fabn.maxpar
#' @export
#'
#' @examples \dontrun{
#' abnwithmaxpar42 <-
#'   abnwithmaxparents(
#'     net.scores = optnoparents42$net.scores,
#'     df = df42,
#'     dist = dist42,
#'     banned = banned42,
#'     retain = retain42,
#'     filenamesuffix = "4.2"
#'   )
#' }
abnwithmaxparents <-
  function(net.scores,
           df,
           dist,
           banned,
           retain,
           filenamesuffix,
           filenamebase = FILENAMEbase,
           filename = FILENAME,
           method = METHOD,
           no.cores = n.cores,
           score = SCORE) {
    cat("\nStart ABN with max.par...")
    starttime <- Sys.time()

    # Find number of parents with highest network score
    x <- net.scores %>%
      filter(scoretype == score) %>%
      mutate(npar = as.numeric(npar),
             scorevalue = as.numeric(scorevalue))
    max.par <- x$npar[which(x$scorevalue == max(x$scorevalue))][1]

    cat(paste("\nusing method:", method,
              "and max. parents:", max.par))

    mycache.maxpar <- buildScoreCache(
      data.df = as.data.frame(df),
      data.dists = dist,
      dag.banned = banned,
      dag.retained = retain,
      max.parents = max.par,
      method = method
    )

    dag.maxpar <- mostProbable(score.cache = mycache.maxpar,
                               score = score)
    fabn.maxpar <- fitAbn(object = dag.maxpar,
                          method = method)

    endtime <- Sys.time()
    cat(paste("\nEnd ABN with max.par. Time used [h]:", round(
      difftime(endtime, starttime, units = "hours"), 2
    )))

    # save unpruned ABN DAG
    png(filename = paste0(
      filenamebase,
      filename,
      filenamesuffix,
      "_intermediateABNDAG.png"
    ))
    abn2bnlearn.plot(df, dag.maxpar, title = "prelim. ABN DAG with max. parents.")
    dev.off()

    return(
      list(
        "df" = df,
        "dist" = dist,
        "retain" = retain,
        "banned" = banned,

        "mycache.maxpar" = mycache.maxpar,
        "max.par" = max.par,
        "dag.maxpar" = dag.maxpar,
        "fabn.maxpar" = fabn.maxpar
      )
    )
  }



#' Run MCMC for ABN
#'
#' Requires output from \code{bnaiaR::abnwithmaxparents()}.
#'
#' @param mycache.maxpar output from \code{bnaiaR::abnwithmaxparents()$mycache.maxpar}.
#' @param max.par output from \code{bnaiaR::abnwithmaxparents()$maxpar}.
#' @param dag.maxpar output from \code{bnaiaR::abnwithmaxparents()$dag.maxpar}.
#' @param fabn.maxpar output from \code{bnaiaR::abnwithmaxparents()$fabn.maxpar}. Only used to collect in the final results.
#' @param df data.frame with training data
#' @param dist list of variable = distribution type. Details in \code{?abn::buildScoreCache()}.
#' @param banned matrix with column variable as parent of row-variable, that defines which arcs are not permitted. Details in \code{?abn::buildScoreCache()}.
#' @param retain matrix with column variable as parent of row-variable, that defines which arcs are enforced. Details in \code{?abn::buildScoreCache()}.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param method either "Bayes" or "mle" approach. Details in \code{?abn::buildScoreCache()}.
#' @param no.cores integer of number of cores to parallelise.
#' @param score character of score that is used to score the network. Details in \code{?abn::mostprobable()}.
#' @param mcmcseeds vector of integers who set the seeds in each parallel run. Details in \code{?mcmcabn::mcmcabn()}.
#' @param mcmcscheme vector of integers with the number of returned DAGs, the number of thinned steps and length of the burn-in phase. Details in \code{?mcmcabn::mcmcabn()}.
#' @param mcmcprior integer of 1 for uniform structural prior and 2 for weighted prior. Details in \code{?mcmcabn::mcmcabn()}.
#' @param prob.rev probability of selecting a new edge reversal. Details in \code{?mcmcabn::mcmcabn()}.
#' @param prob.mbr probability of selecting a Markov blanket resampling scheme. Details in \code{?mcmcabn::mcmcabn()}.
#'
#' @return list of df, dist, retain, banned, score, method, mycache.maxpar, max.par, dag.maxpar, fabn.maxpar, mcmc.out.list, mcmc.scheme, prob.rev, prob.mbr, prior.choice.
#' @export
#'
#' @examples \dontrun{
#' mcmcabn42 <-
#' mcmcabn_bnaiar(
#'   mycache.maxpar = abnwithmaxpar42$mycache.maxpar,
#'   max.par = abnwithmaxpar42$max.par,
#'   dag.maxpar = abnwithmaxpar42$dag.maxpar,
#'   fabn.maxpar = abnwithmaxpar42$fabn.maxpar,
#'   df = df42,
#'   dist = dist42,
#'   banned = banned42,
#'   retain = retain42,
#'   filenamesuffix = "4.2"
#' )
#' }
mcmcabn_bnaiar <-
  function(mycache.maxpar,
           max.par,
           dag.maxpar,
           fabn.maxpar,
           df,
           dist,
           banned,
           retain,
           filenamesuffix,
           filenamebase = FILENAMEbase,
           filename = FILENAME,
           method = METHOD,
           no.cores = n.cores,
           score = SCORE,
           mcmcseeds = MCMC.SEEDS,
           mcmcscheme = MCMC.SCHEME,
           mcmcprior = MCMC.PRIOR,
           prob.rev = PROB.REV,
           prob.mbr = PROB.MBR) {
    cat("\nStart MCMCabn...")
    starttime <- Sys.time()

    clust <-
      parallel::makeCluster(
        length(mcmcseeds),
        outfile = paste0(
          filenamebase,
          filename,
          filenamesuffix,
          "_multicoreMCMCABN.log"
        )
      )
    doParallel::registerDoParallel(cl = clust)
    mcmc.out.list <- foreach(
      SEED = mcmcseeds,
      .packages = c("abn", "mcmcabn"),
      .inorder = TRUE
    ) %dopar% {
      mcmcabn(
        score.cache = mycache.maxpar,
        score = score,
        data.dists = dist,
        max.parents = max.par,
        mcmc.scheme = mcmcscheme,
        seed = SEED,
        verbose = FALSE,
        start.dag = dag.maxpar$dag,
        prob.rev = prob.rev,
        # REV and MBR are efficient in producing high scoring structure but computationally costly compared to classical MCMC jumps.
        prob.mbr = prob.mbr,
        prior.choice = mcmcprior
      ) # Koivisto prior
    }
    stopCluster(clust)

    endtime <- Sys.time()
    cat(paste("\nEnd MCMC ABN. Time used [h]:", round(
      difftime(endtime, starttime, units = "hours"), 2
    )))

    finalresults <- list(
      "df" = df,
      "dist" = dist,
      "retain" = retain,
      "banned" = banned,
      "score" = score,
      "method" = method,

      "mycache.maxpar" = mycache.maxpar,
      "max.par" = max.par,
      "dag.maxpar" = dag.maxpar,
      "fabn.maxpar" = fabn.maxpar,

      "mcmc.out.list" = mcmc.out.list,
      "mcmc.scheme" = mcmcscheme,
      "prob.rev" = prob.rev,
      "prob.mbr" = prob.mbr,
      "prior.choice" = mcmcprior
    )

    ### Save raw data
    save(
      list = "finalresults",
      file = paste0(filenamebase, filename, filenamesuffix, "_final.RData")
    )
    cat(
      "Final data written to:",
      paste0(filenamebase, filename, filenamesuffix, "_final.RData")
    )
    return(finalresults)
  }
