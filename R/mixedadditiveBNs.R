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
#' @param CLTYPE "FORK" (default) or "PSOCK" if on windows. The later can cause memory issues.
#' @param randomeffect name of grouping variable as character string.
#' @param catcov.restriction Passed on to \code{mclogit::mblogit} \code{CatCov} argument.
#' @param score character of score that is used to score the network. Details in \code{?abn::mostprobable()}.
#' @param rngSEED integer to use as seed.
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
findOptNoParentNodes <- function(df,
                                 dist,
                                 banned,
                                 retain,
                                 filenamesuffix,
                                 filenamebase = FILENAMEbase,
                                 filename = FILENAME,
                                 method = METHOD,
                                 no.cores = n.cores,
                                 CLTYPE = "FORK",
                                 randomeffect = NULL,
                                 catcov.restriction = "diagonal",
                                 score = SCORE,
                                 rngSEED = SEED){
  message(paste("\nRun the exact search across incremental parent limits with method:", METHOD))

  if(!is.null(randomeffect)){
    novars <- ncol(df[,-which(colnames(df)==randomeffect)])
  } else if (is.null(randomeffect)){
    novars <- ncol(df)
  } else {
    stop("randomeffect must be NULL or corresponding to one of the column names of df.")
  }

  tmpscores <- vector(length = novars)
  net.scores <-
    data.frame(npar = NULL,
               scoretype = NULL,
               scorevalue = NULL)

  clust <-
    parallel::makeCluster(no.cores,
                          outfile = paste0(filenamebase, filename, filenamesuffix, "_multicoreABNmaxpar.log"),
                          type = CLTYPE)
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
      method = method,
      group.var = randomeffect,
      verbose = TRUE,
      control = build.control(catcov.mblogit = catcov.restriction,
                              seed = rngSEED)
    )

    dag.mP <- mostProbable(score.cache = mycache,
                           score = score,
                           verbose = TRUE)

    tryCatch({fabn.mP <- fitAbn(object = dag.mP,
                                method = method,
                                group.var = randomeffect,
                                verbose = TRUE,
                                control = fit.control(catcov.mblogit = catcov.restriction,
                                                      seed = rngSEED))},
             error = function(e){NULL})

    if(!exists("fabn.mP")){
      warning(paste("Could not fit abn with maxpar: ", max.par, ". Returning score with value NA."))
      fabn.mP <- list("bic"=NA)
    }


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
#' @param randomeffect name of grouping variable as character string.
#' @param catcov.restriction Passed on to \code{mclogit::mblogit} \code{CatCov} argument.
#' @param rngSEED integer to use as seed.
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
           score = SCORE,
           randomeffect = NULL,
           catcov.restriction = "diagonal",
           rngSEED = SEED) {
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
      method = method,
      group.var = randomeffect,
      verbose = TRUE,
      control = build.control(catcov.mblogit = catcov.restriction,
                              seed = rngSEED)
    )

    dag.maxpar <- mostProbable(score.cache = mycache.maxpar,
                               score = score)
    fabn.maxpar <- fitAbn(object = dag.maxpar,
                          method = method,
                          group.var = randomeffect,
                          verbose = TRUE,
                          control = fit.control(catcov.mblogit = catcov.restriction,
                                                  seed = rngSEED))

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
    abn2bnlearn.plot(data = df[,which(colnames(df) %in% colnames(dag.maxpar$dag))],
                     abndag = dag.maxpar,
                     title = "prelim. ABN DAG with max. parents.")
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
#' @param CLTYPE "FORK" (default) or "PSOCK" if on windows. The later can cause memory issues.
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
           CLTYPE = "FORK",
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
        ),
        type = CLTYPE
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

#' Maximum Number of Parents for ABN
#'
#' Evaluate the number of parent nodes that does not increase the score anymore.
#'
#' @param net.scores output from \code{bnaiaR::findOptNoParentNodes()$net.scores}
#' @param SAVEPLOTS If TRUE the plot will be saved.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param filetype "pdf" or "png" or any that is supported by \code{?ggplot2::ggsave()}.
#' @param plotwidth integer specifying plot width.
#' @param plotheight integer specifying plot height.
#'
#' @return ggplot2 and data.frame.
#' @export
maxparentsplot <- function(net.scores,
                           SAVEPLOTS,
                           filenamesuffix,
                           filenamebase = FILENAMEbase,
                           filename = FILENAME,
                           filetype = "pdf",
                           plotwidth = PLOTWIDTH,
                           plotheight = PLOTHEIGHT) {
  df <- net.scores %>%
    mutate(
      scoretype = as.factor(scoretype),
      npar = as.factor(npar),
      scorevalue = as.numeric(scorevalue)
    ) %>%

    reframe(
      scorevalue.norm = (1 - abs(1 - scorevalue / max(scorevalue))) * 100,
      npar = npar,
      scorevalue = scorevalue,
      .by = scoretype)

  # Plot Network score by increasing max parents
  plt.rel <-
    ggplot(df, aes(x = npar, color = scoretype, y = scorevalue.norm)) +
    geom_point(aes(
      shape = scoretype,
      alpha = 0.5,
      size = 1
    )) +
    # facet_wrap(.~dist_type)+
    labs(title = "Relative network score per no. parent nodes",
         x = "number of parent nodes",
         y = "network score [%]") +
    scale_x_discrete(limits = unique(df$npar)) +
    theme_bw()

  plt.abs <-
    ggplot(df, aes(color = scoretype, y = scorevalue, group = scoretype)) +
    geom_point(aes(
      x = npar,
      shape = scoretype,
      alpha = 0.5,
      size = 1
    )) +
    # facet_wrap(.~dist_type)+

    labs(title = "Absolute network score per no. parent nodes",
         x = "number of parent nodes",
         y = "network score") +
    scale_x_discrete(limits = unique(df$npar)) +
    theme_bw()

  plt.comb <-
    cowplot::plot_grid(plt.rel, plt.abs, labels = "AUTO", ncol = 1)

  if (SAVEPLOTS) {
    plotname <-
      paste0(filenamebase, filename, filenamesuffix,
             "_netscore_per_no.parent_nodes")

    ggsave(
      paste0(plotname, ".", filetype),
      plot = plt.comb,
      width = plotwidth,
      height = plotheight
    )
  } else {
    return(list(df, plt.comb))
  }
}

#' Draw graphs for different number of max parents.
#'
#'
#' @param net.scores.dags output from \code{bnaiaR::findOptNoParentNodes()$net.scores.dags}
#' @param df data.frame with training data
#' @param SAVEPLOTS If TRUE the plot will be saved.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param filetype "pdf" or "png" or any that is supported by \code{?ggplot2::ggsave()}.
#' @param plotwidth integer specifying plot width.
#' @param plotheight integer specifying plot height.
#'
#' @return plot
#' @export
dagswithdiffparentsplt <- function(net.scores.dags,
                                   df,
                                   SAVEPLOTS,
                                   filenamesuffix,
                                   filenamebase = FILENAMEbase,
                                   filename = FILENAME,
                                   filetype = "pdf",
                                   plotwidth = PLOTWIDTH,
                                   plotheight = PLOTHEIGHT) {
  parents <- 1
  plt <- list()
  for (i in net.scores.dags) {
    # plotAbn(i, dist, edge.direction = "undirected")

    dag = bnlearn::empty.graph(names(df))
    bnlearn::amat(dag) = t(i$dag)

    temp.plt <- bnlearn::graphviz.plot(
      dag,
      shape = "rectangle",
      main = paste0("DAG with max.par=", parents),
      render = F
    )
    plt[[parents]] <- temp.plt
    parents <- parents + 1
  }

  if (SAVEPLOTS) {
    plotname <-
      paste0(filenamebase,
             filename,
             filenamesuffix,
             "_netscore_per_no.parent_nodes")
    if (filetype == "png") {
      png(
        filename = paste0(plotname, ".", filetype),
        width = plotwidth,
        height = plotheight
      )
    } else if (filetype == "pdf") {
      pdf(
        file = paste0(plotname, ".", filetype),
        width = plotwidth,
        height = plotheight
      )
    }

    par(mfrow = c(3, 3))
    parents <- 1
    for (i in plt) {
      attrs <- list(node=list(shape="rectangle", fixedsize=FALSE))
      Rgraphviz::plot(i, attrs=attrs)
      title(paste0("DAG with max.par=", parents))
      parents <- parents + 1
    }
    dev.off()
  } else {
    par(mfrow = c(3, 3))
    parents <- 1
    for (i in plt) {
      attrs <- list(node=list(shape="rectangle", fixedsize=FALSE))
      Rgraphviz::plot(i, attrs=attrs)
      title(paste0("DAG with max.par=", parents))
      parents <- parents + 1
    }
  }
}

#' Parameteric Bootstrapping
#'
#' Supply as \code{object} an estimate of an initial "best" Graph $\eqn{G_0}$ from the original, real-world data $\eqn{D_0}$.
#' Let $N$ the number of bootstrap samples that are provided as \eqn{n.sim}.
#' This function then simulates N data set $\eqn{D_1}, \dots, \eqn{D_n}$ from model $\eqn{G_0}$
#' and estimates N new models $\eqn{G_1}, \dots, \eqn{G_n}$ from the respective simulated data.
#'
#' The output can be fed in \code{consensusDAG} which will estimate the final model $\eqn{G_{sum}}$, by summarising over all $\eqn{G_1}, \dots, \eqn{G_n}$.
#' The consensus model $\eqn{G_{consensus}}$ is the trimmed $\eqn{G_{sum}}$ based on accepting only significant edges.
#'
#' @param object of class abnFit
#' @param n.sim number of bootstrap iterations.
#' @param dag.banned a matrix or a formula statement defining which arcs are not permitted - banned. Note that colnames and rownames must be set, otherwise same row/column names as data.df will be assumed. If set as NULL an empty matrix is assumed.
#' @param dag.retained a matrix or a formula statement defining which arcs are must be retained in any model search. Note that colnames and rownames must be set, otherwise same row/column names as data.df will be assumed. If set as NULL an empty matrix is assumed.
#' @param max.parents a constant or named list giving the maximum number of parents allowed.
#' @param catcov.restriction Passed on to \code{mclogit::mblogit} \code{CatCov} argument.
#' @param btseeds integer vector with individual seeds for each simulation. Requires one, unique seed for each iteration.
#' @param filenamesuffix character specifying the current run (e.g. experiment number).
#' @param filenamebase character of path to location where results should be stored.
#' @param filename character specifying the current set of runs.
#' @param no.cores integer of number of cores to parallelise.
#' @param CLTYPE "FORK" (default) or "PSOCK" if on windows. The later can cause memory issues.
#' @param verbose print more output.
#'
#' @examples
#' dags <- paramBootAbn(object = myres,
#'                      n.sim = 100,
#'                      dag.banned = banned51[-10, -10], # all except study_source
#'                      dag.retained = retain51[-10, -10], # all except study_source
#'                      max.parents = 4,
#'                      catcov.restriction = "diagonal",
#'                      btseeds = c(1:100)*SEED,
#'                      filenamesuffix = "4.5")
#'
#' @export
#'
#' @return list of abn fit objects
paramBootAbn <- function(object,
                         n.sim,
                         dag.banned,
                         dag.retained,
                         max.parents,
                         catcov.restriction = "diagonal",
                         btseeds,
                         filenamesuffix,
                         filenamebase = FILENAMEbase,
                         filename = FILENAME,
                         no.cores = n.cores,
                         CLTYPE = "FORK",
                         verbose = FALSE){
  ## Prepare inputs
  # check if n.sim and btseeds are valid
  if(!(is.null(n.sim)) & is.integer(n.sim)){
    if (!(is.null(btseeds)) & all(is.integer(btseeds)) & (length(btseeds) == n.sim)){
      if (verbose) message(paste("btseeds (", btseeds, ") and n.sim (", n.sim, ") are ok."))
    } else {
      stop("btseeds must be a vector of integers with length equal to the value of n.sim.")
    }
  } else {
    stop("n.sim must be an integer >= 1.")
  }
  if(!inherits(object, "abnFit")){
    stop("'object' must be of class 'abnFit'.")
  } else {
    dag <- object$abnDag$dag
    data.df <- object$abnDag$data.df
    data.dists <- object$abnDag$data.dists
    group.var <- object$abnDag$group.var
    group.ids <- object$abnDag$group.ids
  }

  ## Actual bootstrapping
  cat("\nStart bootstrapping with abn()...")
  starttime <- Sys.time()
  if (no.cores >1){
    clust <-parallel::makeCluster(no.cores,
                                  outfile = paste0(
                                    filenamebase,
                                    filename,
                                    filenamesuffix,
                                    "_multicoreParamBootABN.log"
                                  ),
                                  type = CLTYPE)
    doParallel::registerDoParallel(cl = clust)

    out <- foreach(i = 1:n.sim,
                   .packages = c("abn", "bnaiaR"),
                   .export = "paramBootAbn_backend",
                   .inorder = TRUE) %dopar% {
      paramBootAbn_backend(i = i,
                           object = object,
                           data.df = data.df,
                           data.dists = data.dists,
                           dag.banned = dag.banned,
                           dag.retained = dag.retained,
                           max.parents = max.parents,
                           catcov.restriction = catcov.restriction,
                           btseeds = btseeds,
                           verbose = verbose)
    }
    stopCluster(clust)
  } else if (no.cores == 1){
    out <- foreach(i = 1:n.sim,
                   .packages = c("abn", "bnaiaR"),
                   .export = "paramBootAbn_backend",
                   .inorder = TRUE) %do% {
      paramBootAbn_backend(i = i,
                           object = object,
                           data.df = data.df,
                           data.dists = data.dists,
                           dag.banned = dag.banned,
                           dag.retained = dag.retained,
                           max.parents = max.parents,
                           catcov.restriction = catcov.restriction,
                           btseeds = btseeds,
                           verbose = verbose)
    }
  }
  endtime <- Sys.time()
  cat(paste("\nEnd Parametric Bootstrapping with ABN. Time used [h]:", round(
    difftime(endtime, starttime, units = "hours"), 2
  )))

  # Save paramBoot output
  save(dags,
    file = paste0(filenamebase, filename, filenamesuffix, "_paramBootAbnOut.RData")
  )
  cat("\nParamBootAbn() output saved.")

  return(out)
}

paramBootAbn_backend <- function(i,
                                 object,
                                 data.df,
                                 data.dists,
                                 dag.banned,
                                 dag.retained,
                                 max.parents,
                                 catcov.restriction,
                                 btseeds,
                                 verbose){
  buildCache_succeed <- FALSE
  while(!buildCache_succeed){
    dfsim <- simulateAbn(object = object,
                         n.iter = as.integer(nrow(data.df)), # Sample set has equal size as original data
                         verbose = verbose,
                         # seed = btseeds[i], # no seed here to have variable data. i.e. When buildScoreChache fails with the sampled data, use different data in the repeat of the simulation.
                         run.simulation = TRUE)

    mycache_sim <- NULL
    tryCatch({
      mycache_sim <- buildScoreCache(method="mle",
                                     data.df=dfsim[, names(data.dists)], # reordered to match data.dists
                                     data.dists=data.dists, # all except study_source
                                     # group.var="study_source", # We don't have grouping from the simulated data set.
                                     dag.banned = dag.banned,
                                     dag.retained = dag.retained,
                                     max.parents=max.parents,
                                     verbose = verbose,
                                     control = build.control(catcov.mblogit = catcov.restriction,
                                                             seed = btseeds[i]))
    }, error = function(e) {buildCache_succeed <- FALSE})

    if (!is.null(mycache_sim)){
      mp.dag_sim <- mostProbable(mycache_sim)
      myres_sim <- fitAbn(method="mle",
                          object = mp.dag_sim,
                          control = fit.control(catcov.mblogit = catcov.restriction,
                                                seed = btseeds[i]))

      # return to outputs
      buildCache_succeed <- TRUE
      gc()
      return(list("dfsim" = dfsim,
                  "cache_sim" = mycache_sim,
                  "mpdag_sim" = mp.dag_sim,
                  "fit_sim" = myres_sim))
    } else {
      message(paste("Simulation no. ", i, "failed and I am repeating it."))
      gc()
    }
  }#EOWHILE
}

#' Consensus Model
#'
#' Supply as \code{object} an estimate of an initial "best" Graph $\eqn{G_0}$ from the original, real-world data $\eqn{D_0}$ to \code{paramBootAbn()}.
#' Let $N$ the number of bootstrap samples that are provided as \code{n.sim}.
#' \code{paramBootAbn()} then simulates N data set $\eqn{D_1}, \dots, \eqn{D_n}$ from model $\eqn{G_0}$
#' and estimates N new models $\eqn{G_1}, \dots, \eqn{G_n}$ from the respective simulated data.
#'
#' The output from \code{paramBootAbn()} can be fed in \code{consensusDAG} which will estimate the final model $\eqn{G_{sum}}$, by summarising over all $\eqn{G_1}, \dots, \eqn{G_n}$.
#' The consensus model $\eqn{G_{consensus}}$ is the trimmed $\eqn{G_{sum}}$ based on accepting only significant edges.
#'
#' @param object list of abnFit objects.
#' @param consensusMethod pruning strategy. "signEdge" calculates the consensus model based on accepting only significant edges.
#'
#' @examples
#' consDAGlist <- consensusDAG(object = dags,
#'                             consensusMethod = "signEdges")
#' plotAbn(consDAGlist[["consDAG"]], data.dists = dist51)
#'
#' @export
#' @return Consensus model as fitAbn object.
consensusDAG <- function(object,
                         consensusMethod = "signEdges"){
  # create list of adjacency matrices (DAGs)
  dags_list <- list()
  for (i in 1:length(object)){
    dags_list[[i]] <- object[[i]]$mpdag_sim$dag
  }

  # adj. matrix with edge frequency
  cumdag <- Reduce("+", dags_list)

  # relative edge frequency aka. arc-strength
  reldag <- cumdag / length(dags_list)

  if (consensusMethod == "signEdges"){
    # calculate arc-strength threshold
    arc.stren.thr <- bnaiaR::arc.stren.threshold(reldag)

    consDAG <- reldag
    consDAG[consDAG<=arc.stren.thr] <- 0
    consDAG[consDAG>arc.stren.thr] <- 1

    return(list("arcStrengthThreshold" = arc.stren.thr,
                "consDAG" = consDAG))
  } else {
    stop(paste("Consensus method", consensusMethod, "is not implemented."))
  }
}
