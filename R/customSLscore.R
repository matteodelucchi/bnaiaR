#' Additive discrete Bayesian network mixed-effects model
#'
#' Custom score function to regress nodes on their parents as generalized linear mixed model.
#'
#'
#' @param node character string of current node. Corresponds to variable name in data.
#' @param parents character vector of the node's parents.
#' @param data data frame for SL incl. the mixed effect variable.
#' @param args list containing node distribution (dist) and variable name for mixed effect (mixed_effect)
#'
#' @return model's score (BIC) as integer
#'
#' @references Scutari, Marco, Christopher Marquis, and Laura Azzimonti. “Using Mixed-Effects Models to Learn Bayesian Networks from Related Data Sets.” In Proceedings of The 11th International Conference on Probabilistic Graphical Models, 73–84. PMLR, 2022. https://proceedings.mlr.press/v186/scutari22a.html.
#' @references Azzimonti, Laura, Giorgio Corani, and Marco Scutari. “A Bayesian Hierarchical Score for Structure Learning from Related Data Sets.” International Journal of Approximate Reasoning 142 (March 1, 2022): 248–65. https://doi.org/10.1016/j.ijar.2021.11.013.
#'
#' @examples
#' \dontrun{
#' model <- glmm.bic(node = "IAlocation_group",
#'                   parents = c("gender", "hpt_aware", "IAsize_diag_grouped_merged","multipleIAs"),
#'                   data = data.dbn.additive,
#'                   args = list(dist = dist.dbn.additive,
#'                               mixed_effect = "study_source"))
#'
#' # or for bootstrapping
#' bb = boot.strength(data.dbn.additive,
#' algorithm = "tabu",
#' R = 500,
#' cluster = cl,
#' algorithm.args = list(score = "custom",
#'                       fun = glmm.bic,
#'                       blacklist = bl.dbn.additive,
#'                       # whitelist = wl.dbn.allstudy,
#'                       args = list(dist = dist.dbn.additive,
#'                                   mixed_effect = "study_source"),
#'                       tabu = 50))
#' }
glmm.bic = function(node, parents, data, args) {
  dist = args$dist
  mixed_effect = args$mixed_effect

  if (length(parents) == 0){
    # no parent no random effect
    model = as.formula(paste(node, "~ 1 + (1|", mixed_effect, ")"))
  } else {
    model = as.formula(paste(node, "~ (1|", mixed_effect ,")+", paste(parents, collapse = "+")))
  }

  message(paste("\n",deparse1(model), "\n"))

  if (dist[[node]] == "gaussian"){
    mod_glmer <- lme4::glmer(model, data = data, family = "gaussian")

    # if mod_glmer is not null
    if(!is.null(mod_glmer)){
      # check if mod_glmer is singular and remove rand. effects if so
      if(lme4::isSingular(mod_glmer)){
        message(paste("Removing random effects due to singularity."))
        if (length(parents) == 0){
          model_nore = as.formula(paste(node, "~ 1"))
        } else {
          model_nore = as.formula(paste(node, "~", paste(parents, collapse = "+")))
        }
        message(paste("\n",deparse1(model_nore), "\n"))
        tryCatch({
          mod_glm_nore = glm(model_nore, data = data, family = "gaussian")
        }, error=function(e) NULL)

        # if fixed effect model is not null
        if (!is.null(mod_glm_nore)){
          # return BIC of fixed effects model
          return(-BIC(mod_glm_nore)/2)
        } else {
          # return very low score
          return(-Inf)
        }
        # if mod_glmer is not singular and not null, return BIC of mod_glmer
      } else if (!lme4::isSingular(mod_glmer)){
        return(-BIC(mod_glmer)/2)
      } else {
        warning("unknown status of the model.")
        return(-Inf)
      }
    } else if (is.null(mod_glmer)){
      # return very low score
      return(-Inf)
    } else {
      warning("unknown status of the model.")
      return(-Inf)
    }

  } else if (dist[[node]] == "binomial"){
    tryCatch({
      mod_glmer = lme4::glmer(model, data = data, family = "binomial")
    }, error=function(e) NULL)

    # if mod_glmer is not null
    if(!is.null(mod_glmer)){
      # check if mod_glmer is singular and remove rand. effects if so
      if(lme4::isSingular(mod_glmer)){
        message(paste("Removing random effects due to singularity."))
        if (length(parents) == 0){
          model_nore = as.formula(paste(node, "~ 1"))
        } else {
          model_nore = as.formula(paste(node, "~", paste(parents, collapse = "+")))
        }
        message(paste("\n",deparse1(model_nore), "\n"))
        tryCatch({
          mod_glm_nore = glm(model_nore, data = data, family = "binomial")
        }, error=function(e) NULL)

        # if fixed effect model is not null
        if (!is.null(mod_glm_nore)){
          # return BIC of fixed effects model
          return(-BIC(mod_glm_nore)/2)
        } else {
          # return very low score
          return(-Inf)
        }
        # if mod_glmer is not singular and not null, return BIC of mod_glmer
      } else if (!lme4::isSingular(mod_glmer)){
        return(-BIC(mod_glmer)/2)
      } else {
        warning("unknown status of the model.")
        return(-Inf)
      }
    } else if (is.null(mod_glmer)){
      # return very low score
      return(-Inf)
    } else {
      warning("unknown status of the model.")
      return(-Inf)
    }

  } else if (dist[[node]] == "multinomial"){
    if (length(parents) == 0){
      model_basic <- as.formula(paste(node, "~ 1"))
      model_random <- as.formula(paste("~ 1|", mixed_effect, sep = ""))
    } else {
      model_basic <- as.formula(paste(node, "~ ", paste(parents, collapse = "+")))
      model_random <- as.formula(paste("~ 1|", mixed_effect, sep = ""))
    }

    message(paste("using mblogit with fixed term:", deparse1(model_basic), "and random term:", deparse1(model_random)))
    tryCatch({
      mod_mblogit = mclogit::mblogit(formula = model_basic, random = model_random, data = data)
    }, error=function(e) NULL)

    # if mod_mblogit is not null
    if(!is.null(mod_mblogit)){
      # check if mod_mblogit is singular and remove rand. effects if so
      if(mod_mblogit$converged == FALSE){
        message(paste("Removing random effects due to singularity/non-convergence."))
        message(paste("\n",deparse1(model_basic), "\n"))
        tryCatch({
          model_nnet_nore = nnet::multinom(formula = model_basic, data = data)
        }, error=function(e) NULL)

        # if fixed effect model is not null
        if (!is.null(model_nnet_nore)){
          # return BIC of fixed effects model
          return(-BIC(model_nnet_nore)/2)
        } else {
          # return a very low score
          return(-Inf)
        }
      } else if(mod_mblogit$converged == TRUE){
        # if not null and did converged, return BIC of mixed effects model
        return(-BIC(mod_mblogit)/2)
      } else {
        warning("unknown status of the model.")
        return(-Inf)
      }
    } else if(is.null(mod_mblogit)){
      return(-Inf) # return a very low score
    } else {
      warning("unknown status of the model.")
      return(-Inf)
    }
  } else {
    stop("unknown distribution type.")
  }
}#GLMM.BIC
