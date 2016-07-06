#' @title Fit an Ensemble of Classifier Chains (ECC)
#' @description Constructs an ensemble of classifier chains, each chain using a
#'   user-supplied base classifier.
#' @param x A data frame or matrix of features.
#' @param y A data frame or matrix of labels. Each label must be its own column
#' and each instance (observation) must be a row of 0s and 1s, indicating which
#' labels belong to the instance.
#' @param m Number of classifier chains (models) to train. Recommended:
#' \code{m = 3} and \code{m = 7} for 4-core and 8-core systems, respectively.
#' @param prop_subset The proportion of the training data to utilize when
#'   \code{m} is greater than 1. Each set of classifier chains in the ensemble
#'   will use a random subset (95\% by default) of the supplied training data.
#' @param run_parallel Whether to utilize multicore capabilities of the system.
#' @param silent Whether to print progress messages to console. Recommended.
#' @param .f User-supplied classifier training function. If not supplied, the
#' trainer will use the built-in classifier. See Details for more information.
#' @param ... additional arguments to pass to \code{.f}.
#' @return An object of class \code{ECC} containing: \itemize{
#'  \item \code{y_labels} : names of the columns of \code{y}
#'  \item \code{fits} : An list of length \code{m}, each element being a set of
#'    classifier chains - a list of length \code{L = ncol(y)} where each element
#'    is a fitted model object trained to predict each of the \code{L} labels.
#' }
#' @examples
#' x <- movies_train[, -(1:3)]
#' y <- movies_train[, 1:3]
#' 
#' fit <- ecc(x, y, m = 1, .f = glm.fit, family = binomial(link = "logit"))
#' 
#' \dontrun{
#' 
#' fit <- ecc(x, y, .f = randomForest::randomForest)
#'
#' fit <- ecc(x, y, m = 7, .f = C50::C5.0, trials = 10)
#' }
#' @export

ecc <- function(x, y, m = 5, prop_subset = 0.95,
                run_parallel = FALSE, silent = TRUE,
                .f = NULL, ...)
{
  n <- nrow(x)
  if ( n != nrow(y) ) {
    stop("x and y must have the same number of rows")
  }
  L <- ncol(y)
  if ( !(m %in% c(1, 3, 5, 7)) ) {
    stop("can only train an ensemble of m = 1, 3, 5, or 7")
  }
  original_labels <- colnames(y)
  colnames(y) <- paste0('label_', 1:L)
  fitted_models <- parallel::mclapply(1:m, function(k) {
    idx <- sample(1:n, floor(prop_subset * n), replace = FALSE)
    fit <- as.list(1:L)
    for ( l in 1:L ) {
      elapsed <- system.time({
        fit[[l]] <- .f(x = cbind(x, y[, -l, drop = FALSE]), y = factor(y[, l]), ...)
      })['elapsed']
      if (!silent) cat(sprintf("Trained model %.0f on response %.0f (took %.3f seconds)\n",
                               k, l, elapsed))
    }
    return(fit)
  }, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows" || !run_parallel, 1, parallel::detectCores()))
  return(structure(list(y_labels = original_labels, fits = fitted_models), class = "ECC"))
}
