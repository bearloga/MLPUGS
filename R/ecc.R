#' Fit an Ensemble of Classifier Chains (ECC)
#' 
#' @param x A data frame or matrix of features.
#' @param y A data frame or matrix of labels. Each label must be its own column
#' and each instance (observation) must be a row of 0s and 1s, indicating which
#' labels belong to the instance.
#' @param m Number of classifier chains (models) to train. Recommended:
#' \code{m = 3} and \code{m = 7} for 4-core and 8-core systems, respectively.
#' @param parallel Whether to utilize multicore capabilities of the system.
#' @param silent Whether to print progress messages to console. Recommended.
#' @param .f User-supplied classifier training function. If not supplied, the
#' trainer will use the built-in classifier. See Details for more information.
#' @param ... additional arguments to pass to \code{.f}.
#' @return A object of class \code{ecc}.
#' @examples \dontrun{
#' x <- movies_train[, -(1:3)]
#' y <- movies_train[, 1:3]
#'
#' fit <- ecc(x, y, .f = randomForest::randomForest)
#'
#' fit <- ecc(x, y, m = 7, .f = C50::C5.0, trials = 10)
#'
#' fit <- ecc(x, y, m = 5, .f = function(x, y, ...) {
#'   xgboost::xgboost(data = as.matrix(x), label = as.numeric(y)-1, ...)
#' }, params = list(objective = "binary:logistic"), nrounds = 100)
#' }
#' @export

ecc <- function(x, y, m = 5,
                parallel = TRUE, silent = FALSE,
                .f = NULL, ...)
{
  n <- nrow(x)
  if ( n != nrow(y) ) stop("x and y must have the same number of rows")
  L <- ncol(y)
  if(!(m %in% c(1, 3, 5, 7))) stop("can only train an ensemble of m = 1, 3, 5, or 7")
  returnable <- list()
  returnable$y_labels <- colnames(y)
  colnames(y) <- paste0('label_', 1:L)
  returnable$fits <- parallel::mclapply(1:m, function(k) {
    idx <- sample(1:n, floor((1-0.025*(m-1))*n), replace = FALSE)
    fit <- list()
    for ( l in 1:L ) {
      elapsed <- system.time({
        fit[[l]] <- .f(x = cbind(x, y[, -l]), y = factor(y[, l]), ...)
      })['elapsed']
      if (!silent) cat(sprintf("Trained model %.0f on response %.0f (took %.3f seconds)\n",
                               k, l, elapsed))
    }
    return(fit)
  }, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows", 1, parallel::detectCores()))
  class(returnable) <- "ECC"
  return(returnable)
  
}
