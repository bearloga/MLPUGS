#' @title Classify new samples using an Ensemble of Classifier Chains
#' @description Uses a trained ECC and Gibbs sampling to predict labels for new
#'   samples. \code{.f} must return a matrix of probabilities, one row for each
#'   observation in \code{newdata}.
#' @param object An object of type \code{ECC} returned by \code{\link{ecc}()}.
#' @param newdata A data frame or matrix of features. Must be the same form as
#' the one used with \code{\link{ecc}()}.
#' @param n.iters Number of iterations of the Gibbs sampler.
#' @param burn.in Number of iterations for adaptation (burn-in).
#' @param thin Thinning interval.
#' @param parallel Logical flag for utilizing multicore capabilities of the
#'   system.
#' @param silent Logical flag indicating whether to have a progress bar (if
#'   the 'progress' package is installed) or print progress messages to console.
#' @param .f User-supplied prediction function that corresponds to the type of
#' classifier that was trained in the \code{\link{ecc}()} step. See Details.
#' @param ... additional arguments to pass to \code{.f}.
#' @return An object of class \code{PUGS} containing: \itemize{
#'  \item \code{y_labels} : inherited from \code{object}
#'  \item \code{preds} : A burnt-in, thinned multi-dimensional array of predictions.
#' }
#' @examples \dontrun{
#' x <- movies_train[, -(1:3)]
#' y <- movies_train[, 1:3]
#'
#' model_c50 <- ecc(x, y, .f = C50::C5.0)
#' predictions_c50 <- predict(model_c50, movies_test[, -(1:3)],
#'                  .f = C50::predict.C5.0, type = "prob")
#'   
#' model_rf <- ecc(x, y, .f = randomForest::randomForest)
#' predictions_rf <- predict(model_rf, movies_test[, -(1:3)],
#'                  n.iters = 1000, burn.in = 100, thin = 10,
#'                  .f = function(rF, newdata){
#'                         randomForest:::predict.randomForest(rF, newdata, type = "prob")
#'                  })
#' }
#' @export
predict.ECC <- function(object, newdata,
                        n.iters = 300, burn.in = 100, thin = 2,
                        parallel = FALSE, silent = TRUE,
                        .f = NULL, ...)
{
  m <- length(object$fits)
  L <- length(object$y_labels)
  n <- nrow(newdata)
  ecc_preds <- unlist(parallel::mclapply(1:m, function(k) {
    # Initialize
    cc_preds <- array(0, c(n, L, burn.in + n.iters))
    cc_preds[,,1] <- matrix(rbinom(n * L, 1, prob = 0.5), nrow = n)
    if (!silent) {
      if (requireNamespace("progress", quietly = TRUE)) {
        pb <- progress::progress_bar$new(total = burn.in + n.iters)
      } else {
        message("'progress' package not installed; using simple updates:")
        cat(sprintf("Model %.0f finished Iteration 1\n", k))
      }
    }
    # Iterate
    for ( i in 2:(burn.in + n.iters) ) {
      elapsed <- system.time({
        for ( l in 1:L ) {
          # Assemble a features matrix by augmenting supplied features
          # with predicted labels using the most recent predictions.
          iter_idx <- i - ((1:L) >= l)
          temp <- matrix(0, nrow = n, ncol = L)
          for ( j in 1:length(iter_idx) ) {
            temp[, j] <- cc_preds[, j, iter_idx[j]]
          }
          colnames(temp) <- paste0('label_', 1:L)
          augmented_newdata <- cbind(newdata, temp[, -l])
          
          # Draw samples of predictions
          predictions <- .f(object$fits[[k]][[l]], augmented_newdata, ...)[, "1"]
          cc_preds[, l, i] <- vapply(predictions, function(p) rbinom(n = 1, size = 1, prob = p),
                                     FUN.VALUE = 1, USE.NAMES = FALSE)
        }
      })['elapsed']
      if (!silent) {
        if (requireNamespace("progress", quietly = TRUE)) {
          pb$tick()
        } else {
          cat(sprintf("Model %.0f finished Iteration %.0f (took %.3f seconds) : %.2f%% done\n",
                      k, i, elapsed, 100 * i / (burn.in + n.iters)))
        }
      }
    }
    return(cc_preds)
  }, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows", 1, parallel::detectCores())))
  ecc_preds <- array(ecc_preds, dim = c(n, L, n.iters, m),
                     dimnames = list("Instances" = NULL,
                                     "Labels" = NULL,
                                     "Iterations" = NULL,
                                     "Models" = NULL))
  return(structure(list(y_labels = object$y_labels, preds = ecc_preds), class = "PUGS"))
}
