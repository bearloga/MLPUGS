#' Classify new samples using an Ensemble of Classifier Chains
#' 
#' Uses a trained ECC and Gibbs sampling to predict labels for new samples.
#' 
#' \code{FUN} must return a matrix of probabilities, one row for each observation in \code{newdata}.
#' 
#' @param object An object of type \code{ECC} returned by \code{\link{train_ecc}}.
#' @param newdata A data frame or matrix of features. Must be the same form as
#' the one used with \code{train_ecc}.
#' @param n.iters Number of iterations of the Gibbs sampler.
#' @param burn.in Number of iterations for adaptation (burn-in).
#' @param thin Thinning interval.
#' @param parallel Whether to utilize multicore capabilities of the system.
#' @param silent Whether to print progress messages to console. Recommended.
#' @param FUN User-supplied prediction function that corresponds to the type of
#' classifier that was trained in the \code{train_ecc} step. See Details below.
#' @param ... additional arguments to pass to \code{FUN}.
#' @return An object of class \code{PUGS} containing: \itemize{
#'  \item \code{y_labels} : inherited from \code{object}
#'  \item \code{preds} : A burnt-in, thinned multi-dimensional array of predictions.
#' }
#' @examples \dontrun{
#' 
#'   y.pred <- train_ecc(x, y) %>% predict(x.new)
#'   
#'   y.pred <- train_ecc(x, y, FUN = foo_train) %>%
#'     predict(x.new, FUN = foo_predict)
#' 
#'   y.pred <- train_ecc(x, y, FUN = C50::C5.0) %>%
#'     predict(x.new,C50::predict.C5.0,type = "prob")
#'             
#'   y.pred <- train_ecc(x, y, FUN = randomForest::randomForest) %>%
#'     predict(x.new, n.iters = 1000, burn.in = 100, thin = 10,
#'             FUN = function(rF,newdata){
#'               randomForest:::predict.randomForest(rF,newdata,type="prob")
#'              })
#' }
#' @export

predict.ECC <- function(object, newdata,
                        n.iters = 300, burn.in = 100, thin = 2,
                        parallel = TRUE, silent = FALSE,
                        FUN = NULL, ...)
{
  if (is.null(FUN)) {
    if (requireNamespace("greenr", quietly = TRUE)) {
      FUN <- function(object, newdata, ...) {
        greenr::forest(object, newdata, type = "prob", ...)
      }
    } else {
      stop("Development version of greenr not installed, please supply your own FUN")
    }
  }
  m <- length(object$fits)
  L <- length(object$y_labels)
  n <- nrow(newdata)
  ecc_preds <- parallel::mclapply(1:m,function(k) {
    # Initialize
    cc_preds <- array(0,c(n,L,burn.in+n.iters))
    cc_preds[,,1] <- matrix(rbinom(n*L,1,prob=0.5),nrow=n)
    if (!silent) cat(sprintf("Model %.0f finished Iteration 1\n",k))
    # Iterate
    for ( i in 2:(burn.in+n.iters) ) {
      elapsed <- system.time({
        for ( l in 1:L ) {
          # Assemble a features matrix by augmenting supplied features
          # with predicted labels using the most recent predictions.
          iter.idx <- (i-1*((1:L)>=l))
          temp <- matrix(0,nrow=n,ncol=L)
          for ( j in 1:length(iter.idx) ) {
            temp[,j] <- cc_preds[,j,iter.idx[j]]
          }
          colnames(temp) <- paste0('label_',1:L)
          augmented_newdata <- cbind(newdata,temp[,-l])
          
          # Draw samples of predictions
          cc_preds[,l,i] <- FUN(object$fits[[k]][[l]],augmented_newdata,...)[,"1"] %>%
            { sapply(.,function(p)rbinom(n=1,size=1,prob=p)) } %>% unname
        }
      })['elapsed']
      if (!silent) cat(sprintf("Model %.0f finished Iteration %.0f (took %.3f seconds) : %.2f%% done\n",k,i,elapsed,100*i/(burn.in+n.iters)))
    }
    return(cc_preds)
  },mc.cores=ifelse(Sys.info()[['sysname']]=="Windows",1,parallel::detectCores())) %>%
  unlist %>% array(dim=c(n,L,n.iters,m),
                   dimnames=c("Instances","Labels","Iterations","Models"))
returnable <- list(y_labels=object$y_labels,preds=ecc_preds)
class(returnable) <- "PUGS"
return(returnable)
}
