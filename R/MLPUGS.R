#' MLPUGS: Multi-Label Prediction Using Gibbs Sampling (and Classifier Chains)
#'
#' An implementation of classifier chains for binary and probabilistic
#' multi-label prediction.
#' 
#' The classification pipeline consists of:
#'
#' \enumerate{
#' \item Training an ensemble of classifier chains. Each chain is a binary
#' classifier (built-in, supplied from an external package or user-coded).
#' \item Making predictions using a Gibbs sampler since each unobserved
#' label is conditioned on the others.
#' \item (Optional) Evaluating the ECC.
#' \item Gathering predictions (aggregating across iterations & models).
#' }
#'
#' To learn more about MLPUGS, start with the vignettes:
#' \code{browseVignettes(package = "MLPUGS")}
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @aliases MLPUGS
#' @docType package
#' @name MLPUGS-package

NULL
