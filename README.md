# MLPUGS
MultiLabel Prediction Using Gibbs Sampling (& Classifier Chains)

An implementation of classifier chains for multi-label prediction. Users can employ an external package (e.g. C50, randomForest), or supply their own. The package can train a single set of CCs or train an ensemble of CCs -- in parallel if running in a multicore environment. New observations are classified using a Gibbs sampler since each unobserved label is conditioned on the others. The package includes methods for evaluating the predictions for accuracy and aggregating across iterations & models to make final binary or probabilistic classifications

## Installation

```R
if ( !('devtools' %in% installed.packages()) ) install.packages("devtools")

devtools::install_github("bearloga/MLPUGS") # or...
devtools::install_github("bearloga/MLPUGS", build_vignettes = TRUE)
```

## Basic Usage

```R
fit <- ecc(x, y)
preds <- predict(fit, x_new)
y_pred <- summary(preds)

# y_pred <- ecc(x, y) %>% predict(x_new) %>% summary
```

For a detailed tutorial, please see `browseVignettes(package="MLPUGS")`.

### External Classifiers

Currently, there is no built-in classifier in version 0.1.1, but users can supply their own or use an existing package. For example:

```R
# Random Forest:
foo_train <- function(x, y) randomForest::randomForest(x, y)
foo_predict <- function(x, newdata) randomForest:::predict.randomForest(x, newdata, type = "prob")

# C5.0:
foo_train <- function(x, y) C50::C5.0(x, y)
foo_predict <- function(x, newdata) C50::predict.C5.0(x, newdata, type = "prob")

fit <- ecc(x, y, FUN = foo_train)
pugs <- predict(fit, x_new, FUN = foo_predict)
y_pred <- summary(pugs, type = "prob")

y_pred <- ecc(x, y, FUN = foo_train) %>%
          predict(x_new, FUN = foo_predict) %>%
          summary(type = "prob")
```

Future versions will use the [greenr](https://github.com/bearloga/greenr) package by default and will maintain the ability to use other classifiers.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
