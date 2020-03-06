# MLPUGS
Multi-Label Prediction Using Gibbs Sampling (and Classifier Chains)

An implementation of classifier chains (CC's) for multi-label prediction. Users can employ an external package (e.g. 'randomForest', 'C50'), or supply their own. The package can train a single set of CC's or train an ensemble of CC's -- in parallel if running in a multi-core environment. New observations are classified using a Gibbs sampler since each unobserved label is conditioned on the others. The package includes methods for evaluating the predictions for accuracy and aggregating across iterations and models to produce binary or probabilistic classifications.

## Installation

```R
# install.packages("remotes")
remotes::install_github("bearloga/MLPUGS")
```

## Basic Usage

```R
fit <- ecc(x, y)
preds <- predict(fit, x_new)
y_pred <- summary(preds)
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

fit <- ecc(x, y, .f = foo_train)
pugs <- predict(fit, x_new, .f = foo_predict)
y_pred <- summary(pugs, type = "prob")

y_pred <- ecc(x, y, .f = foo_train) %>%
          predict(x_new, .f = foo_predict) %>%
          summary(type = "prob")
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
