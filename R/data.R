#' @title FiveThirtyEight's Movie Scores
#' @description This dataset contains every film that has a Rotten Tomatoes
#'   rating, a RT User rating, a Metacritic score, a Metacritic User score,
#'   and IMDb score, and at least 30 fan reviews on Fandango. The data from
#'   Fandango was pulled on Aug. 24, 2015. It is licensed under CC BY 4.0
#' @source \url{https://github.com/fivethirtyeight/data/tree/master/fandango}
#' @rdname movies
#' @format A \code{data.frame} with 146 rows and 9 columns. The training data
#'   contains 87 movies, while the test set contains 59 movies. The first three
#'   columns of the training and test sets indicate the multiple labels: 1 if
#'   the movie got a rating equal to or greater than 80\% on Metacritic,
#'   Rotten Tomatoes, and Fandango; and 0 otherwise.
#' @author FiveThirtyEight
"movies"
#' @rdname movies
"movies_train"
#' @rdname movies
"movies_test"
