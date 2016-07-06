movies <- read.delim("data-raw/fandango538.tsv.gz", stringsAsFactors = FALSE, sep = "\t")[, 1:8]

colnames(movies) <- c("movie", "rotten_tomatoes", "rotten_tomatoes_user",
                      "metacritic", "metacritic_user",
                      "imdb_user", "fandango", "fandango_user")

movies$title <- stringr::str_replace(movies$movie, " \\([0-9]{4}\\)", "")
movies$year <- stringr::str_sub(stringr::str_extract(movies$movie, "\\([0-9]{4}\\)"), 2, -2)

movies$rotten_tomatoes <- as.numeric(movies$rotten_tomatoes >= 80)
movies$metacritic <- as.numeric(movies$metacritic >= 80)
movies$fandango <- as.numeric(movies$fandango >= 4)

set.seed(0)
train_idx <- sample.int(nrow(movies), nrow(movies)*0.6, replace = FALSE)
test_idx <- setdiff(1:nrow(movies), train_idx)

movies_train <- movies[train_idx, c("rotten_tomatoes", "metacritic", "fandango", "rotten_tomatoes_user", "metacritic_user", "fandango_user", "imdb_user")]
rownames(movies_train) <- movies$movie[train_idx]
movies_test <- movies[test_idx, c("rotten_tomatoes", "metacritic", "fandango", "rotten_tomatoes_user", "metacritic_user", "fandango_user", "imdb_user")]
rownames(movies_test) <- movies$movie[test_idx]

movies <- movies[, setdiff(union(c("title", "year", "rotten_tomatoes", "metacritic", "fandango"), names(movies)), "movie")]

devtools::use_data(movies, movies_train, movies_test, internal = FALSE, compress = "gzip", overwrite = TRUE)
