#' @import caret
experiment <- function(dataset, autoencoder_f, method, normalize = TRUE) {
  set.seed(4242)

  ## Prepare dataset
  k <- 5
  train_idx <- createFolds(dataset$y, k = k)
  message(str(train_idx))

  results <- list()

  for (i in 1:k) {
    train_x <- dataset$x[-train_idx[[i]],]
    train_y <- dataset$y[-train_idx[[i]]]
    test_x <- dataset$x[train_idx[[i]],]
    test_y <- dataset$y[train_idx[[i]]]

    if (normalize) {
      mx <- apply(train_x, 2, max)
      mn <- apply(train_x, 2, min)
      range <- mx - mn
      train_x <- t(apply(train_x, 1, function(x) (x - mn) / range))
      test_x <- t(apply(test_x, 1, function(x) (x - mn) / range))
    }

    model <- train_model(autoencoder_f, method, train_x, train_y, normalized = normalize)
    predictions <- test_model(model, test_x)
    results[[i]] <- evaluate_model(test_y, predictions)
  }

  results
}

#' @import purrr
#' @export
run_experiment <- function() {
  keras::backend()$clear_session()

  datasets <- dataset_list()

  results <- map(datasets, function(dataset) {
    # resultsnoae <- dataset %>% experiment(FALSE, "kknn")
    resultsaered <- dataset %>% experiment(supercore, "kknn", normalize = TRUE)
    resultspca <- dataset %>% experiment("pca", "kknn")
    resultsae <- dataset %>% experiment(ruta::autoencoder, "kknn", normalize = TRUE)

    list(
      pca = resultspca,
      basic = resultsae,
      reductive = resultsaered
    )
  })

  saveRDS(results, file = "results.rds")
  results
}
