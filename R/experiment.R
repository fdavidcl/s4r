#' @import caret
experiment <- function(dataset, autoencoder_f, method, normalize = TRUE) {
  set.seed(4242)

  ## Prepare dataset
  k <- 5
  train_idx <- createFolds(dataset$y, k = k)

  results <- list()

  for (i in 1:k) {
    train_x <- dataset$x[-train_idx[[i]],]
    train_y <- dataset$y[-train_idx[[i]]]
    test_x <- dataset$x[train_idx[[i]],]
    test_y <- dataset$y[train_idx[[i]]]

    if (dataset$normalize) {
      mx <- apply(train_x, 2, max)
      mn <- apply(train_x, 2, min)
      # Avoid division by zero
      range_n <- max(mx - mn, keras::k_epsilon())

      train_x <- t(apply(train_x, 1, function(x) (x - mn) / range_n))
      test_x <- t(apply(test_x, 1, function(x) (x - mn) / range_n))
    }

    model <- train_model(autoencoder_f, method, train_x, train_y, normalized = TRUE)
    outs <- test_model(model, test_x)
    results[[i]] <- c(evaluate_features(outs$features, test_y), evaluate_model(test_y, outs$predictions))
  }

  results
}

#' @import purrr
#' @export
run_experiment <- function(datasets = dataset_list()) {
  options(keras.fit_verbose = 0)

  results <- map(datasets, function(dataset) {
    keras::backend()$clear_session()
    evaluate_features(dataset$x, dataset$y)
    # resultsnoae <- dataset %>% experiment(FALSE, "kknn")
    resultsaered <- dataset %>% experiment(supercore, "kknn")
    resultspca <- dataset %>% experiment("pca", "kknn")
    resultsae <- dataset %>% experiment(ruta::autoencoder, "kknn")

    list(
      pca = resultspca,
      basic = resultsae,
      reductive = resultsaered
    )
  })

  saveRDS(results, file = "results.rds")
  results
}

# results <- run_experiment(list(read_data("data/wdbc.data", row.names = 1) %>% class_first("M")))
# results[[1]] %>% map(function(x) x %>% map("fisher") %>% as.numeric %>% summary)
