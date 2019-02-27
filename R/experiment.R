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
run_experiment <- function(datasets = dataset_list(), filename = "results.rds", classifier = "kknn") {
  options(keras.fit_verbose = 0)

  results <- map(datasets, function(dataset) {
    keras::backend()$clear_session()
    evaluate_features(dataset$x, dataset$y)
    resultsnoae <- dataset %>% experiment(FALSE, classifier)
    resultsaered <- dataset %>% experiment(supercore, classifier)
    resultspca <- dataset %>% experiment("pca", classifier)
    resultsae <- dataset %>% experiment(ruta::autoencoder, classifier)

    list(
      baseline = resultsnoae,
      pca = resultspca,
      basic = resultsae,
      reductive = resultsaered
    )
  })

  saveRDS(results, file = filename)
  results
}

# results <- run_experiment(list(read_data("data/wdbc.data", row.names = 1) %>% class_first("M")))
# results[[1]] %>% map(function(x) x %>% map("fisher") %>% as.numeric %>% summary)

get_tables <- function(results) {
  results %>% map(function(ds) {
    metrics <- names(ds[[1]][[1]])
    #print(metrics)
    table <- metrics %>% map(function(metric) {
      #ds %>% map(metric) %>% as.numeric %>% mean
      ds %>%
        map(function(k) k %>% map(metric) %>% as.numeric() %>% mean(na.rm = TRUE)) %>%
        do.call(cbind, .)
    }) %>% do.call(rbind, .) %>% t()
    colnames(table) <- metrics
    table
  })
}

print_tables <- function(tables) {
  sink("results.md")
  for (table in names(tables)) {
    c("\n##", table) %>% paste0(collapse = " ") %>% cat()
    tables[[table]] %>% knitr::kable(., format = "markdown") %>% print()
  }
  sink()
}
