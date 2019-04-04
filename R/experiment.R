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
    # resultspca <- dataset %>% experiment("pca", classifier)
    # resultsae <- dataset %>% experiment(ruta::autoencoder, classifier)
    resultssvm <- dataset %>% experiment(svmae, classifier)

    list(
      baseline = resultsnoae,
      # pca = resultspca,
      svm = resultssvm,
      # basic = resultsae,
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

myrank <- function(x, decreasing = FALSE, ...) {
  if (decreasing) x <- -x
  rank(x, ...)
}

#' @import scmamp
get_tests <- function(result_file, quiet = T) {
  results <- readRDS(result_file)
  tab <- results %>% get_tables()

  metrics <- colnames(tab[[1]])
  allres <- map(metrics, function(metric) {
    tab %>% map(~ .[, metric]) %>% do.call(rbind, .)
  })
  names(allres) <- metrics

  # layout(matrix(1:4, nrow = 2))
  gridExtra::grid.arrange(
    scmamp::plotDensities(allres$fisher, na.rm = TRUE),
    scmamp::plotDensities(allres$efficiency, na.rm = TRUE),
    scmamp::plotDensities(allres$fscore, na.rm = TRUE),
    scmamp::plotDensities(allres$kappa, na.rm = TRUE),
    nrow = 2
  )

  higher_is_better <- list(
    fisher = TRUE,
    volume = TRUE,
    efficiency = TRUE,
    errorknn = FALSE,
    ir = FALSE,
    ppd = TRUE,
    fscore = TRUE,
    kappa = TRUE,
    auc = TRUE
  )

  ranking_table <- list()
  different_table <- list()

  for (metric in metrics) {
    if (!quiet) cat("\nNow evaluating", metric, "\n")
    metric_results <- allres[[metric]]
    ranking <- t(apply(metric_results, 1, myrank, decreasing = higher_is_better[[metric]])) %>% colMeans()
    ranking_table[[metric]] <- ranking
    if (!quiet) cat("Average ranking: ",
        paste0(
          colnames(metric_results),
          " = ",
          ranking,
          collapse = ", "
        ),
        "\n")

    comptest <- scmamp::friedmanAlignedRanksTest(metric_results)
    different <- if (comptest$p.value < 0.01) {
      if (!quiet) cat("With confidence > 0.99, not all algorithms perform the same.\n")
      contrtest <- scmamp::friedmanAlignedRanksPost(metric_results, control = 2)

      noneq <- !is.na(contrtest) & contrtest < 0.01
      namesnoneq <- colnames(contrtest)[noneq]
      if (!quiet) cat(
        "These methods performed differently than supercore:",
        paste0(namesnoneq, collapse = ', '),
        "\n"
      )
      which(noneq)
    } else {
      if (!quiet) cat("All methods performed the same than supercore")
      numeric(0)
    }

    different_table[[metric]] <- different
  }

  different_table <- different_table %>% map(function(d) {
    v <- logical(ncol(metric_results) - 1)
    if (length(d) > 0) v[d] <- TRUE
    v
  })

  results <- list(
    ranking = do.call(rbind, ranking_table),
    different = do.call(rbind, different_table)
  )

  colnames(results$ranking) <- colnames(metric_results)
  colnames(results$different) <- colnames(metric_results)[-4]
  results
}

print_all_tests <- function() {
  files <- c(dir("config_200ep_gain", full.names = T), dir("config_200ep_loss", full.names = T))
  for (f in files) {
    cat("\n\n#", f, "\n")
    t <- get_tests(f)
    cat(t %>% knitr::kable())
  }
}
