#' @import caret
experiment <- function(dataset, autoencoder_f, method, normalize = TRUE, ...) {
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

    model <- train_model(autoencoder_f, method, train_x, train_y, normalized = TRUE, ...)
    outs <- test_model(model, test_x)
    results[[i]] <- c(evaluate_features(outs$features, test_y), evaluate_model(test_y, outs$predictions))
  }

  results
}

#' @import purrr
#' @export
run_experiment <- function(datasets = dataset_list(), folder = "results", classifier = "kknn") {
  options(keras.fit_verbose = 0)

  dir.create(folder, recursive = TRUE)

  results <- map2(datasets, names(datasets), function(dataset_f, name) {
    keras::backend()$clear_session()
    gc()
    dataset <- dataset_f()
    resultsnoae <- dataset %>% experiment(FALSE, classifier)
    resultsaered <- dataset %>% experiment(supercore, classifier)
    resultspca <- dataset %>% experiment("pca", classifier)
    resultsae <- dataset %>% experiment(ruta::autoencoder, classifier)
    resultssvm <- dataset %>% experiment(svmae, classifier)
    resultscom1 <- dataset %>% experiment(combinedae, classifier, fisher = 0.01, svm = 1)
    resultscom2 <- dataset %>% experiment(combinedae, classifier, fisher = 0.1, svm = 1)
    resultscom3 <- dataset %>% experiment(combinedae, classifier, fisher = 0.01, svm = 0.01)

    this_dataset <- list(
      baseline = resultsnoae,
      pca = resultspca,
      svm = resultssvm,
      basic = resultsae,
      supercore = resultsaered,
      f001svm1 = resultscom1,
      f01svm1 = resultscom2,
      f001svm001 = resultscom3
    )

    saveRDS(this_dataset, file = file.path(folder, paste0(name, ".rds")))

    this_dataset
  })

  saveRDS(results, file = file.path(folder, "results.rds"))
  results
}

PARAMS <- expand.grid(svm = c(0.01, 0.05, 0.1, 0.5, 1), fisher = c(0.01, 0.05, 0.1, 0.5, 1))

param_opt <- function(datasets = dataset_list(), filename = "param_opt.rds", classifier = "kknn") {
  options(keras.fit_verbose = 0)

  results <- map(datasets, function(dataset) {
    keras::backend()$clear_session()

    this_dataset <- vector(mode = "list", length = nrow(PARAMS))
    for (i in 1:nrow(PARAMS)) {
      svm <- PARAMS[i,"svm"]
      fisher <- PARAMS[i,"fisher"]
      this_dataset[[i]] <- c(
        list(svm = svm, fisher = fisher),
        experiment(dataset, combinedae, classifier, svm_weight = svm, fisher_weight = fisher)
      )
    }

    this_dataset
  })

  saveRDS(results, file = filename)
  results
}

param_to_results <- function(param_results) {
  param_results %>% map(function(r) {
    pars <- r %>% map(function(k) k[1:2] %>% paste0(collapse = "_"))
    newr <- r %>% map(function(k) {
      newk <- k[3:length(k)]
    })
    names(newr) <- pars
    newr
  })
}

# best_res <- param_to_results(readRDS("param_opt.rds")) %>% get_tables() %>% map(function(df) df[order(-df[,"auc"])[1,drop=FALSE],,drop=FALSE])
# best_combination <- param_to_results(readRDS("param_opt.rds")) %>% get_tables() %>% map(function(df) df[order(-df[,"auc"])[1,drop=FALSE],,drop=FALSE]) %>% map(row.names) %>% as.character() %>% table()


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
get_tests <- function(results = NULL, result_file = NULL, quiet = T, control = "supercore") {
  if (is.null(results))
    results <- readRDS(result_file)

  tab <- results %>% get_tables()

  metrics <- colnames(tab[[1]])
  allres <- map(metrics, function(metric) {
    tab %>% map(~ .[, metric]) %>% do.call(rbind, .)
  })
  names(allres) <- metrics

  gridExtra::grid.arrange(
    scmamp::plotDensities(allres$fisher, na.rm = TRUE, show.legend = FALSE) + ggplot2::xlab("Fisher"),
    scmamp::plotDensities(allres$efficiency, na.rm = TRUE) + ggplot2::xlab("Efficiency"),
    scmamp::plotDensities(allres$fscore, na.rm = TRUE, show.legend = FALSE) + ggplot2::xlab("Fscore"),
    scmamp::plotDensities(allres$kappa, na.rm = TRUE) + ggplot2::xlab("Kappa"),
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
    controln <- which(colnames(metric_results) == control)
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
      contrtest <- scmamp::friedmanAlignedRanksPost(metric_results, control = controln)

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

  # colnames(results$ranking) <- colnames(metric_results)
  colnames(results$different) <- setdiff(colnames(metric_results), control)
  results
}

## TODO: combinar supercore y l2svm, seleccionar datasets alta dimensionalidad
## la propuesta no sería la combinación sino cada una por separado

print_all_tests <- function() {
  files <- c(dir("config_200ep_gain", full.names = T), dir("config_200ep_loss", full.names = T))
  for (f in files) {
    cat("\n\n#", f, "\n")
    t <- get_tests(f)
    cat(t %>% knitr::kable())
  }
}
