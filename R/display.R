printable <- function(x, ...) UseMethod("printable")

printable.list_metrics <- function(x, ...) {
  structure(as.numeric(x), names = names(x))
}

print.list_metrics <- function(x, ...) {
  print(printable(x), ...)
  invisible(x)
}

printable.results_dataset <- function(x, ...) {
  average <- function(obj) {
    map(obj, printable) %>% do.call(rbind, .) %>% colMeans
  }
  printable_features <- average(x$features)
  printable_cls <- map(x$classifiers, average)

  do.call(c, c(list(printable_features), printable_cls))
}

print.results_dataset <- function(x, ...) {
  cat("Measures averaged over", length(x[[1]]), "folds:\n")
  print(printable(x))
  invisible(x)
}

printable.results_experiment <- function(x, ...) {
  x %>% map(~ map(., printable)) %>% map(~ do.call(rbind, .))
}

print.results_experiment <- function(x, ...) {
  reslist <- printable(x)
  walk2(reslist, names(x), function(obj, name) {
    cat("================\n", name, "\n================\n", sep = "")

    print(obj)
  })

  invisible(x)
}

printable.default <- function(x, ...) x

# x: results_experiment: { dataset = { method = { "features" = [ { metric = numeric } ], "classifiers" = [ { metric = numeric } ] } } }
# return: { dataset = { method = [ { metric = numeric } ] } }
select_classifier <- function(x, cl) {
  map(x, function(dataset)
    map(dataset, function(method) {
      #print(method$classifiers[[cl]])
      listzip(method$features, method$classifiers[[cl]])
    }))
}

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

get_table_bests <- function(tables) {
  tables %>%
    map(function(tab) {
      row.names(tab)[apply(tab, 2, function(col) match(max(col), col))]
    }) %>%
    do.call(rbind, .)
}

print_tables <- function(tables, filename = NULL) {
  if (!is.null(file)) sink(filename)
  for (table in names(tables)) {
    c("\n##", table) %>% paste0(collapse = " ") %>% cat()
    tables[[table]] %>% knitr::kable(., format = "markdown") %>% print()
  }
  if (!is.null(file)) sink()
}

myrank <- function(x, decreasing = FALSE, ...) {
  if (decreasing) x <- -x
  rank(x, ...)
}

#' @import scmamp
get_tests <- function(tables, quiet = T, control = "slicer", exclude = character(0)) {
  # if (is.null(results))
  #   results <- readRDS(result_file)
  #
  # tab <- results %>% get_tables()
  tab <- tables

  include <- setdiff(rownames(tab[[1]]), exclude)
  print(include)
  tab <- tab %>% map(function(tb) tb[include, ])

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
    errorlin = FALSE,
    nonlin = FALSE,
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
  # colnames(results$different) <- include
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
