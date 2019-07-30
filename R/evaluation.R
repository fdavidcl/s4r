#' @import purrr
digraph_measures <- function(features, classes) {
  positives <- classes == 1
  negatives <- classes == 0

  cover <- safely(cccd::cccd.classifier)(features[positives, ], features[negatives, ])

  if (is.null(cover$error)) {
    cover <- cover$result
    total_balls <- length(cover$Rx) + length(cover$Ry)
    total_points <- length(classes)
    onb_total <- total_balls / total_points
    onb_avg <- (length(cover$Rx) / sum(positives) + length(cover$Ry) / sum(negatives)) / 2

  } else {
    warning(paste("Error calculating digraph measures:", cover$error$message))
    cat(" [digraph error] ")

    onb_total <- NA_real_
    onb_avg <- NA_real_
  }

  list(
    onb_total = onb_total,
    onb_avg = onb_avg
  )
}

#' @import dcme
#' @import ECoL
evaluate_features <- function(features, classes) {
  #return(structure(list(), class = list_metrics))
  df <- as.data.frame(features)
  structure(c(list(
    fisher =     safely(ECoL::overlapping, otherwise = NA_real_)(x = df, y = classes, measures = "F1")$result, #dcme::F1(features, classes)
    volume =     safely(ECoL::overlapping, otherwise = NA_real_)(x = df, y = classes, measures = "F2")$result, #dcme::F2(features, classes),
    efficiency = safely(ECoL::overlapping, otherwise = NA_real_)(x = df, y = classes, measures = "F3")$result,
    errorknn =   safely(ECoL::neighborhood, otherwise = NA_real_)(x = df, y = classes, measures = "N3")$result,
    errorlin =   safely(ECoL::linearity.class, otherwise = NA_real_)(x = df, y = classes, measures = "L2")$result,
    nonlin =     safely(ECoL::linearity.class, otherwise = NA_real_)(x = as.data.frame(features), y = classes, measures = "L3")$result
    # ir = dcme::IR(classes),
    # ppd = dcme::T2(features)
  ),
  digraph_measures(features, classes)), class = list_metrics)
}

#' @importFrom pROC auc
evaluate_model <- function(true_y, pred_y) {
  tp <- sum(true_y == pred_y & true_y == 1)
  tn <- sum(true_y == pred_y & true_y == 0)
  fp <- sum(true_y != pred_y & true_y == 0)
  fn <- sum(true_y != pred_y & true_y == 1)
  n <- length(true_y)

  accuracy <- mean(true_y == pred_y)
  # sensitivity <- tp / (tp + fn)
  # specificity <- tn / (tn + fp)
  # precision <- tp / (tp + fp)
  kappa <- 1 - (1 - accuracy) / (1 - (tp + fp)/n * (tp + fn)/n - (tn + fn)/n * (tn + fp)/n)
  fscore <- 2 * tp / (2 * tp + fp + fn)
  auc <- auc(true_y %>% as.numeric(), pred_y %>% as.numeric())

  # list_metrics: [numeric]
  metrics <- structure(list(
    fscore = fscore,
    kappa = kappa,
    auc = auc
  ), class = list_metrics)
}

correlations <- function(fmetric, cmetric) {
  c(pearson = cor(fmetric, cmetric),
    kendall = cor(fmetric, cmetric, method = "kendall"),
    spearman = cor(fmetric, cmetric, method = "spearman"))
}

metric_list <- function(folder = "checkpoints", method = "baseline", metric = "onb_total") {
  metricl <- numeric()

  baselines <- dir(file.path(folder, method), pattern = "_eval_", full.names = T)

  for (f in 1:length(baselines)) {
    metrics <- readRDS(baselines[f])

    next_val <- if ("result" %in% names(metrics$features[[metric]])) {
      metrics$features[[metric]]$result
    } else {
      metrics$features[[metric]]
    }

    if (is.null(next_val)) {
      next_val <- NA
    }

    metricl <- c(metricl, next_val)
  }

  metricl
}

eval_lists <- function(folder = "checkpoints", method = "slicer", eval = "kappa") {
  classifier_metrics <- list(
    knn = numeric(),
    svmRadial = numeric(),
    mlp = numeric()
  )

  reductors <- dir(file.path(folder, method), pattern = "_eval_", full.names = T)

  for (f in 1:length(reductors)) {
    classifiers <- readRDS(reductors[f])$classifiers

    for (nm in names(classifier_metrics)) {
      if (!(is.null(classifiers) || is.null(classifiers[[nm]]))) {
        classifier_metrics[[nm]] <- c(classifier_metrics[[nm]], classifiers[[nm]][[eval]])
      } else {
        classifier_metrics[[nm]] <- c(classifier_metrics[[nm]], NA)
      }
    }
  }

  classifier_metrics
}

evaluate_correlations_from_files <- function(folder = "checkpoints", method = "slicer", metric = "onb_total") {
  basemetrics <- metric_list(folder = folder, metric = metric) # numeric vector
  evalmetrics <- eval_lists(folder = folder, method = method) # list of numeric vectors

  useful <-
    !(
      (evalmetrics %>% map(is.na) %>% reduce(`|`))
      |
      (basemetrics %>% is.na)
    )

  map(evalmetrics, ~ correlations(.[useful], basemetrics[useful]))
}
