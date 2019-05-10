#' @import dcme
#' @import ECoL
evaluate_features <- function(features, classes) {
  structure(list(
    fisher = dcme::F1(features, classes),
    volume = dcme::F2(features, classes),
    efficiency = dcme::F3(features, classes),
    errorknn = dcme::N3(features, classes),
    errorlin = ECoL::linearity.class(x = as.data.frame(features), y = classes, measures = "L2")
    # nonlin = ECoL::linearity.class(x = as.data.frame(features), y = classes, measures = "L3")
    # ir = dcme::IR(classes),
    # ppd = dcme::T2(features)
  ), class = list_metrics)
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

