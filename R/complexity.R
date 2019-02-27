#' @import dcme
evaluate_features <- function(features, classes) {
  negative <- features[classes == 0,] %>% expand_dims()
  positive <- features[classes == 1,] %>% expand_dims()

  list(
    fisher = dcme::F1(features, classes),
    volume = dcme::F2(features, classes),
    efficiency = dcme::F3(features, classes),
    errorknn = dcme::N3(features, classes),
    ir = dcme::IR(classes),
    ppd = dcme::T2(features)
  )
}
