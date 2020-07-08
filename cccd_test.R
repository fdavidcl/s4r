


digraph_measures <- function(ds) {
  positives <- ds$y == 1
  negatives <- ds$y == 0
  cover <- cccd::cccd.classifier(ds$x[positives, ], ds$x[negatives, ])

  total_balls <- length(cover$Rx) + length(cover$Ry)
  total_points <- length(ds$y)
  onb_total <- total_balls / total_points
  onb_avg <- (length(cover$Rx) / sum(positives) + length(cover$Ry) / sum(negatives)) / 2

  list(
    onb_total = onb_total,
    onb_avg = onb_avg
  )
}

devtools::load_all()
datasets <- dataset_list()

setwd("/media/datos/Documentos/research/publications/2020/scorer/s4r")
gina <- datasets$Gina()

digraph_measures(gina)
