visualize_embedding <- function(dataset_f, method, epochs = 50, dataset = dataset_f(), ...) {
  x <- dataset$x
  y <- dataset$y

  train_reduction(x, y, method, dataset$normalize)
}
