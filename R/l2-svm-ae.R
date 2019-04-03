## Classes
svmae <- "svmae"
ruta_loss_svm <- "ruta_loss_svm"

#' @import purrr
#' @export
svmae <- function(network, loss, method = "svm", weight = 1) {
  ruta::autoencoder(network, loss) %>% make_svm(method, weight)
}

make_svm <- function(learner, method = "svm", weight = 1) {
  learner$loss <- loss_svm(learner$loss, method, weight)
  class(learner) <- c(svmae, class(learner))
  learner
}

is_svm <- function(learner) {
  ruta_loss_svm %in% class(learner$loss)
}

loss_svm <- function(reconstruction_loss, method = "svm", weight = 1) {
  structure(list(
    reconstruction_loss = reconstruction_loss,
    method = method,
    weight = weight
  ),
  class = c(ruta_loss_svm, ruta:::ruta_loss))
}

to_keras.svmae <- function(learner, weights_file = NULL) {
  models <- ruta:::to_keras.ruta_autoencoder(learner, weights_file = weights_file)

  encoding <- keras::get_layer(models$autoencoder, "encoding") %>% keras::get_output_at(1)
  svmtrainer <- layer_svm(encoding, "svm_layer")

  # class input accepts ones and zeros
  class_pos <- keras::layer_input(list(1))
  models$autoencoder <- keras::keras_model(list(models$autoencoder$input, class_pos),
                                           list(models$autoencoder$output, svmtrainer))

  models
}

#' @export
train.svmae <- function(
  learner,
  data,
  classes,
  epochs = 20,
  optimizer = keras::optimizer_rmsprop(),
  ...) {
  learner$input_shape <- dim(data)[-1]
  learner$models <- to_keras.svmae(learner)

  loss_node <- learner$loss %>% ruta:::to_keras(learner)
  learner$models$autoencoder$add_loss(loss_node)

  learner$models$autoencoder$compile(optimizer = optimizer)

  input_data <- if (is.null(learner$filter)) {
    data
  } else {
    ruta::apply_filter(learner$filter, data)
  }

  keras::fit(
    learner$models$autoencoder,
    x = list(input_data, classes),
    epochs = epochs,
    ...
  )

  invisible(learner)
}

to_keras.ruta_loss_svm <- function(loss, learner) {
  if (loss$method != "svm") {
    stop("Allowed complexity reduction methods: svm")
  }

  enc_input <- learner$models$autoencoder$inputs[[1]]
  class_pos <- learner$models$autoencoder$inputs[[2]]
  encoding <- keras::get_layer(learner$models$autoencoder, "encoding") %>% keras::get_output_at(1)
  svm_ly <- keras::get_layer(learner$models$autoencoder, "svm_layer")
  svm_weight <- svm_ly$weights[[1]]
  svm_out <- keras::get_output_at(svm_ly, 1)
  # learner$models$autoencoder$layers[[2]]$trainable_weights[[3]] <- svm_weight
  # learner$models$autoencoder$layers[-1]$trainable_weights$extend(list(svm_weight))
  decodification <- learner$models$autoencoder$output[[1]]
  t_n <- 2 * class_pos - 1 # class \in {-1, 1}

  sum_term <- keras::k_square(keras::k_max(1 - svm_out * t_n, 0))
  svm_loss <- keras::k_sum(sum_term)

  weight_reg <- keras::k_sum(svm_weight * svm_weight) / 2

  rec_loss <- (loss$reconstruction_loss %>% ruta:::to_keras(learner))(enc_input, decodification)
  # regularized_loss <- (rec_loss %>% keras::k_mean()) - loss$weight * fisher_gain
  regularized_loss <- keras::k_mean(rec_loss) + weight_reg + loss$weight * svm_loss
  regularized_loss
}

test_svm_iris <- function(cl = "versicolor", weight = 1) {
  ae <- svmae(input() + dense(10, "relu") + dense(2, "sigmoid") + dense(10, "relu") + output(), "mean_squared_error", "svm", weight)
  ae <- train.svmae(ae, as.matrix(iris[,1:4]), as.numeric(iris$Species == cl), epochs = 200)
  ae %>% encode(as.matrix(iris[,1:4])) %>% plot(col = as.numeric(iris$Species == cl) + 1)
}

test_sc_iris <- function(cl = "versicolor", weight = 1) {
  ae <- supercore(input() + dense(10, "relu") + dense(2, "sigmoid") + dense(10, "relu") + output(), "mean_squared_error", "fisher", weight)
  ae <- train.supercore(ae, as.matrix(iris[,1:4]), as.numeric(iris$Species == cl), epochs = 200)
  ae %>% encode(as.matrix(iris[,1:4])) %>% plot(col = as.numeric(iris$Species == cl) + 1)
}
