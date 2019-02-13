## Classes
supercore <- "supercore"
ruta_loss_reductive <- "ruta_loss_reductive"

#' @import purrr
#' @export
supercore <- function(network, loss, method = "fisher", weight = 0.01) {
  ruta::autoencoder(network, loss) %>% make_reductive(method, weight)
}

make_reductive <- function(learner, method = "fisher", weight = 0.01) {
  learner$loss <- loss_reductive(learner$loss, method, weight)
  class(learner) <- c(supercore, class(learner))
  learner
}

is_reductive <- function(learner) {
  ruta_loss_reductive %in% class(learner$loss)
}

loss_reductive <- function(reconstruction_loss, method = "fisher", weight = 0.01) {
  structure(list(
    reconstruction_loss = reconstruction_loss,
    method = method,
    weight = weight
  ),
  class = c(ruta_loss_reductive, ruta:::ruta_loss))
}

to_keras.supercore <- function(learner, weights_file = NULL) {
  models <- ruta:::to_keras.ruta_autoencoder(learner, weights_file = weights_file)
  # class input accepts ones and zeros
  class_pos <- keras::layer_input(list(1))
  models$autoencoder <- keras::keras_model(list(models$autoencoder$input, class_pos), models$autoencoder$output)

  models
}

#' @export
train.supercore <- function(
  learner,
  data,
  classes,
  epochs = 20,
  optimizer = keras::optimizer_rmsprop(),
  ...) {
  learner$input_shape <- dim(data)[-1]
  learner$models <- to_keras.supercore(learner)

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

to_keras.ruta_loss_reductive <- function(loss, learner) {
  if (loss$method != "fisher") {
    stop("Allowed complexity reduction methods: fisher")
  }

  enc_input <- learner$models$autoencoder$inputs[[1]]
  class_pos <- learner$models$autoencoder$inputs[[2]]
  encoding <- keras::get_layer(learner$models$autoencoder, "encoding") %>% keras::get_output_at(1)
  decodification <- learner$models$autoencoder$output

  # calculate negative instances
  class_neg <- 1 - class_pos

  # count positive and negative instances
  amount_pos <- keras::k_sum(class_pos) + keras::k_epsilon()  # add epsilon to prevent NaN
  amount_neg <- keras::k_sum(class_neg) + keras::k_epsilon()

  # keeps the value of the encoding or zero according to each instance's class
  encoding_if_pos <- class_pos * encoding
  encoding_if_neg <- class_neg * encoding

  # sum each feature over all instances in the batch
  mean_pos <- keras::k_sum(encoding_if_pos, axis = 1) / amount_pos
  mean_neg <- keras::k_sum(encoding_if_neg, axis = 1) / amount_neg

  # similarly calculate variance as E[X^2] - E[X]^2
  variance_pos <- keras::k_sum(keras::k_square(encoding_if_pos), axis = 1) / amount_pos - keras::k_square(mean_pos)
  variance_neg <- keras::k_sum(keras::k_square(encoding_if_neg), axis = 1) / amount_neg - keras::k_square(mean_neg)

  fisher_ratios <- keras::k_square(mean_pos - mean_neg) * (1 / (variance_pos + variance_neg + keras::k_epsilon()))
  fisher_gain <- keras::k_max(fisher_ratios)

  rec_loss <- (loss$reconstruction_loss %>% ruta:::to_keras(learner))(enc_input, decodification)
  # regularized_loss <- (rec_loss %>% keras::k_mean()) - loss$weight * fisher_gain
  regularized_loss <- (rec_loss %>% keras::k_mean()) - keras::k_tanh(fisher_gain)
  regularized_loss
}
