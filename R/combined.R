combinedae <- function(network, loss, fisher_weight = .01, svm_weight = .1) {
  ruta::autoencoder(network, loss) %>% make_combined(fisher_weight, svm_weight)
}

## Classes
combined <- "combined"
ruta_loss_combined <- "ruta_loss_combined"


make_combined <- function(learner, fisher, svm) {
  learner$loss <- loss_combined(learner$loss, fisher, svm)
  class(learner) <- c(combined, class(learner))
  learner
}

is_combined <- function(learner) {
  ruta_loss_combined %in% class(learner$loss)
}

loss_combined <- function(reconstruction_loss, fisher, svm) {
  structure(list(
    reconstruction_loss = reconstruction_loss,
    fisher = fisher,
    svm = svm
  ),
  class = c(ruta_loss_combined, ruta:::ruta_loss))
}

to_keras.combined <- function(learner, weights_file = NULL) {
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
train.combined <- function(
  learner,
  data,
  classes,
  epochs = 20,
  optimizer = keras::optimizer_rmsprop(),
  ...) {
  learner$input_shape <- dim(data)[-1]
  learner$models <- to_keras.combined(learner)

  loss_node <- ruta:::to_keras(learner$loss, learner)
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

to_keras.ruta_loss_combined <- function(loss, learner) {

  enc_input <- learner$models$autoencoder$inputs[[1]]
  class_pos <- learner$models$autoencoder$inputs[[2]]
  encoding_ly <- keras::get_layer(learner$models$autoencoder, "encoding")
  encoding <- encoding_ly %>% keras::get_output_at(1)
  encoding_weight <- keras::get_layer(learner$models$autoencoder, "pre_encoding")$weights[[1]]
  svm_ly <- keras::get_layer(learner$models$autoencoder, "svm_layer")
  svm_weight <- keras::k_concatenate(svm_ly$weights, axis = 1)
  svm_out <- keras::get_output_at(svm_ly, 1)
  decodification <- learner$models$autoencoder$output[[1]]

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
  fisher_gain <- keras::k_mean(fisher_ratios)

  # Normalization as seen in https://github.com/lpfgarcia/ECoL/. Now complexity is reduced when the metric is reduced
  fisher_loss_normalized <- 1 / (fisher_gain + 1)
  # fisher_gain_normalized <- keras::k_tanh(fisher_gain)

  encoding_dev <- 1 / (keras::k_epsilon() + keras::k_std(encoding, axis = 1) %>% keras::k_sum())

  t_n <- 2 * class_pos - 1 # class \in {-1, 1}
  sum_term <- keras::k_square(keras::k_max(1 - svm_out * t_n, 0))
  svm_loss <- keras::k_sum(sum_term)

  weight_reg <- keras::k_sum(svm_weight * svm_weight) / 2 #+ keras::k_sum(encoding_weight * encoding_weight) / 2)



  rec_loss <- (loss$reconstruction_loss %>% ruta:::to_keras(learner))(enc_input, decodification)
  # regularized_loss <- (rec_loss %>% keras::k_mean()) - loss$weight * fisher_gain
  regularized_loss <- keras::k_mean(rec_loss) +
    loss$fisher * fisher_loss_normalized + weight_reg + loss$svm * svm_loss #+ encoding_dev
  regularized_loss
}

test_combined_iris <- function(cl = "versicolor", fisher = 0.1, svm = 1) {
  dat <- as.matrix(iris[,1:4])
  intrain <- sample(1:150, 100)
  ae <- combinedae(input() + dense(10, "relu") + dense(2, "sigmoid") + dense(10, "relu") + output(), "mean_squared_error", fisher, svm)
  ae <- train.combined(ae, dat[intrain,], as.numeric(iris$Species == cl), epochs = 200)
  print(mean(abs(decode(ae, encode(ae, dat[-intrain,])) - dat[-intrain,])))
  ae %>% encode(dat) %>% plot(col = as.numeric(iris$Species == cl) + 1)
}
