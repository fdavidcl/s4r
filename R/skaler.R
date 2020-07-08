#' Skaler
#'
#' Supervised overlap reduction with Kullback-Leibler divergence penalty
#'
#' @section Arguments:
#'
#'   `network` Ruta network object indicating the desired architecture.
#'
#'   `loss` Character scalar (e.g. `"mean_squared_error"`) or Ruta loss object.
#'
#'   `weight` Weight of the applied penalty.
#'
#' @section Methods:
#'
#'   `$new()` Initialize new object
#'
#'   `$set_autoencoder()` Save a Keras model as internal autoencoder.
#'
#'   `$train()` Train the autoencoder with data.
#'
#'   `$encode()` Once trained, encode new data.
#'
#'   `$penalty()` Retrieve the Keras tensor which computes the penalty for the loss.
#'
#' @name Scorer
NULL

#' @include custom-autoencoder.R
#' @export
Skaler <- R6::R6Class("Skaler",
  inherit = CustomAutoencoder,
  private = list(
    to_keras = function(input_shape) {
      learner <- ruta::autoencoder(private$network, private$reconstruction_loss)
      learner$input_shape <- input_shape
      models <- ruta:::to_keras(learner)

      # Class input accepts ones and zeros
      class_pos <- keras::layer_input(list(1))
      models$autoencoder <- keras::keras_model(
        list(models$autoencoder$input, class_pos),
        list(models$autoencoder$output)
      )

      private$autoencoder <- models$autoencoder
      private$encoder <- models$encoder
      private$decoder <- models$decoder

      # Compute the loss
      start <- models$autoencoder$inputs[[1]]
      end <- models$autoencoder$output
      rec_loss <- (ruta:::to_keras(learner$loss, learner))(start, end)
      private$loss <- keras::k_mean(rec_loss) + self$penalty()
    }
  ),
  public = list(
    penalty = function(input_shape) {
      # Compute the loss
      class_pos <- private$autoencoder$inputs[[2]]
      encoding <- keras::get_layer(private$autoencoder, "encoding") %>% keras::get_output_at(1)

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

      p_high_given_pos <- keras::k_softmax(mean_pos)
      p_high_given_neg <- keras::k_softmax(mean_neg)
      p_high <- keras::k_clip(keras::k_mean(encoding, axis = 1), 0, 1)
      # p_highest <- keras::k_softmax(mean_encoding)

      kld <-
        keras::k_sum(p_high_given_pos * keras::k_log(
          p_high_given_pos / (p_high_given_neg + keras::k_epsilon()) + keras::k_epsilon()
        ) / log(2)) +
        keras::k_sum(p_high_given_neg * keras::k_log(
          p_high_given_neg / (p_high_given_pos + keras::k_epsilon()) + keras::k_epsilon()
        ) / log(2))
      entropy <- keras::k_mean(
        -(1 - p_high) * keras::k_log(1 - p_high + keras::k_epsilon()) / log(2) -
          (p_high) * keras::k_log(p_high + keras::k_epsilon()) / log(2)
      )

      private$weight * (- kld - entropy)
    }
  )
)
