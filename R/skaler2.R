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
Skaler2 <- R6::R6Class("Skaler2",
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
      p_high_given_pos <- keras::k_softmax(keras::k_sum(encoding_if_pos, axis = 1))# / amount_pos
      p_high_given_neg <- keras::k_softmax(keras::k_sum(encoding_if_neg, axis = 1))# / amount_neg
      p_low_given_pos <- 1 - p_high_given_pos # keras::k_sum(class_pos - encoding_if_pos, axis = 1) / amount_pos + keras::k_epsilon()
      p_low_given_neg <- 1 - p_high_given_neg # keras::k_sum(class_neg - encoding_if_neg, axis = 1) / amount_neg + keras::k_epsilon()
      mean_encoding <- keras::k_mean(encoding, axis = 1)

      p_pos <- keras::k_mean(class_pos)
      p_neg <- 1 - p_pos
      p_high <- keras::k_clip(mean_encoding, 0, 1)

      entropy <- - p_high * keras::k_log(p_high + keras::k_epsilon()) - (1 - p_high) * keras::k_log(1 - p_high + keras::k_epsilon())

      cond_entropy <- -(
        p_pos * p_high_given_pos * keras::k_log(p_high_given_pos + keras::k_epsilon()) +
        p_neg * p_high_given_neg * keras::k_log(p_high_given_neg + keras::k_epsilon()) +
        p_pos * p_low_given_pos * keras::k_log(p_low_given_pos + keras::k_epsilon()) +
        p_neg * p_low_given_neg * keras::k_log(p_low_given_neg + keras::k_epsilon())
      )

      mutual_inf <- keras::k_mean(entropy - cond_entropy)

      private$weight * (- mutual_inf)
    },
    penalty_mutual_inf = function(input_shape) {
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

      # sigmoid is required
      p_pos <- keras::k_mean(class_pos) + keras::k_epsilon()
      p_neg <- keras::k_mean(class_pos) + keras::k_epsilon()
      p_pos_act <- keras::k_mean(encoding_if_pos, axis = 1) + keras::k_epsilon()
      p_neg_act <- keras::k_mean(encoding_if_neg, axis = 1) + keras::k_epsilon()
      p_pos_low <- keras::k_mean(class_pos - encoding_if_pos, axis = 1) + keras::k_epsilon()
      p_neg_low <- keras::k_mean(class_neg - encoding_if_neg, axis = 1) + keras::k_epsilon()
      p_act <- keras::k_mean(encoding, axis = 1) + keras::k_epsilon()
      p_low <- 1 - p_act

      kld <- keras::k_mean(
          # p_pos_act * keras::k_log(p_pos_act / (p_pos * p_act)) +
          # p_neg_act * keras::k_log(p_neg_act / (p_neg * p_act)) +
          p_pos_low * keras::k_log(p_pos_low / (p_pos * p_low)) +
          p_neg_low * keras::k_log(p_neg_low / (p_neg * p_low))
      )

      - private$weight * kld
    },
    penalty_kl = function(input_shape) {
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

      # sigmoid is required
      p_pos_act <- keras::k_mean(encoding_if_pos, axis = 1) + keras::k_epsilon()
      p_neg_act <- keras::k_mean(encoding_if_neg, axis = 1) + keras::k_epsilon()
      p_act <- keras::k_mean(encoding, axis = 1) + keras::k_epsilon()

      kld <- keras::k_mean((p_neg_act - p_pos_act) / p_act * keras::k_log(p_neg_act / p_pos_act))

      # entropy <- keras::k_mean(-
      #     (1 - p_act) * keras::k_log(1 - p_act) -
      #     (p_act) * keras::k_log(p_act)
      # )
      p_high <- 0.2
      q_high <- p_act
      sparsity <- keras::k_sum(p_high * keras::k_log(p_high/q_high) +
                                 (1 - p_high) * keras::k_log((1 - p_high)/(1 - q_high)))

      private$weight * (-kld + sparsity / 2)

    },
    penalty_dkl_matrices = function(input_shape) {
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
      mean_encoding <- keras::k_mean(encoding, axis = 1)

      p_pos <- keras::k_expand_dims(keras::k_softmax(mean_pos))
      p_neg <- keras::k_expand_dims(keras::k_softmax(mean_neg), 1L)
      p_feat <- keras::k_clip(mean_encoding, 0 + keras::k_epsilon(), 1 - keras::k_epsilon())

      # compute joint probability
      p <- keras::k_dot(p_pos, p_neg)

      # compute an "almost identity" joint probability matrix
      side <- keras::k_int_shape(p)[[1]]
      diag_almost_1 <- keras::k_eye(side) * (1 - (side - 1) * keras::k_epsilon()) # force sum 1 in identity
      complem_eps <- (1 - keras::k_eye(side)) * keras::k_epsilon()
      id <- diag_almost_1 + complem_eps

      # KL divergence between the joint probability and the identity
      kld <- keras::k_sum(p * keras::k_log(p / id))

      entropy <- keras::k_mean(-
          (1 - p_feat) * keras::k_log(1 - p_feat) -
          (p_feat) * keras::k_log(p_feat)
      )

      - private$weight * (kld + entropy)
    }
  )
)
