#' @include custom-autoencoder.R
Scorer <- R6::R6Class("Scorer",
  inherit = CustomAutoencoder,
  private = list(
    to_keras = function(input_shape) {
      learner <- ruta::autoencoder(private$network, private$reconstruction_loss)
      learner$input_shape <- input_shape
      models <- ruta:::to_keras.ruta_autoencoder(learner)

      # Class input accepts ones and zeros
      class_pos <- keras::layer_input(list(1))

      # Compute the loss
      enc_input <- models$autoencoder$inputs[[1]]
      encoding <- keras::get_layer(models$autoencoder, "encoding") %>% keras::get_output_at(1)
      decodification <- models$autoencoder$output

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

      rec_loss <- (learner$loss %>% ruta:::to_keras(learner))(enc_input, decodification)
      regularized_loss <- (rec_loss %>% keras::k_mean()) + private$weight * fisher_loss_normalized
      # regularized_loss <- (rec_loss %>% keras::k_mean()) - fisher_gain_normalized

      models$autoencoder <- keras::keras_model(
        list(models$autoencoder$input, class_pos),
        list(models$autoencoder$output)
      )

      private$autoencoder <- models$autoencoder
      private$encoder <- models$encoder
      private$decoder <- models$decoder
      private$loss <- regularized_loss
    }
  ),
  public = list(
  )
)