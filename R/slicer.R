#' @include custom-autoencoder.R
#' @include svm_layer.R
Slicer <- R6::R6Class("Slicer",
  inherit = CustomAutoencoder,
  private = list(
    to_keras = function(input_shape) {
      learner <- ruta::autoencoder(private$network, private$reconstruction_loss)
      learner$input_shape <- input_shape
      models <- ruta:::to_keras.ruta_autoencoder(learner)

      encoding <- keras::get_layer(models$autoencoder, "encoding") %>% keras::get_output_at(1)
      svmtrainer <- layer_svm(encoding, "svm_layer")

      # class input accepts ones and zeros
      class_pos <- keras::layer_input(list(1))
      models$autoencoder <- keras::keras_model(list(models$autoencoder$input, class_pos),
                                               list(models$autoencoder$output, svmtrainer))

      enc_input <- models$autoencoder$inputs[[1]]
      class_pos <- models$autoencoder$inputs[[2]]
      encoding <- keras::get_layer(models$autoencoder, "encoding") %>% keras::get_output_at(1)
      svm_ly <- keras::get_layer(models$autoencoder, "svm_layer")
      svm_weight <- svm_ly$weights[[1]]
      svm_out <- keras::get_output_at(svm_ly, 1)
      decodification <- models$autoencoder$output[[1]]
      t_n <- 2 * class_pos - 1 # class \in {-1, 1}

      sum_term <- keras::k_square(1 - svm_out * t_n)
      svm_loss <- keras::k_sum(sum_term)

      weight_reg <- keras::k_sum(svm_weight * svm_weight) / 2

      rec_loss <- (learner$loss %>% ruta:::to_keras(learner))(enc_input, decodification)

      regularized_loss <- keras::k_mean(rec_loss) + weight_reg + private$weight * svm_loss

      private$autoencoder <- models$autoencoder
      private$encoder <- models$encoder
      private$decoder <- models$decoder
      private$loss <- regularized_loss
    }
  ),
  public = list(
  )
)
