SVMLayer <- R6::R6Class("R6Layer",
  inherit = keras::KerasLayer,
  public = list(
    # output_dim = NULL,
    kernel = NULL,
    initialize = function() {
      # self$output_dim <- output_dim
    },
    build = function(input_shape) {
      self$kernel <- self$add_weight(
        name = "svm_kernel",
        shape = list(input_shape[[2]]),
        initializer = keras::initializer_zeros(),
        trainable = TRUE
      )
    },
    call = function(x, mask = NULL) {
      keras::k_sum(self$kernel * x, axis = 2, keepdims = TRUE)
    },
    compute_output_shape = function(input_shape) {
      list(input_shape[[1]], 1)
    }
  )
)

layer_svm <- function(object, name = NULL, trainable = TRUE) {
  keras::create_layer(SVMLayer, object, list(
    name = name,
    trainable = trainable,
    dtype = "float32"
  ))
}
