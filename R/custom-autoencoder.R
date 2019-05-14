CustomAutoencoder <- R6::R6Class("CustomAutoencoder",
  private = list(
    network = NULL,
    reconstruction_loss = NULL,
    weight = NULL,
    autoencoder = NULL,
    encoder = NULL,
    decoder = NULL,
    loss = NULL
  ),
  public = list(
    initialize = function(network, loss, weight) {
      private$network <- network
      private$reconstruction_loss <- loss
      private$weight <- weight
    },

    set_autoencoder = function(autoencoder) {
      private$autoencoder <- autoencoder
    },

    train = function(data, classes, epochs = 100, optimizer = keras::optimizer_rmsprop(), ...) {
      input_shape <- dim(data)[-1]
      private$to_keras(input_shape)

      private$autoencoder$add_loss(private$loss)
      private$autoencoder$compile(optimizer = optimizer)

      keras::fit(
        private$autoencoder,
        x = list(data, classes),
        epochs = epochs,
        ...
      )

      invisible(self)
    },

    encode = function(data) {
      private$encoder$predict(data)
    }
  )
)
