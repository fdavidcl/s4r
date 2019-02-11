name <- function(features) {
  colnames(features) <- paste0("h", 1:dim(features)[2])
  features
}

#' @import purrr
preparation <- function(dataset, class_name = "class", value_positive = 1) {
  filtered <- if (is_character(class_name)) which(names(dataset) == class_name) else class_name
  list(
    x = dataset[, -filtered] %>% as.matrix(),
    y = (dataset[, class_name] %in% value_positive) %>% as.integer() %>% as.factor()
  )
}
read_data <- purrr::partial(read.csv, header = FALSE)
class_first <- purrr::partial(preparation, class_name = 1)
class_last <- function(dataset, value_positive = 1)
  preparation(dataset, class_name = ncol(dataset), value_positive = value_positive)
vs <- function(dataset, class_pos, positive, negative) {
  dataset[dataset[,class_pos] %in% c(positive, negative), ] %>%
    preparation(class_pos, positive)
}
vs_first <- purrr::partial(vs, class_pos = 1)
vs_last <- function(dataset, positive, negative)
  vs(dataset, class_pos = ncol(dataset), positive, negative)

#' @import purrr
train_model <- function(autoencoder_f, method, train_x, train_y, normalized) {

  hidden_dim <- ceiling(0.1 * dim(train_x)[2])

  if (is_logical(autoencoder_f) && autoencoder_f == FALSE) {
    feature_extractor <- function(x) return(x)
    features <- train_x
  } else if (is_character(autoencoder_f) && autoencoder_f == "pca") {
    ## Extract features

    pca <- train_x %>% prcomp(scale = normalized)
    feature_extractor <- function(x) predict(pca, x)[, 1:hidden_dim] %>% array(c(dim(x)[1], hidden_dim)) %>% name()
    features <- feature_extractor(train_x)
  } else {
    activation <- if (normalized) "sigmoid" else "linear"
    network <- ruta::input() + ruta::dense(hidden_dim, "relu") + ruta::output(activation)

    # Do not use binary crossentropy (and sigmoid activation) *unless* the data has been
    # accordingly normalized (to the [0, 1] interval)
    loss <- if (normalized) "binary_crossentropy" else "mean_squared_error"
    feature_extractor <- autoencoder_f(network, loss = loss)
    print(feature_extractor)
    feature_extractor <- if (is_reductive(feature_extractor))
      feature_extractor %>% train.supercore(train_x, classes = as.numeric(train_y) - 1, epochs = 200)
    else
      feature_extractor %>% ruta::train(train_x, epochs = 200)

    feature_extractor <- purrr::compose(name, purrr::partial(ruta::encode, learner = feature_extractor, .lazy = FALSE))
    features <- feature_extractor(train_x)
  }

  str(features)

  ## Classifier
  # ctrl <- trainControl()
  classifier <- caret::train(features, train_y, method = method)
  print(classifier)

  list(
    feature_extractor = feature_extractor,
    classifier = classifier
  )
}

#' @import purrr
test_model <- function(model, test_x) {
  features <- model$feature_extractor(test_x)
  str(features)
  predictions <- model$classifier %>% predict(newdata = features)
}

evaluate_model <- function(true_y, pred_y) {
  tp <- sum(true_y == pred_y & true_y == 1)
  tn <- sum(true_y == pred_y & true_y == 0)
  fp <- sum(true_y != pred_y & true_y == 0)
  fn <- sum(true_y != pred_y & true_y == 1)

  list(
    accuracy = mean(true_y == pred_y),
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    precision = tp / (tp + fp),
    fscore = 2 * tp / (2 * tp + fp + fn)
  )
}

#' @import purrr
compare <- function(dataset_results, metric) {
  dataset_results %>% map(~ . %>% map(metric) %>% unlist() %>% mean())
}

get_metrics <- function(dataset_results) {
  c("accuracy", "sensitivity", "specificity", "precision", "fscore") %>%
    map(~ compare(dataset_results, .)) %>%
    as.data.frame()
}