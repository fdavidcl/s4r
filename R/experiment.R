#' @importFrom ruta input dense output train autoencoder encode
#' @importFrom purrr partial compose %>%
train_reduction <- function(train_x, train_y, method, normalized = TRUE, ...) {
  max_for_10ppd <- ceiling(0.1 * nrow(train_x))
  ten_percent <- ceiling(0.1 * ncol(train_x))
  # At least 2 generated features
  hidden_dim <- max(min(max_for_10ppd, ten_percent), 2)
  hidden_dim <- max(ceiling(sqrt(ncol(train_x))), 2)

  activation <- if (normalized) "sigmoid" else "linear"
  network <- input() + dense(hidden_dim, "relu") + output(activation)

  # Do not use binary crossentropy (and sigmoid activation) *unless* the data has been
  # accordingly normalized (to the [0, 1] interval)
  loss <- if (normalized) "binary_crossentropy" else "mean_squared_error"

  # reduction_f <- function(x) x
  reduction_f <-
    switch(method,
           baseline = {
             function(x)
               x
           },
           pca = {
             pca <- train_x %>% prcomp(scale = FALSE, center = FALSE)
             function(x) predict(pca, x)[, 1:hidden_dim]
           },
           autoencoder = {
             feature_extractor <- autoencoder(network, loss = loss, ...) %>%
               train(train_x, epochs = 200)
             partial(encode, learner = feature_extractor, .lazy = FALSE)
           },
           scorer = {
             feature_extractor <- Scorer$new(network, loss = loss, weight = 0.01)
             feature_extractor$train(train_x, classes = as.numeric(train_y) - 1, epochs = 200)
             feature_extractor$encode
           },
           slicer = {
             feature_extractor <- Slicer$new(network, loss = loss, weight = 1)
             feature_extractor$train(train_x, classes = as.numeric(train_y) - 1, epochs = 200)
             feature_extractor$encode
           },
           combined = {
             feature_extractor <- Combined$new(network, loss = loss, slicer_weight = 1, scorer_weight = 0.01)
             feature_extractor$train(train_x, classes = as.numeric(train_y) - 1, epochs = 200)
             feature_extractor$encode
           })

  compose(name, expand_dims, reduction_f)
}

#' @importFrom caret trainControl train
#' @importFrom purrr partial
train_classifier <- function(train_x, train_y, classifier) {
  ctrl <- trainControl(method = "none")
  classifier <- caret::train(train_x, train_y, method = classifier, trControl = ctrl)
  function(dat) predict(classifier, dat)
}

#' @importFrom caret createFolds
#' @export
experiment_validation <- function(dataset, method, name, classifiers = c("knn", "svmRadial", "mlp"), seed = 4242, folds = 5, verbose = TRUE, autosave = TRUE, loadsave = TRUE, ...) {
  log <- function(...) {
    if (verbose) cat(...)
  }

  set.seed(seed)

  log("Testing method", method, "now\n")

  folds_idx <- createFolds(dataset$y, k = folds)

  results <- list()
  results$features <- list()
  cls <- list()

  for (i in 1:folds) {
    train_x <- dataset$x[-folds_idx[[i]],]
    train_y <- dataset$y[-folds_idx[[i]]]
    test_x <- dataset$x[folds_idx[[i]],]
    test_y <- dataset$y[folds_idx[[i]]]

    log("Entered fold", i, paste0("(", sum(test_y == 1), "/", length(test_y), " positives)"), ">> ")

    dirname <- file.path("checkpoints", method)
    filename <- file.path(dirname, paste0(name, "_fold_", i, ".rds"))
    evalname <- file.path(dirname, paste0(name, "_eval_", i, ".rds"))
    evaluate <- T

    if (loadsave && dir.exists(dirname) && file.exists(filename)) {
      if (file.exists(evalname)) {
        current <- readRDS(evalname)
        results$features[[i]] <- current$features
        cls[[i]] <- current$classifiers
        evaluate <- F
      } else {
        reduced <- readRDS(filename)
      }
    } else {
      if (dataset$normalize) {
        if (verbose) cat("normalizing >> ")
        mx <- apply(train_x, 2, max)
        mn <- apply(train_x, 2, min)
        # Avoid division by zero
        range_n <- max(mx - mn, keras::k_epsilon())

        train_x <- t(apply(train_x, 1, function(x) (x - mn) / range_n))
        test_x <- t(apply(test_x, 1, function(x) (x - mn) / range_n))
      }

      reduction <- train_reduction(train_x, train_y, method, dataset$normalize, ...)

      reduced <- list(
        train = reduction(train_x),
        test = reduction(test_x)
      )

      if (autosave) {
        dir.create(dirname, showWarnings = FALSE, recursive = TRUE)
        saveRDS(reduced, file = filename)
      }
    }

    if (evaluate) {
      log("evaluating: ")
      results$features[[i]] <- evaluate_features(reduced$test, test_y)
      cls[[i]] <- map(classifiers, function(cl) {
        log(cl, "(training)")
        model <- safely(train_classifier)(reduced$train, train_y, cl)
        if (is.null(model$result)) {
          return(NULL)
        } else {
          log(" (predicting) ")
          predictions <- safely(model$result)(reduced$test)
          if (!is.null(predictions$result)) {
            res <- safely(evaluate_model)(test_y, predictions$result)
            return(res$result)
          } else {
            return(NULL)
          }
        }
      })
      names(cls[[i]]) <- classifiers

      if (autosave) {
        saveRDS(list(features = results$features[[i]], classifiers = cls[[i]]), file = evalname)
      }
    }
    log("\n")
  }

  results$classifiers <- map(classifiers, function(cl) {
    cls %>% map(cl)
  })
  names(results$classifiers) <- classifiers

  if (verbose) cat("Test ok.\n")

  structure(results, class = results_dataset)
}

#' @export
experiment_all <-
  function(datasets = dataset_list(),
           folder = paste0("results_", format(Sys.time(), "%y-%m-%d_%H:%M")),
           methods = c("baseline",
                       "pca",
                       "autoencoder",
                       "scorer",
                       "slicer",
                       "combined"),
           verbose = T,
           autosave = T,
           loadsave = T,
           ...) {
  log <- function(...) {
    if (verbose) cat(...)
  }

  options(keras.fit_verbose = 0)

  dir.create(folder, recursive = TRUE)

  results <- map2(datasets, names(datasets), function(dataset_f, name) {
    save_name <- file.path(folder, paste0(name, ".rds"))
    if (loadsave && file.exists(save_name)) {
      log("Skipping", name, "\n")
      readRDS(save_name)
    } else {
      keras::backend()$clear_session()
      gc()
      log("Now reading", name, ">> ")
      dataset <- dataset_f()
      log("OK. Testing methods...\n")

      this_dataset <- map(methods, function(m)
        experiment_validation(dataset = dataset, method = m, verbose = verbose, name = name, autosave = autosave, loadsave = loadsave, ...))
      names(this_dataset) <- methods

      log("All tests ok. Saving...\n")
      if (autosave) {
        saveRDS(this_dataset, file = save_name)
      }

      this_dataset
    }
  })

  results <- structure(results, class = results_experiment)

  saveRDS(results, file = file.path(folder, "results.rds"))
  invisible(results)
}

