#!/usr/bin/env Rscript
if (sys.nframe() == 0){
  args <- commandArgs(TRUE)
  devtools::load_all(".")
  verbose <- as.logical(args[3])
  options(keras.fit_verbose = if (verbose) 2 else 0)

  dataset_f <- dataset_list()[[args[1]]]

  if (length(args) > 6) {
    extraArgs <- strsplit(args[6:length(args)], split = "=", fixed = T)
    values <- lapply(extraArgs, function(x) x[-1])
    argnames <- lapply(extraArgs, function(x) x[1])
    names(values) <- argnames
  }

  if (is.null(dataset_f)) stop("Did not find dataset!")

  retval <- experiment_validation(
    dataset_f = dataset_f,
    method = args[2],
    verbose = verbose,
    name = args[1],
    autosave = as.logical(args[4]),
    loadsave = as.logical(args[5]),
    epochs = 50
  )

  saveRDS(retval, args[6])
}
