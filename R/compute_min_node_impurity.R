# source: compute_min_node_impurity.R

#######################################################################################################
#' invoke_model.
#'
#' This function instantiates a HHDecisionTree model, it is used to induce classification
#' or regression trees depending upon the value of the response parameter. It supports
#' the parameters listed below. It is used to find the optimum value for min_node_impurity.
#'
#' @param X The feature variables we will use to train the HHDecisionTree model on.
#' @param y The target variable.
#' @param try_this_min_imp The minimum node impurity we will try on this invocation of
#' the HHDecisionTree model.
#' @param iter The number of times function invoke_model has been invoked, used to
#' increment a number that is passed to the function set.seed().
#' @param response The response parameter is used to specify what type of model to build, either 'classify'
#' for a classification tree model or 'regressor' for a regression tree model. The default is 'classify'.
#' @param mni_n_folds The number of folds to pass to mni.control to be used to instantiate
#' a HHDecisionTree model when searching for an optimum value of min_node_impurity.
#' @param mni_n_trees The number of trees to pass to mni.control to be used to instantiate
#' a HHDecisionTree model when searching for an optimum value of min_node_impurity.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#' original training data to find the optimal splits rather than using the reflected data. The
#' default value is FALSE.
#' @param seed The seed parameter is used to provide a seed value for the R random number
#' generator to ensure repeatable experiments.
#'
#' @return Returns statistics on the current run of the HHDecisionTree model.
#'
invoke_model <- function(X, y,
                         try_this_min_imp,
                         iter,
                         response,
                         mni_n_folds,
                         mni_n_trees,
                         useIdentity,
                         seed){
  # invoke HHDecisionTree with the current value of min node impurity.
  dtc    <- HHDecisionTree(response          = response,
                           n_folds           = mni_n_folds,
                           n_trees           = mni_n_trees,
                           min_node_impurity = try_this_min_imp,
                           useIdentity       = useIdentity,
                           pruning           = FALSE,
                           control           = mni.control(mni_n_folds = mni_n_folds,
                                                           mni_n_trees = mni_n_trees),
                           show_progress     = FALSE,
                           seed              = seed)
  vv     <- dtc$fit(X,y)
  rStats <- get_run_stats(TRUE)
  # remove temporary objects
  rm(dtc)
  rm(vv)
  # returns one row per fold for current min_node_impurity value
  return(rStats)
}

run_hhdecisiontree_model <- function(X, y,
                                     iter,
                                     starting_val,
                                     number_vals_2_try,
                                     step_val,
                                     response,
                                     mni_n_folds,
                                     mni_n_trees,
                                     useIdentity,
                                     seed){

  # preds_vec: vector to hold predictions
  preds_vec          <- c(rep(0, number_vals_2_try * mni_n_folds))
  # min_imp: vector to hold min_node_impurity values tried
  min_imp            <- c(rep(0, number_vals_2_try * mni_n_folds))
  # leaves_vec: vector to hold number of leaves
  leaves_vec         <- c(rep(0, number_vals_2_try * mni_n_folds))
  # folds_vec: one row for each fold
  folds_vec          <- c(rep(0, number_vals_2_try * mni_n_folds))

  cnt <- 1
  try_this_min_imp    <- starting_val / 100
  for(x in seq(0, number_vals_2_try, 1)) {
    # run the model either to classify or regress.
    data_fromclass    <- invoke_model(X, y,
                                      try_this_min_imp,
                                      iter,
                                      response,
                                      mni_n_folds,
                                      mni_n_trees,
                                      useIdentity,
                                      seed)
    data_fromclass    <- data_fromclass[[1]]
    for (row in 1:nrow(data_fromclass)) {
      folds_vec[cnt]  <- row
      preds_vec[cnt]  <- data_fromclass[row, "Accuracy"]
      leaves_vec[cnt] <- data_fromclass[row, "Number_of_Leaves"]
      min_imp[cnt]    <- try_this_min_imp
      cnt             <- cnt + 1
    }
    try_this_min_imp <- try_this_min_imp + step_val
  }
  outp <- list(folds_vec, preds_vec, min_imp, leaves_vec)
  return(outp)
}

#######################################################################################################
#' compute_min_node_impurity.
#'
#' This function instantiates a HHDecisionTree model, it is used to induce classification or regression
#' trees depending upon the value of the response parameter. It supports the parameters listed below.
#'
#' @param X The feature variables we will use to train the HHDecisionTree model on.
#' @param y The target variable.
#' @param response The response parameter is used to specify what type of model to build, either 'classify'
#' for a classification tree model or 'regressor' for a regression tree model. The default is 'classify'.
#' @param seed The random seed to be used to prime the random number generator.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#' original training data to find the optimal splits rather than using the reflected data. The
#' default value is FALSE.
#'
#' @return Returns the "best" value of min_node_impurity.
#'
compute_min_node_impurity <- function(X, y, response, seed, useIdentity = FALSE){

  # loop_count: number of times to repeat
  loop_count        <- pkg.env$mni_trials
  # starting_val: first min_node_impurity value to try/100
  starting_val      <- pkg.env$mni_start * 100
  # number_vals_2_try: number of min_node_impurity values to try
  number_vals_2_try <- pkg.env$mni_numvals
  # step_val: decrement min_node_impurity by this much each time
  step_val          <- pkg.env$mni_size
  # mni_n_folds: number of folds
  mni_n_folds       <- pkg.env$mni_n_folds
  # mni_n_trees: number of trees
  mni_n_trees       <- pkg.env$mni_n_trees

  fold_list         <- list()
  seed_list         <- list()
  acc_list          <- list()
  min_imp_list      <- list()
  leaves_list       <- list()

  if(pkg.env$show_progress){
    msg <- "Finding best min_node_impurity starting at %#.2f, by steps of %s, for %s values."
    smsg <- sprintf(msg, pkg.env$mni_start, step_val, number_vals_2_try)
    message(smsg)

    msg <- "Using mni.control parameters n_folds-[%s], n_trees-[%s] for response-[%s]."
    smsg <- sprintf(msg, mni_n_folds, mni_n_trees, response)
    message(smsg)
  }

  for(i in 1:loop_count){
    results      <- run_hhdecisiontree_model(X, y,
                                             i,
                                             starting_val,
                                             number_vals_2_try,
                                             step_val,
                                             response,
                                             mni_n_folds,
                                             mni_n_trees,
                                             useIdentity,
                                             seed)
    fold_list    <- append(fold_list, results[[1]])
    acc_list     <- append(acc_list, results[[2]])
    min_imp_list <- append(min_imp_list, results[[3]])
    leaves_list  <- append(leaves_list, results[[4]])
  }

  # check if I can use this for the agg input.
  dfx     <- do.call(rbind, Map(data.frame,
                                fold              = fold_list,
                                seed              = seed,
                                min_node_impurity = min_imp_list,
                                accuracy          = acc_list,
                                number_of_leaves  = leaves_list))
  pkg.env$min_node_impurity_data <- dfx
  max_acc_subset <- dfx[dfx$accuracy == max(dfx$accuracy),]
  max_acc <- unique(max(max_acc_subset$accuracy))
  max_imp <- max(max_acc_subset$min_node_impurity)

  if(pkg.env$show_progress){
    msg  <- "Maximum accuracy %s found at the following min_node_impurity value %s. Seed %s."
    smsg <- sprintf(msg, round(max_acc, 2), round(max_imp, 2), seed)
    message(smsg)
  }

  # if more than one value of min_node_impurity take the largest value.
  if(length(max_imp) > 1){
    max_imp <- max(max_imp)
  }
  if(pkg.env$show_progress){
    msg  <- "Returning min_node_impurity value %s."
    smsg <- sprintf(msg, round(max_imp, 2))
    message(smsg)
    message("")
  }

  res <- list(round(max_imp, 2), seed)
  return(res)
} # end compute_min_node_impurity()
