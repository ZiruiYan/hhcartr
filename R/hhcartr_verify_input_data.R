# source: hhcartr_verify_input_data.R

###########################################################################################################
#' hhcart_verify_input_data verify the input data.
#'
#' This internal function is used to perform some basic checks on the input dataset. The first check to
#' fail will stop the model from being instantiated. The following checks are performed:
#'  - there must be no NA's in the training dataset.
#'  - all columns in the training datasets must contain numeric data only.
#'  - max_features must not be greater than n_features.
#'  - if its a classification problem the target variable must be categorical.
#'  - if its a regression problem the target variable must not be categorical.
#'
#' @param X The training dataset.
#' @param y target variable column.
#' @param classify Default is TRUE. Set TRUE for a classification problem and FALSE for a regression problem.
#' @param max_features_ Default is 0. The maximum number of features to search for an optimal split.
#' @param n_features_ Default is 0. The total number of feature columns in the training dataset.
#' @param rforest Default is FALSE. Indicates whether this function is being called from
#' HHDecisionTree or HHRandomForest.
#' @return Nothing if all checks are passed, otherwise the function stops.
#'

hhcart_verify_input_data = function(X, y,
                                    classify = TRUE,
                                    max_features_ = 0,
                                    n_features_ = 0,
                                    rforest = FALSE){

  # make sure all X columns/rows contain no NA's
  if(any(is.na(X))){
    stop("hhcartr(hhcart_verify_input_data) does not yet support NA's in the data. Terminating.")
  }
  # make sure all feature columns are numeric
  if(!all(sapply(X, is.numeric))){
    # problem - not all columns are numeric
    message("sapply(X, typeof):")
    message(sapply(X, typeof))
    message("lapply(X, class):")
    message(lapply(X, class))
    stop("hhcartr(hhcart_verify_input_data) Not all columns are numeric. Terminating.")
  }
  # make sure y is a factor
  if(!is.factor(y) & classify){
    stop("hhcartr(hhcart_verify_input_data) The y variable must be a factor for a classification problem. Terminating.")
  }
  if(is.factor(y) & !classify){
    stop("hhcartr(hhcart_verify_input_data) The y variable must not be a factor for a regression problem. Terminating.")
  }
  if(max_features_ > n_features_){
    stop("hhcartr(hhcart_verify_input_data) max_features_ greater than n_features_. Terminating.")
  }
  if(rforest == FALSE){
    if(pkg.env$n_folds > nrow(X)){
      stop("hhcartr(hhcart_verify_input_data) n_folds exceeds number of rows in training data. Terminating.")
    }
  } else {
    if(pkg.env$n_trials > nrow(X)){
      stop("hhcartr(hhcart_verify_input_data) n_trials exceeds number of rows in training data. Terminating.")
    }
  }
  if(pkg.env$mni_n_folds > nrow(X)){
    stop("hhcartr(hhcart_verify_input_data) mni_n_folds exceeds number of rows in training data. Terminating.")
  }
}
