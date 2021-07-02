# source: hhcartr_run_classifier.R

#################################################################################################
#'
#' hhcart_run_classifier - This function calls grow_tree_() to induce a decision tree.
#'
#' @param train_set The training data.
#' @param sample_size The sample size parameter is used to determine how much of the training
#' dataset is actually used during training. A value of 1.0 allows all of the current training
#' dataset to be used for training. A value of less than one will mean that proportion of the
#' training dataset will be selected at random and then used for training. The value of parameter
#' sampleWithReplacement will determine if the random sampling of the training dataset is
#' performed using replacement or not. The default value is 1.0.
#' @param j Not used, kept for compatibility.
#' @param n_min The n_min parameter is used to stop splitting a node when a
#' minimum number of samples at that node has been reached. The default value is 2.
#' @param min_node_impurity The min node impurity parameter is used to stop splitting a node
#' if the node impurity at that node is less than this value. The node impurity is calculated
#' using the hyperplane Gini index. The default value is 0.2.
#' @param sampleWithReplacement The sampleWithReplacement parameter is used in conjunction with
#' the sample size parameter. The sampleWithReplacement parameter will determine if sampling from
#' the training dataset is done with or without replacement. The default value is FALSE.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#' original training data to find the optimal splits rather than using the reflected data. The
#' default value is FALSE.
#' @param classify The classify parameter when set TRUE indicates that the data is for building
#' a classification model. A value of FALSE and a regression model will be induced.
#' @param n_features The number of features in the training data.
#' @param n_classes The number of classes in the training data.
#' @param max_features The max features parameter determines the number of features to consider
#' when looking for the best split, and can take one of the values listed below. The default value
#' is “sqrt”.
#' @return Returns an induced decision tree.
#'

hhcart_run_classifier <- function(train_set,
                                  sample_size,
                                  j,
                                  n_min,
                                  min_node_impurity,
                                  sampleWithReplacement,
                                  useIdentity,
                                  classify,
                                  n_features,
                                  n_classes,
                                  max_features = NA){

  # if max_features is not specified than we are dealing with a fold from HHDecisionTree
  # otherwise its a trial from HHRandomForest.
  fold_or_trial <- "trial"
  if(is.na(max_features)){ fold_or_trial <- "fold" }

  proportionOfTrainSetToUse <- as.integer(nrow(train_set) * sample_size)
  #if(proportionOfTrainSetToUse != nrow(train_set)){
  #  msg <- "hhcart_run_classifier() Using a sub-sample of %s rows from the current %s, initial size was %s."
  #  msgs <- sprintf(msg, proportionOfTrainSetToUse, fold_or_trial, nrow(train_set))
  #  message(msgs)
  #}

  # compute indices we will actually use in the sampled training set
  train_set_indices_after_subsampling <- sample(nrow(train_set),
                                                proportionOfTrainSetToUse,
                                                replace = sampleWithReplacement)
  # find the actual indices we will use in train set
  tmp <- train_set[train_set_indices_after_subsampling,]

  all_data_indices <- rep(1:nrow(train_set))
  OOB_rows <- all_data_indices[-train_set_indices_after_subsampling]

  X_train <- tmp[,1:ncol(train_set) - 1]
  y_train <- tmp[,ncol(train_set)]

  # save total number of training samples used for this tree
  pkg.env$num_training_samples <- nrow(X_train)

  root <- NNode(gini                  = NA,
                num_samples           = NA,
                num_samples_per_class = NA,
                predicted_class       = NA,
                parent_node           = NA,
                objectid              = 0,
                oob_row_indices       = OOB_rows)
  root["node_children_left"]     <- NA
  root["node_children_left_NA"]  <- TRUE
  root["node_children_right"]    <- NA
  root["node_children_right_NA"] <- TRUE
  root["node_type"]              <- "ROOT"

  treeobj <- grow_tree_(root,
                        X_train,
                        y_train,
                        n_min             = n_min,
                        min_node_impurity = min_node_impurity,
                        useIdentity       = useIdentity,
                        classify          = classify,
                        n_features        = n_features,
                        n_classes         = n_classes,
                        max_features      = max_features)
  return(treeobj)
}
