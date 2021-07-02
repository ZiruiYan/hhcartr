# source: testbed.R

######################################################################################################
#'
#' testbed - Test internal hhcartr functions.
#'
#' This function allows the testing of internal hhcartr functions in a standalone fashion.
#' Function testbed requires an export so that it can be used by the devtools::check() and devtools::test()
#' functions. The currently supported values for parameter test_func are ["best_split_",
#' "split_using_original_data", "reflect_feature_space", "hhcart_reflect_feature_space_g",
#' "hhcartr_regressor_find_better_split"].
#'
#' @param X            Training data, the feature variables.
#' @param y            Training data, the target variable.
#' @param most_freq_class The most frequent class in the target variable.
#' @param split_original boolean to indicate whether to split on original data or reflected data.
#' @param n_classes    The number of classes in y.
#' @param max_features The maximum number of features to use when training random forests.
#' @param test_func    The hhcartr function to be tested.
#' @param n_features   The number of feature columns in the training dataset.
#' @param X_matrix     The A-matrix - rows containing the most frequent class in the training dataset.
#' @return             Returns the output of the test_func to be tested (if it returns output).
#'

#' @export
testbed <- function(X, y, most_freq_class, split_original, n_classes, max_features, test_func, n_features, X_matrix){
  result <- NA
  if(test_func == "best_split_"){
    # returns: (best_idx, best_thr, best_gini)
    result <- best_split_(X, y,
                          most_freq_class,
                          split_original,
                          n_classes,
                          max_features)
  } else if(test_func == "split_using_original_data"){
    # returns: list(idx, thr, X_house, householder_matrix, using_householder)
    result <- split_using_original_data(X, y,
                                        most_freq_class,
                                        TRUE,
                                        n_classes = n_classes,
                                        max_features = max_features)
  } else if(test_func == "reflect_feature_space"){
    # returns: list(idxA, thrA, gidxA, X_houseA, newH_A)
    result <- reflect_feature_space(X_matrix, X, y,
                                         most_freq_class = most_freq_class,
                                         n_classes       = n_classes,
                                         max_features    = max_features,
                                         n_features      = n_features,
                                         colx            = 1)
  } else if(test_func == "hhcart_reflect_feature_space_g"){
    # returns: list(idx, thr, X_house, householder_matrix, using_householder)
    result <- hhcart_reflect_feature_space_g(X, y,
                                               useIdentity  = FALSE,
                                               max_features = max_features,
                                               n_features   = n_features,
                                               n_classes    = n_classes,
                                               classify     = TRUE,
                                               colx         = 1)
  } else if(test_func == "hhcartr_regressor_find_better_split"){
    # returns: list(var_idx_, split_, score_)
    result <- hhcartr_regressor_find_better_split(X, y,
                                             max_features = max_features)
  } else {
    message("Unknown function not supported yet.")
  }
  # return any results or NA.
  return(result)
}
