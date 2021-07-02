# source: hhcartr_calculate_margin.R

###########################################################################################################
#' calculate_margin_for_tree() - Calculate the margin for a tree.
#'
#' This internal function is called from bagging_predict to calculate the margin on the
#' current tree using the test set.
#'
#' @param preds Predictions on the test set.
#' @param actuals The actuals classes ie. the target variables.
#' @param count_classified_correct The number of correctly classified predictions.
#' @param total The number of rows in the test set.
#' @return returns the margin for the current tree.
#'
calculate_margin_for_tree <- function(preds, actuals, count_classified_correct, total){
  # initialise some variables
  mr              <- 0.0
  nomisclassified <- FALSE

  misclassified_indices <- which(preds != actuals)
  if(length(misclassified_indices) > 0){
    # get list of all misclassified classes
    classes_misclassified                   <- actuals[misclassified_indices]
    count_most_frequent_class_misclassified <- max(table(classes_misclassified))
  } else {
    nomisclassified                         <- TRUE
  }
  if(nomisclassified){
    mr <- count_classified_correct / total
  } else {
    mr <- (count_classified_correct / total) - (count_most_frequent_class_misclassified / total)
  }
  return(round(mr, 2))
} # End calculate_margin_for_tree()
