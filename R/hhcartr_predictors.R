# source: hhcartr_predictors.R

###########################################################################################################
#' bagging_predict Make predictions for each test dataset row against each tree.
#'
#' This internal function is used to perform some basic checks on the input dataset. The first check to
#' fail will stop the model from being instantiated. The following checks are performed:
#'
#' @param mytrees A list of all trees.
#' @param testx The test dataset, the target variables are in the last column.
#' @param useIdentity Whether the training data has been transformed with the householder transform.
#' @param classify Default is TRUE. Set TRUE for a classification problem and FALSE for a regression problem.
#' @param objectid List of objectids (tree nodes) that will be collapsed in the current tree.
#'
#' @return A matrix of all predictions made from all trees and another matrix with a margin for each tree.
#'
bagging_predict <- function(mytrees, testx, useIdentity, classify, objectid){
  #
  forest_predictions <- c(rep(0, nrow(testx)))
  #
  tree_mr <- matrix(c(rep(0, length(mytrees))),
                    ncol = 1, #length(mytrees),
                    nrow = length(mytrees), #1,
                    byrow = TRUE)
  #
  all_predictions <- matrix(c(rep(0, length(mytrees) * nrow(testx))), ncol=length(mytrees), nrow=nrow(testx), byrow = TRUE)
  all_numbers <- matrix(c(rep(0, length(mytrees) * nrow(testx))), ncol=length(mytrees), nrow=nrow(testx), byrow = TRUE)
  #
  #actuals <- as.integer(unlist(testx["y"], use.names = FALSE))
  actuals <- as.integer(testx[, ncol(testx)])

  first_col <- 1
  last_col  <- (dim(testx)[2]) - 1

  for(mytree in 1:length(mytrees)){

    root_node <- mytrees[[mytree]]

    for(row in 1:nrow(testx)){
      # make the prediction for the current row and current tree
      tmp <- row_predict(root_node,
                                                  testx[row, first_col:last_col],
                                                  useIdentity,
                                                  objectid)
      all_predictions[row, mytree] <- tmp[[0]]
      all_numbers[row, mytree]  <- tmp[[1]]
      
    } # end for test data

    # calculate the margin for classification models
    if(classify){
      preds             <- all_predictions[, mytree]
      correct           <- length(which(preds == actuals))
      mr_               <- calculate_margin_for_tree(preds, actuals, correct, nrow(testx))
      tree_mr[mytree]   <- mr_
      root_node$node_mr <- mr_
    }
  }
  return(list(all_predictions, tree_mr,all_numbers))
}

###########################################################################################################
#' row_predict Make predictions for a test dataset row against a tree.
#'
#' This internal function is used to run a test dataset row through a tree to make a prediction.
#'
#' @param xnode Root node of the current tree we are making predictions against.
#' @param test_row Current row from the test dataset.
#' @param useIdentity Whether the training data has been transformed with the householder transform.
#' @param objectid A list of node numbers that will be 'pruned' ie. when making
#' predictions if the tree node matches a node in objectid the tree node will be used
#' to make the prediction rather than traversing any underlying nodes.
#'
#' @return A prediction for the test dataset row.
#'
row_predict <- function(xnode, test_row, useIdentity, objectid){
  number=0
  while(!xnode$node_children_left_NA){
    number+=1
    if(useIdentity | !xnode$node_using_householder){
      new_threshold <- test_row[,xnode$node_feature_index]
    } else {
      #new_threshold <- as.matrix(test_row) %*% (xnode$node_householder_matrix[, xnode$node_feature_index])
      new_threshold <- as.matrix(test_row) %*% (xnode$node_householder_matrix)
    }
    #if(length(new_threshold)>1){message(new_threshold)}

    # if we are performing ccp and object-ids match then found internal node that we want
    # to collapse into a leaf - so just get prediction from this.
    if(xnode$node_objectid %in% objectid[[1]]){
      # return class when we find a node to collapse.
      return(xnode$node_predicted_class)
    }
    # here not performing ccp so carry on as normal
    # node_reverse_cond added to support rpart ingestion.
    if(xnode$node_reverse_cond){
      if(new_threshold >= xnode$node_threshold){
        xnode <- xnode$node_children_left
      } else {
        if(xnode$node_children_right_NA){
          browser()
        }
        xnode <- xnode$node_children_right
      }
    } else {
      if(new_threshold <= xnode$node_threshold){
        xnode <- xnode$node_children_left
      } else {
        if(xnode$node_children_right_NA){
          browser()
        }
        xnode <- xnode$node_children_right
      }
    }
  }
  return(list(xnodec,number))
}

###########################################################################################################
#' make_predictions Make predictions on the test dataset.
#'
#' This internal function is used to make predictions on the test dataset against all induced trees.
#'
#' @param list_trees List of all induced trees.
#' @param test The test dataset.
#' @param useIdentity Whether the training data has been transformed with the householder transform.
#' @param classify Default is TRUE. Set TRUE for a classification problem and FALSE for a regression problem.
#' @param objectid A list of node numbers that will be 'pruned' ie. when making
#' predictions if the tree node matches a node in objectid the tree node will be used
#' to make the prediction rather than traversing any underlying nodes.
#'
#' @return Accuracy, margin and predictions.
#'
make_predictions <- function(list_trees, test, useIdentity, classify, objectid){
  stats              <- c(rep(0, length(list_trees)))

  # here need to convert list_trees to a Matrix(dataframe) if we are going to use C++.
  # just do this once for each tree.
  cpp_preds <- NA
  pkg.env$using_cpp <- FALSE

  # go make predictions for each row of test against each tree - for a trial
  # bagging_predict returns a 2D-matrix of shape nrow(test)*ncol(length(list_trees))
  bagging_output     <- bagging_predict(list_trees, test, useIdentity, classify, objectid)
  prediction_results <- bagging_output[[1]]
  tree_mr            <- bagging_output[[2]]
  numbers            <- bagging_output[[3]]

  # get the target variable values for later comparison
  actuals            <- as.integer(test[,ncol(test)])

  if(classify){
    for(result in 1:ncol(prediction_results)){
      preds          <- prediction_results[,result]
      correct        <- length(which(preds == actuals))
      accuracy       <- (correct / nrow(test)) * 100
      # here save prediction_results for the current tree in the current trial
      stats[result]  <- accuracy
    }

  } else {
    # here then its a regression problem so calculate R-squared, RMSE
    stats <- list() #c(rep(0, 2*length(list_trees)))
    for(result in 1:ncol(prediction_results)){
      preds          <- prediction_results[,result]
      mean_y         <- mean(actuals)
      ss_tot         <- sum((actuals - mean_y) ^ 2)
      ss_res         <- sum((actuals - preds) ^ 2)
      r_square       <- 1 - (ss_res / ss_tot)
      rmse           <- sqrt(mean((actuals - preds) ^ 2))
      stat_row       <- cbind(r_square, rmse)
      stats          <- rbind(stats, stat_row)
      numbers        <- numbers[,result]
    }
  }
  return(list(stats, tree_mr, prediction_results,numbers))
}

run_make_predictions <- function(trees, test, useIdentity, classify, objectid, prune_type = NA){
  # now go and make predictions using the sub-trees in alpha_df
  subtree_prediction_output <- make_predictions(trees, test, useIdentity, classify, objectid)
  # tree accuracy in [[1]]
  subtree_accuracy <- subtree_prediction_output[[1]]

  # tree mr in [[2]]
  subtree_tree_mr <- subtree_prediction_output[[2]]

  # predictions in [[3]]
  predictions <- subtree_prediction_output[[3]]
  #
  res <- list(subtree_accuracy, subtree_tree_mr, predictions)
  return(res)
}

###########################################################################################################
#' pruning_make_predictions Make predictions for each test dataset row against each tree.
#'
#' This internal function is a front-end to function make_predictions for making
#' predictions on CCP generated subtrees.
#'
#' @param loop_count The current fold number.
#' @param j The current tree number.
#' @param alpha_df A dataframe containing the CCP generated subtrees.
#' @param current_tree The current decision tree in the hhcartr internal format.
#' @param test The test dataset.
#' @param useIdentity Whether the training data has been transformed with the householder transform.
#' @param classify Default is TRUE. Set TRUE for a classification problem and FALSE for a regression problem.
#' @param colname The column name in alpha_df that contains the list of internal node numbers
#' that will be collapsed in the current subtree. The default value is "collapse_this_node".
#' @param pred_type Is pruning_make_predictions being called with CCP generated subtrees or
#' fatbears generated subtrees. Can have a value of either "ccp" or "fatbears". The default
#' value is "fatbears".
#'
#' @return Returns a list of (new_df, subtree_accuracy_predictions_df).
#'
pruning_make_predictions <- function(loop_count,
                                     j,
                                     alpha_df,
                                     current_tree,
                                     test,
                                     useIdentity,
                                     classify,
                                     colname = "collapse_this_node",
                                     pred_type = "fatbears"){
  if(pkg.env$show_progress){
    start_time <- Sys.time()
    msg <- "%s %s [%s] sub-trees returned. Starting prediction..."
    msgs <- sprintf(msg, start_time, toupper(pred_type), nrow(alpha_df))
    message(msgs)
  }
  # now go and make predictions on the sub-trees in alpha_df using the test set.
  subtree_accuracy <- apply(alpha_df, 1,
                            function(x) run_make_predictions(list(current_tree), test, useIdentity, classify, x[colname], pred_type))

  # get the first two columns from the list - Accuracy and margin
  subtree_accuracy_cols_1_2 <- lapply(subtree_accuracy, function (x) x[1:2])
  # get the actual predictions
  subtree_accuracy_predictions <- lapply(subtree_accuracy, function (x) x[3])
  subtree_accuracy_predictions_df <- data.frame(matrix(unlist(subtree_accuracy_predictions), nrow = nrow(alpha_df), byrow = T))

  # turn alpha_list into a data.frame
  subtree_accuracy_df <- data.frame(matrix(unlist(subtree_accuracy_cols_1_2), nrow = nrow(alpha_df), byrow = T))
  fold_num <- rep(loop_count, nrow(alpha_df))
  tree_num <- rep(j, nrow(alpha_df))
  new_df <- cbind(fold_num, tree_num, subtree_accuracy_df, alpha_df)
  if(pred_type == "ccp"){
    colnames(new_df) <- c("Fold", "Tree", "Accuracy", "Margin",
                          "Alpha", "Subtree with Smallest Alpha",
                          "Collapse this Node", "number_internal_nodes_deleted",
                          "total_number_nodes_deleted")
  } else {
    colnames(new_df) <- c("Fold", "Tree", "Accuracy", "Margin",
                          "Subtree", "Objectids", "Tree Internal Nodes",
                          "objectids", "Subtree Internal Nodes",
                          "Leaf_nodes_collapsed", "Internal_nodes_collapsed")
  }
  if(pkg.env$show_progress){
    end_time <- Sys.time()
    time_diff <- difftime(end_time, start_time, units = "secs")
    sub_trees_per_second <- nrow(alpha_df) / as.double(time_diff)
    msg <- "%s %s All [%s] predictions completed in [%s] seconds. [%s] sub-trees/sec."
    msgs <- sprintf(msg, end_time, toupper(pred_type), nrow(alpha_df), round(time_diff, digits=2), round(sub_trees_per_second, digits=2))
    message(msgs)
  }

  res <- list(new_df, subtree_accuracy_predictions_df)
  return(res)
}
