# source: hhcartr_export_results.R

#################################################################################################
#'
#' results - Create generic S3method to display results via results.hhcartr. Needs export entry
#' in the NAMESPACE file.
#'
#' This function creates a generic S3method results which is used to call results.hhcartr when
#' an object of type hhcartr passed to the results function, i.e. an object that is returned
#' from the fit() function. Parameters and return are the same for the results.hhcartr function.
#'
#' @param x   Unused parameter.
#' @param ... Unused parameter.
#'
#' @return    Prints relevant information about accuracy from model training. Object returned
#'            from call to results exposes the accuracy() method, which can then be used to return
#'            accuracy information for each fold/trial of the training process. Also exposed is
#'            the margin() method, this returns the margin for each tree in each fold.
#'
#' @example man/examples/results.R
#'

#' @export
results <- function(x, ...) {
  UseMethod("results")
}

#' @export
results.hhcartr <- function(x, ...){

  # get parameters used to create the model
  classify              <- pkg.env$classify
  n_folds               <- pkg.env$n_folds
  n_trees               <- pkg.env$n_trees
  n_min                 <- pkg.env$n_min
  min_node_impurity     <- pkg.env$min_node_impurity
  sampleWithReplacement <- pkg.env$sampleWithReplacement
  useIdentity           <- pkg.env$useIdentity
  testSize              <- pkg.env$testSize

  if(is.na(get_model_name())){
    message("Run the fit() function before trying to display results.")
    return()
  }

  msg             <- "%s() : Run Statistics for Dataset: %s"
  msgs            <- sprintf(msg, get_model_name(), get_data_description())
  message(msgs)
  message("using parameters:")
  msg             <- "n_folds-[%s]  n_trees-[%s]  n_min-[%s]  min_node_impurity-[%s]"
  msgs            <- sprintf(msg, n_folds, n_trees, n_min, min_node_impurity)
  message(msgs)
  msg             <- "sampleWithReplacement-[%s]  useIdentity-[%s]  testSize-[%s]"
  msgs            <- sprintf(msg, sampleWithReplacement, useIdentity, testSize)
  message(msgs)
  runStats        <- get_run_stats(classify)
  meanTotalNodes  <- mean(runStats[[1]]$Number_of_Nodes)
  meanTotalLeaves <- mean(runStats[[1]]$Number_of_Leaves)
  if(classify){
    meanAccuracy  <- mean(runStats[[1]]$Accuracy)
    msg           <- "Mean Accuracy-[%s] Mean Nodes-[%s] Mean Leaves-[%s]"
    msgs          <- sprintf(msg, round(meanAccuracy, 2), round(meanTotalNodes, 2), round(meanTotalLeaves, 2))
    message(msgs)
  } else {
    meanRsquare   <- mean(runStats[[1]]$R_squared)
    meanRMSE      <- mean(runStats[[1]]$RMSE)
    msg           <- "Mean R-Squared-[%s] Mean RMSE-[%s] Mean Nodes-[%s] Mean Leaves-[%s]"
    msgs          <- sprintf(msg, round(meanRsquare, 2), round(meanRMSE, 2), round(meanTotalNodes, 2), round(meanTotalLeaves, 2))
    message(msgs)
  }

  parms <- list(
    get_number = function(){
      # returns numbers
      return(numbers)
    },
    
    accuracy = function(){
      # returns Accuracy, Number_of_Nodes, Number_of_Leaves for each fold/trial
      return(runStats)
    },

    margin = function(){
      # returns tree margin on each fold
      df <- data.frame()
      for (i in seq_along(pkg.env$tree_margin)){
        nRow <- data.frame(Fold = i, Margin = round(pkg.env$tree_margin[[i]], 2))
        df <- rbind(df, nRow)
      }
      return(df)
    },

    mni_data = function(){
      if(length(pkg.env$min_node_impurity_data) == 0){
        message("Instantiate model with min_node_impurity = 'auto' before trying to display results.")
        return()
      } else {
        # returns all data for min_node_impurity search
        return(pkg.env$min_node_impurity_data)
      }
    },

    coefficients = function(n_node = NA, fold = NA){
      if(!useIdentity){
        res <- getCoefficients(n_node, fold)
        return(res)
      } else {
        message("Coefficients only valid when useIdentity = FALSE.")
        return()
      }
    },

    ccp_subtree_data = function(){
      if(length(pkg.env$ccp_subtree_data) == 0){
        message("Instantiate model with pruning = TRUE before trying to get pruned sub-tree data.")
        return()
      } else {
        # returns all data from ccp pruning step.
        return(pkg.env$ccp_subtree_data)
      }
    },

    ccp_phase_data = function(){
      if(length(pkg.env$ccp_phase_data) == 0){
        message("Instantiate model with pruning = TRUE before trying to get pruned sub-tree data.")
        return()
      } else {
        # returns all data from ccp pruning step.
        return(pkg.env$ccp_phase_data)
      }
    },

    ccp_predictions = function(){
      if(length(pkg.env$ccp_predictions) == 0){
        message("Instantiate model with pruning = TRUE before trying to get pruned sub-tree predictions.")
        return()
      } else {
        # returns all data from ccp predictions.
        return(pkg.env$ccp_predictions)
      }
    }

  )
  class(parms) <- append(class(parms), "results")
  return(parms)
}
