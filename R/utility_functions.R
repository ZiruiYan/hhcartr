# source: utility_functions.R

# hhcartr internal utility functions to manipulate package environment variables.


# increment objectid_count - number of NNode objects in a tree.
increment_objectid_count <- function(){
  pkg.env$objectid_count <- pkg.env$objectid_count + 1
}

# get objectid_count - number of NNode objects in a tree.
get_objectid_count <- function(){
  return(as.integer(pkg.env$objectid_count))
}

# initialise objectid_count - number of NNode objects in a tree.
zero_objectid_count <- function(){
  pkg.env$objectid_count <- 0
}

# increment node_count
increment_node_count <- function(){
  pkg.env$node_count <- pkg.env$node_count + 1
}

# return node_count
get_node_count <- function(){
  return(pkg.env$node_count)
}

# initialise node_count
zero_node_count <- function(){
  pkg.env$node_count <- 0
}

# leaf_count
increment_leaf_count <- function(){
  pkg.env$leaf_count <- pkg.env$leaf_count + 1
}

# get leaf_count
get_leaf_count <- function(){
  return(pkg.env$leaf_count)
}

# initialise leaf_count
zero_leaf_count <- function(){
  pkg.env$leaf_count <- 0
}

# add dot statement to dot_list()
append_dot_list <- function(dottext){
  pkg.env$dot_list <- append(pkg.env$dot_list, dottext)
}

# return the dot_list - it contains dot statements for the currently selected tree.
get_dot_list <- function(){
  return(pkg.env$dot_list)
}

# initialise dot_list
clear_dot_list <- function(){
  pkg.env$dot_list <- list()
}

# save_oobee_accuracy()
save_oobee_accuracy <- function(oobee_acc){
  pkg.env$oobee_accuracy <- append(pkg.env$oobee_accuracy, oobee_acc)
}

# clear_oobee_accuracy()
clear_oobee_accuracy <- function(){
  pkg.env$oobee_accuracy <- list()
}

# run_stats()
save_run_stats <- function(runstats){
  pkg.env$run_stats <- rbind(pkg.env$run_stats, runstats)
}

# get the list of run statistics for the current model.
get_run_stats <- function(classify = TRUE, show_oobee = FALSE){
  # if nothing in the list then fit() has not been run so nothing to return.
  if(!length(pkg.env$run_stats)){
    message("Nothing to return, run fit() first.")
    return()
  }

  if(length(pkg.env$oobee_accuracy)){
    show_oobee <- TRUE
  }

  if(classify){
    # classification results
    if(!show_oobee){
      df = data.frame(accuracy         <- round(unlist(pkg.env$run_stats), 2),
                      Number_of_Nodes  <- unlist(pkg.env$node_count_node_vector),
                      Number_of_Leaves <- unlist(pkg.env$node_count_leaf_vector))
      colnames(df) <- c("Accuracy", "Number_of_Nodes", "Number_of_Leaves")
    } else {
      df <- data.frame(accuracy         <- unlist(pkg.env$run_stats),
                       oobee_accuracy   <- unlist(pkg.env$oobee_accuracy),
                       Number_of_Nodes  <- unlist(pkg.env$node_count_node_vector),
                       Number_of_Leaves <- unlist(pkg.env$node_count_leaf_vector))
      colnames(df) <- c("Accuracy", "OOB_EE_Accuracy", "Number_of_Nodes", "Number_of_Leaves")
    }
  } else {
    # regression results
    tmp <- matrix(unlist(pkg.env$run_stats), ncol=2, byrow=FALSE)
    if(!show_oobee){
      df = data.frame(r_squared        <- unlist(tmp[,1]),
                      rmse             <- unlist(tmp[,2]),
                      Number_of_Nodes  <- unlist(pkg.env$node_count_node_vector),
                      Number_of_Leaves <- unlist(pkg.env$node_count_leaf_vector))
      colnames(df) <- c("R_squared", "RMSE", "Number_of_Nodes", "Number_of_Leaves")
    } else {
      df = data.frame(r_squared        <- unlist(tmp[,1]),
                      rmse             <- unlist(tmp[,2]),
                      oobee_accuracy   <- unlist(pkg.env$oobee_accuracy),
                      Number_of_Nodes  <- unlist(pkg.env$node_count_node_vector),
                      Number_of_Leaves <- unlist(pkg.env$node_count_leaf_vector))
      colnames(df) <- c("R_squared", "RMSE", "OOB_EE_Accuracy", "Number_of_Nodes", "Number_of_Leaves")
    }
  }
  res <- list(df)
  return(res)
}

# get the list of run statistics for the current model.
get_run_stats_train <- function(classify = TRUE, show_oobee = FALSE){
  # if nothing in the list then fit() has not been run so nothing to return.
  if(length(pkg.env$oobee_accuracy)){
    show_oobee <- TRUE
  }

  # classification results
  df = data.frame(accuracy    <- round(unlist(pkg.env$run_stats_train), 2))
  colnames(df) <- c("Accuracy")
  
  res <- list(df)
  return(res)
}

# initialise the list of run statistics.
clear_run_stats <- function(){
  pkg.env$run_stats <- list()
}

# is this used?
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
  return(ux)
}

#
save_count_tree_node_vector <- function(x){
  pkg.env$node_count_node_vector <- x
}

#
save_count_tree_leaf_vector <- function(x){
  pkg.env$node_count_leaf_vector <- x
}

# get the data_description for the current model.
get_data_description <- function(){
  return(pkg.env$model_data_description)
}

# save feature importances for the current tree.
save_feature_importances <- function(fi){
  pkg.env$model_feature_importances <- rbind(pkg.env$model_feature_importances, fi)
}

# initialise list of feature importances.
clear_feature_importances <- function(){
  pkg.env$model_feature_importances <- list()
}

# Tree Accuracy??
save_trees <- function(foldresults){
  pkg.env$folds_trees <- append(pkg.env$folds_trees, foldresults)
}

# initialise
clear_trees <- function(){
  pkg.env$folds_trees <- list()
}

# save tree strength for the current tree
save_strength <- function(strength_){
  pkg.env$tree_strength <- rbind(pkg.env$tree_strength, strength_)
}

# initialise the tree strength list
clear_strength <- function(){
  pkg.env$tree_strength <- list()
}

# return the tree strength list
get_strength <- function(){
  if(length(pkg.env$tree_strength)){
    return(pkg.env$tree_strength)
  } else {
    message("Nothing to return, run fit() first.")
    return()
  }
}

# save the tree margin for the current tree
save_margin <- function(margin_){
  pkg.env$tree_margin <- rbind(pkg.env$tree_margin, margin_)
}

#initialise the tree numbers
clear_numbers <- function(){
  pkg.env$numbers  <- list() 
}

# initialise the tree margin list
clear_margin <- function(){
  pkg.env$tree_margin <- list()
}

# return the tree margin list
get_margin <- function(){
  return(pkg.env$tree_margin)
}

# Trial Strength
save_trial_strength <- function(strength_){
  pkg.env$trial_strength <- rbind(pkg.env$trial_strength, strength_)
}

clear_trial_strength <- function(){
  pkg.env$trial_strength <- list()
}

get_trial_strength <- function(){
  if(length(pkg.env$tree_strength)){
    # trial_strength holds both the strength and correlation for each trial
    colnames(pkg.env$trial_strength) <- c("Strength", "Correlation")
    rown <- list()
    for(i in 1:nrow(pkg.env$trial_strength)){
      rown <- append(rown, paste0("Trial_", i))
    }
    rownames(pkg.env$trial_strength) <- rown
    return(pkg.env$trial_strength)
  } else {
    message("Nothing to return, run fit() first.")
    return()
  }
}

#
clear_model_fit_results <- function(){
  pkg.env$model_fit_results <- list()
}

get_model_fit_results <- function(){
  return(pkg.env$model_fit_results)
}

# get the maximum number of features to search for splits.
get_max_features <- function(){
  return(pkg.env$max_features)
}

# set the max_features environment variable.
set_max_features <- function(max_features){
  pkg.env$max_features <- max_features
}

# get the total number of features in the current training dataset.
get_total_features <- function(){
  return(pkg.env$total_features)
}

# set the total_features environment variable.
set_total_features <- function(total_features){
  pkg.env$total_features <- total_features
}

# set the model_name environment variable.
get_model_name <- function(){
  return(pkg.env$model_name)
}

# set the model_name environment variable.
set_model_name <- function(model_name){
  pkg.env$model_name <- model_name
}

# manage variable - pkg.env$cpp_subtree_data
save_cpp_subtree_data <- function(cpp_data){
  pkg.env$cpp_subtree_data <- cpp_data
}

get_cpp_subtree_data <- function(){
  return(pkg.env$cpp_subtree_data)
}

clear_cpp_subtree_data <- function(){
  pkg.env$cpp_subtree_data <- list()
}

# manage variable - pkg.env$cpp_subtree_predictions
save_cpp_subtree_predictions <- function(cpp_preds){
  pkg.env$cpp_subtree_predictions <- cpp_preds
}

get_cpp_subtree_predictions <- function(){
  return(pkg.env$cpp_subtree_predictions)
}

clear_cpp_subtree_predictions <- function(){
  pkg.env$cpp_subtree_predictions <- list()
}

# ---------------------------------------------------------------------------------
# manage variable - pkg.env$r_subtree_data
save_r_subtree_data <- function(r_data){
  pkg.env$r_subtree_data <- rbind(pkg.env$r_subtree_data, r_data)
}

get_r_subtree_data <- function(){
  return(pkg.env$r_subtree_data)
}

clear_r_subtree_data <- function(){
  pkg.env$r_subtree_data <- list()
}

# manage variable - pkg.env$r_subtree_predictions
save_r_subtree_predictions <- function(r_preds){
  pkg.env$r_subtree_predictions <- rbind(pkg.env$r_subtree_predictions, r_preds)
}

get_r_subtree_predictions <- function(){
  return(pkg.env$r_subtree_predictions)
}

clear_r_subtree_predictions <- function(){
  pkg.env$r_subtree_predictions <- list()
}

# ---------------------------------------------------------------------------------

# save the latest ccp subtree df
save_ccp_subtree_data <- function(ccp_subtree_data){
  pkg.env$ccp_subtree_data <- rbind(pkg.env$ccp_subtree_data, ccp_subtree_data)
}

# get the ccp subtree df
get_ccp_subtree_data <- function(){
  return(pkg.env$ccp_subtree_data)
}

# clear the ccp subtree df
clear_ccp_subtree_data <- function(){
  pkg.env$ccp_subtree_data <- list()
}

# ---------------------------------------------------------------------------------

# save the latest ccp phase df
save_ccp_phase_data <- function(ccp_phase_data){
  #pkg.env$ccp_phase_data <- rbind(pkg.env$ccp_phase_data, ccp_phase_data)
  pkg.env$ccp_phase_data <- append(pkg.env$ccp_phase_data, list(ccp_phase_data))
}

# get the ccp phase df
get_ccp_phase_data <- function(){
  return(pkg.env$ccp_phase_data)
}

# clear the ccp phase df
clear_ccp_phase_data <- function(){
  pkg.env$ccp_phase_data <- list()
}

# -------------------------------------------------------------------------------

# save ccp raw predictions
save_ccp_predictions <- function(predictions){
  # save predictions for each fold per tree.
  pkg.env$ccp_predictions <- append(pkg.env$ccp_predictions, list(t(predictions)))
}

# get the ccp pruned tree df
get_ccp_predictions <- function(){
  return(pkg.env$ccp_predictions)
}

# clear the ccp pruned tree df
clear_ccp_predictions <- function(){
  pkg.env$ccp_predictions <- list()
}

# save fatbears raw predictions
save_fatbears_predictions <- function(predictions){
  pkg.env$fatbears_predictions <- cbind(pkg.env$fatbears_predictions, predictions)
}

# get the fatbears pruned tree df
get_fatbears_predictions <- function(){
  return(pkg.env$fatbears_predictions)
}

# clear the fatbears pruned tree df
clear_fatbears_predictions <- function(){
  pkg.env$fatbears_predictions <- list()
}

# save partition data
save_partition_data <- function(data_){
  pkg.env$partition_data <- data_
}

# get the fatbears pruned tree df
get_partition_data <- function(){
  return(pkg.env$partition_data)
}

# clear the fatbears pruned tree df
clear_partition_data <- function(){
  pkg.env$partition_data <- list()
}

#
save_fatbears_ensemble_accuracy <- function(accuracy){
  pkg.env$fatbears_ensemble_accuracy <- rbind(pkg.env$fatbears_ensemble_accuracy, accuracy)
}

#
get_fatbears_ensemble_accuracy <- function(){
  return(pkg.env$fatbears_ensemble_accuracy)
}

#
clear_fatbears_ensemble_accuracy <- function(){
  pkg.env$fatbears_ensemble_accuracy <- list()
}
