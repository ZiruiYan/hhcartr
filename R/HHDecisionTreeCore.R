# source: HHDecisionTreeCore.R

# Package:  hhcartr

# This package implements the HHCART(G) algorithm as described in the following paper:
# Wickramarachchi C, Robertson B, Reale M, Price C, Brown J (2019). “A reflected feature
# space for CART.” Australian & New Zealand Journal of Statistics, 61, 380–391. doi:
# 10.1111/anzs.12275.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @importFrom graphics abline
#' @importFrom graphics hist
#' @importFrom graphics matplot
#' @importFrom stats cov
#' @importFrom stats quantile
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @importFrom DiagrammeR grViz
#' @importFrom utils install.packages
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit

#
# --------------------------------------------------
# package local variables
#
# https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
pkg.env                           <- new.env(parent=emptyenv())

pkg.env$node_count                <- 0
pkg.env$leaf_count                <- 0
pkg.env$objectid_count            <- 0
pkg.env$dot_list                  <- list()
pkg.env$run_stats                 <- list()
pkg.env$node_count_node_vector    <- list()
pkg.env$node_count_leaf_vector    <- list()
pkg.env$model_fit_results         <- list()
pkg.env$model_data_description    <- "Unknown"
pkg.env$folds_trees               <- list()
pkg.env$max_features              <- NA
pkg.env$total_features            <- NA
pkg.env$oobee_accuracy            <- list()
pkg.env$model_name                <- NA
pkg.env$model_feature_importances <- list()
pkg.env$feature_names             <- NA
pkg.env$tree_strength             <- list()
pkg.env$trial_strength            <- list()
pkg.env$tree_margin               <- list()
pkg.env$useIdentity               <- NA
pkg.env$classify                  <- NA
pkg.env$n_folds                   <- NA
pkg.env$n_trials                  <- NA
pkg.env$n_trees                   <- NA
pkg.env$n_min                     <- NA
pkg.env$min_node_impurity         <- NA
pkg.env$sampleWithReplacement     <- NA
pkg.env$testSize                  <- NA
pkg.env$sample_size               <- NA
pkg.env$min_node_impurity_data    <- list()
pkg.env$seed                      <- NA
pkg.env$num_training_samples      <- NA
pkg.env$rpart_num_training_samples <- NA
pkg.env$subtree_count             <- NA
pkg.env$pruning                   <- FALSE
pkg.env$pruned_data               <- list()   # delete ?
pkg.env$ccp_subtree_data          <- list()
pkg.env$ccp_phase_data            <- list()
pkg.env$ccp_predictions           <- list()
pkg.env$fatbears_predictions      <- data.frame()
pkg.env$partition_data            <- list()
pkg.env$fatbears_ensemble_accuracy <- list()
pkg.env$using_cpp                 <- TRUE
pkg.env$cpp_subtree_data          <- NA
pkg.env$cpp_subtree_predictions   <- NA
pkg.env$r_subtree_data            <- NA
pkg.env$r_subtree_predictions     <- NA
pkg.env$show_progress             <- FALSE

# mni - min_node_impurity parameters - default values
pkg.env$mni_trials                <- 1
pkg.env$mni_n_folds               <- 10
pkg.env$mni_n_trees               <- 1
pkg.env$mni_size                  <- 0.01
pkg.env$mni_start                 <- 0.05
pkg.env$mni_numvals               <- 50

# prune - pruning parameters for ccp and fatbears pruning methods - default values
pkg.env$prune_type                <- "ccp"
pkg.env$prune_fatbears_max_nodes  <- 10
pkg.env$prune_fatbears_max_depth  <- 10
pkg.env$prune_fatbears_iterations <- 10

#################################################################################################
#'
#' HHDecisionTreeCore Common function for all hhcartr model functions.
#'
#' This function internal function provides a common interface for all hhcartr model function.
#' At the time of writing these are HHDecisionTreeClassifier and HHDecisionTreeRegressor. The
#' following parameters are supported (they are not necessarily all common to the classifier and
#' regressor models - look at documentation for each model).
#'
#' @param response The response parameter is used to specify what type of model to build, either 'classify'
#' for a classification tree model or 'regressor' for a regression tree model. The default is 'classify'.
#' @param n_min The n min parameter is used to stop splitting a node when a minimum
#' number of samples at that node has been reached. The default value is 2.
#' @param min_node_impurity The min node impurity parameter is used to stop splitting a node
#' if the node impurity at that node is less than this value. The node impurity is calculated
#' using the hyperplane Gini index. The default value is 0.2.
#' @param n_trees The n trees parameter is used to specify the number of trees to use(grow) per
#' fold or trial. The default value is 1.
#' @param n_folds The n folds parameter is used to specify the number of folds to use i.e. split
#' the input data into n folds equal amounts, for n folds times, use one portion of the input
#' data as a test dataset, and the remaining n folds-1 portions as the training dataset. The model
#' is then trained using these training and test datasets, once training complete the next fold or
#' portion of the input dataset is treated as the test dataset and the remainder the training
#' dataset, the model is then trained again. This process is repeated until all portions or folds
#' of the input dataset have been used as a test dataset. The default value is 5.
#' @param sample_size The sample size parameter is used to determine how much of the training
#' dataset is actually used during training. A value of 1.0 allows all of the current training
#' dataset to be used for training. A value of less than one will mean that proportion of the
#' training dataset will be selected at random and then used for training. The value of parameter
#' sampleWithReplacement will determine if the random sampling of the training dataset is
#' performed using replacement or not. The default value is 1.0.
#' @param testSize The testSize parameter determines how much of the input dataset is to be used
#' as the test dataset. The remainder is used as the training dataset. This parameter is only used
#' when the parameter n_folds=1. For values of n_folds greater than one, the computed fold size will
#' govern the test dataset size used (see the n_folds parameter for more details). The default value
#' is 0.2.
#' @param sampleWithReplacement The sampleWithReplacement parameter is used in conjunction with
#' the sample size parameter. The sampleWithReplacement parameter will determine if sampling from
#' the training dataset is done with or without replacement. The default value is FALSE.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#' original training data to find the optimal splits rather than using the reflected data. The
#' default value is FALSE.
#' @param dataDescription The dataDescription parameter is a short description used to describe
#' the dataset being modelled. It is used is output displays and plots as documentation. The
#' default value is “Unknown”.
#' @param max_features The max features parameter determines the number of features to consider
#' when looking for the best split, and can take one of the values listed below. The default value
#' is “sqrt”.
#' @param pruning The pruning parameter when set TRUE specifies that the induced tree is to be
#' pruned after induction. The default value is FALSE.
#' @param classify The classify parameter when set TRUE indicates that the data is for building
#' a classification model. A value of FALSE and a regression model will be induced.
#' @param parallelize The parallelize parameter when set TRUE will allow selected loops to be run
#' in parallel. (This functionality has yet to be fully tested). The default value is FALSE.
#' @param number_cpus The number of available CPU’s to use when parameter parallelize is set to
#' TRUE. The maximum number of CPU’s to be used will be the number of physical CPU’s available
#' (as returned via the detectCores() function of the parallel package) minus one. The default
#' value is 1.
#' @param show_progress The show_progress parameter when set TRUE will cause progress messages
#' to be displayed as trees are induced. A value of FALSE will result in no progress messages being
#' displayed. The default value is FALSE.
#' @param control Default value mni.control(n_folds = 5). The control parameter is used to specify parameters for the mni.control
#' function. See documentation for mni.control for supported parameters.
#' @param prune_control Default value prune.control(prune_type = "all",
#'                                                  prune_fatbears_max_nodes = 10,
#'                                                  prune_fatbears_iterations = 1000)
#' The prune_control parameter is used to specify parameters for the prune.control
#' function. This parameter is only used when 'pruning = TRUE'. See documentation for
#' prune.control for supported parameters.
#' @param debug_msgs Not fully implemented yet but will turn on debug messages.
#' @param seed Specify a seed to seed the RNG. Acceptable values are 1-9999. If no
#' value is specified a random integer in the range 1-9999 is used.
#'
#' @return a list of the trees induced during training, these are saved in global enviornment
#'         variable pkg.env$folds_trees.
#'

#' @export
HHDecisionTreeCore <- function(response              = "classify",
                               n_min                 = 2,
                               min_node_impurity     = 0.2,
                               n_trees               = 1,
                               n_folds               = 5,
                               sample_size           = 1.0,
                               testSize              = 0.20,
                               sampleWithReplacement = FALSE,
                               useIdentity           = FALSE,
                               dataDescription       = "Unknown",
                               max_features          = "None",
                               pruning               = FALSE,
                               parallelize           = FALSE,
                               number_cpus           = 1,
                               show_progress         = FALSE,
                               seed                  = seed,
                               control               = control,
                               prune_control         = prune_control,
                               debug_msgs            = FALSE) {

  # validate model input parameters.
  validate_parameters(response,
                      n_min,
                      n_folds,
                      n_trees,
                      min_node_impurity,
                      sample_size,
                      testSize,
                      sampleWithReplacement,
                      useIdentity,
                      pruning,
                      parallelize)

  # Get the environment for this instance of the function.
  thisEnv <- environment()

  # validate mni.control parameters
  mni_parms <- control
  # parameters all validated, now save
  pkg.env$mni_trials  <- mni_parms[[1]]
  pkg.env$mni_n_folds <- mni_parms[[2]]
  pkg.env$mni_n_trees <- mni_parms[[3]]
  pkg.env$mni_size    <- mni_parms[[4]]
  pkg.env$mni_start   <- mni_parms[[5]]
  pkg.env$mni_numvals <- mni_parms[[6]]

  # validate prune.control parameters
  prune_parms <- prune_control
  # parameters all validated, now save
  pkg.env$prune_type                <- prune_parms[[1]]
  pkg.env$prune_fatbears_max_nodes  <- prune_parms[[2]]
  pkg.env$prune_fatbears_max_depth  <- prune_parms[[3]]
  pkg.env$prune_fatbears_iterations <- prune_parms[[4]]

  # set internal parameters based upon response parameter value.
  if(response == "classify"){
    modelName <- "HHDecisionTreeClassifier"
    classify  <- TRUE
  } else {
    modelName <- "HHDecisionTreeRegressor"
    classify  <- FALSE
  }

  # initialise hhcartr environment variables with validated parameters
  pkg.env$useIdentity           <- useIdentity
  pkg.env$classify              <- classify
  pkg.env$n_folds               <- n_folds
  pkg.env$n_trees               <- n_trees
  pkg.env$n_min                 <- n_min
  pkg.env$min_node_impurity     <- min_node_impurity
  pkg.env$sampleWithReplacement <- sampleWithReplacement
  pkg.env$testSize              <- testSize
  pkg.env$sample_size           <- sample_size
  pkg.env$pruning               <- pruning
  pkg.env$seed                  <- seed
  pkg.env$show_progress         <- show_progress

  # save the data description
  setDataDescription(dataDescription)

  # save the model type/name
  set_model_name(modelName)


  parms <- list(
    ##

    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    #thisEnv <- pkg.env,

    getRunStats = function(){
      msg             <- "%s() : Run Statistics for Dataset: %s"
      msgs            <- sprintf(msg, get_model_name(), get_data_description())
      message(msgs)
      message("using parameters:")
      if(classify){
        msg             <- "n_folds-[%s]  n_trees-[%s]  n_min-[%s]  min_node_impurity-[%s]"
        msgs            <- sprintf(msg, n_folds, n_trees, n_min, pkg.env$min_node_impurity)
        message(msgs)
        msg             <- "useIdentity-[%s]  testSize-[%s]"
        msgs            <- sprintf(msg, useIdentity, testSize)
        message(msgs)
      } else {
        msg             <- "n_folds-[%s]  n_trees-[%s]  n_min-[%s] "
        msgs            <- sprintf(msg, n_folds, n_trees, n_min)
        message(msgs)
        msg             <- "useIdentity-[%s]  testSize-[%s]"
        msgs            <- sprintf(msg, useIdentity, testSize)
        message(msgs)
      }

      runStats        <- get_run_stats(classify)
      meanTotalNodes  <- mean(runStats[[1]]$Number_of_Nodes)
      meanTotalLeaves <- mean(runStats[[1]]$Number_of_Leaves)
      if(classify){
        meanAccuracy  <- mean(runStats[[1]]$Accuracy)
        msg           <- "Mean Accuracy-[%s] Mean Nodes-[%s] Mean Leaves-[%s]"
        msgs          <- sprintf(msg, round(meanAccuracy,2), round(meanTotalNodes,2), round(meanTotalLeaves,2))
        message(msgs)
      } else {
        meanRsquare   <- mean(runStats[[1]]$R_squared)
        meanRMSE      <- mean(runStats[[1]]$RMSE)
        msg           <- "Mean R-Squared-[%s] Mean RMSE-[%s] Mean Nodes-[%s] Mean Leaves-[%s]"
        msgs          <- sprintf(msg, round(meanRsquare,2), round(meanRMSE,2), round(meanTotalNodes,2), round(meanTotalLeaves,2))
        message(msgs)
      }
      return(runStats)
    },

    ## Fit function
    fit = function(X, y){

      # go compute value for max_features_
      n_features_   <- dim(X)[2]
      max_features_ <- compute_max_features(max_features, n_features_)
      set_max_features(max_features_)

      # verify input data, won't return if problem found
      hhcart_verify_input_data(X, y, classify, max_features_, n_features_)

      # ------------------------------- Start min_node_impurity check --------------------------
      # check if we have to compute the min_node_impurity value
      if(min_node_impurity == "auto"){
        # save n_folds and n_trees as they are overwritten during process to
        # compute min_node_impurity
        save_show_progress             <- pkg.env$show_progress
        save_seed                      <- pkg.env$seed
        save_n_folds                   <- pkg.env$n_folds
        save_n_trees                   <- pkg.env$n_trees
        save_pruning                   <- pkg.env$pruning
        save_data_description          <- pkg.env$model_data_description
        res                            <- compute_min_node_impurity(X, y, response, seed, useIdentity)
        min_node_impurity              <- res[[1]]
        new_seed                       <- res[[2]]
        # save our computed min_node_impurity value
        pkg.env$min_node_impurity      <- min_node_impurity
        # restore n_folds and n_trees to original values.
        pkg.env$show_progress          <- save_show_progress
        pkg.env$seed                   <- new_seed
        pkg.env$n_folds                <- save_n_folds
        pkg.env$n_trees                <- save_n_trees
        pkg.env$pruning                <- save_pruning
        pkg.env$model_data_description <- save_data_description
      }
      # ------------------------------- End min_node_impurity check --------------------------

      # initialise pkg.env$model_fit_results
      clear_model_fit_results()

      # initialise folds trees results - pkg.env$folds_trees
      clear_trees()

      # initialise folds trees margin - pkg.env$tree_margin
      clear_margin()

      # initialise run stats - pkg.env$run_stats
      clear_run_stats()

      # initialise oob-ee stats - pkg.env$oobee_accuracy
      clear_oobee_accuracy()

      # initialise pruned data - pkg.env$pruned_data
      #clear_pruned_data()

      # initialise ccp pruned data - pkg.env$ccp_pruned_data
      clear_ccp_subtree_data()

      # initialise ccp predictions - pkg.env$ccp_predictions
      clear_ccp_predictions()

      # initialise fatbears predictions - pkg.env$fatbears_predictions
      #clear_fatbears_predictions()

      # initialise partition data - pkg.env$partition_data
      clear_partition_data()

      # initialise c++ fatbears subtree data - pkg.env$cpp_subtree_data
      clear_cpp_subtree_data()
      # initialise c++ fatbears subtree predictions - pkg.env$cpp_subtree_predictions
      clear_cpp_subtree_predictions()

      # initialise R fatbears subtree data - pkg.env$r_subtree_data
      clear_r_subtree_data()
      # initialise R fatbears subtree predictions - pkg.env$r_subtree_predictions
      clear_r_subtree_predictions()

      # initialise to force build of tree data frame - pkg.env$using_cpp <- FALSE
      pkg.env$using_cpp <- TRUE

      # find number of classes from the target variable
      n_classes <- length(table(y))

      # join feature variables with target variable
      datas <- cbind(X,y)

      # make sure pkg.env$n_folds does nor exceed nrow(datas)
      if(pkg.env$n_folds > nrow(datas)){
        msg  <- "%s() n_folds [%s] must not exceed number of rows in the training dataset [%s]."
        msgs <- sprintf(msg, get_model_name(), pkg.env$n_folds, nrow(datas))
        stop(msgs)
      }
      # find number of samples per fold
      fold_size <- as.integer(nrow(datas) / pkg.env$n_folds)

      # limit the following loop to loop_limit times
      loop_limit <- pkg.env$n_folds
      loop_limit_remainder <- nrow(datas) %% pkg.env$n_folds
      loop_count <- 0
      z <- 0

      # initialise
      number_tree_nodes  <- c(rep(0, pkg.env$n_trees * pkg.env$n_folds))
      number_tree_leaves <- c(rep(0, pkg.env$n_trees * pkg.env$n_folds))

      results <- list()

      msg  <- "%s %s() tree inference starts. Fold size=[%s]."
      msgs <- sprintf(msg, Sys.time(), get_model_name(), fold_size)
      message(msgs)

      # if seed not specified, generate a random one.
      if(is.na(seed)){
        seed <- sample(1:9999, 1)
      }

      pkg.env$seed <- seed
      if(pkg.env$show_progress){
        msg  <- "%s %s() Using seed = [%s]."
        msgs <- sprintf(msg, Sys.time(), get_model_name(), seed)
        message(msgs)
      }

      # pass train and test to classifier/regressor
      for(j in 1:pkg.env$n_trees){
        # zero object_id count - pkg.env$objectid_count - before inducing each tree
        zero_objectid_count()

        loop_count <- 0
        seed <- seed + j
        set.seed(seed)
        indices <- sample(nrow(datas), nrow(datas))

        # process for each fold
        for(i in seq(1, nrow(datas), fold_size)){
          # zero object_id count - pkg.env$objectid_count - before inducing each tree
          zero_objectid_count()

          loop_count <- loop_count + 1
          if(loop_count > loop_limit){
            next
          }
          if(pkg.env$show_progress){
            msg  <- "%s ***** Starting Fold %s of %s. Tree %s of %s."
            msgs <- sprintf(msg, Sys.time(), loop_count, pkg.env$n_folds, j, pkg.env$n_trees)
            message(msgs)
          }

          k <- (i + fold_size) - 1
          if(loop_count == loop_limit){
            k <- k + loop_limit_remainder
          }
          if(n_folds == 1){
            # here need to calculate train/test split
            if(testSize > 0.0){
              # only do this is testSize is GT 0.0
              new_k <- as.integer(k * testSize)
              test_samples <- indices[i:new_k]
            }
          } else {
            test_samples <- indices[i:k]
          }
          if(testSize > 0.0){
            train <- datas[-test_samples,]
            test <- datas[test_samples,]
          } else {
            # using testSize = 0.0, so use all dats for both train and test set (review)
            train <- datas
            test <- datas
          }

          # initialise for new tree
          zero_leaf_count()
          zero_node_count()
          z <- z + 1

          # call classifier - returns an object of type S4
          current_tree <- hhcart_run_classifier(train,
                                                pkg.env$sample_size,
                                                j,
                                                pkg.env$n_min,
                                                pkg.env$min_node_impurity,
                                                sampleWithReplacement,
                                                useIdentity,
                                                classify     = classify,
                                                n_features   = n_features_,
                                                n_classes    = n_classes,
                                                max_features = max_features_)
          # save current tree
          results <- append(results, current_tree)
          # save number of nodes/leaves for current tree
          number_tree_nodes[z]  <- get_node_count()
          number_tree_leaves[z] <- get_leaf_count()

          # go and make predictions on the test set
          prediction_output <- make_predictions(results, test, useIdentity, classify, 999999)

          # tree accuracy in [[1]]
          stats <- prediction_output[[1]]

          # tree mr in [[2]]
          tree_mr <- prediction_output[[2]]
          # save this folds trees
          save_margin(tree_mr)

          # save accuracy stats for current fold
          save_run_stats(stats)

          if(debug_msgs){
            msg  <- "Node Count=%s Leaf Count=%s"
            msgs <- sprintf(msg, get_node_count(), get_leaf_count())
            message(msgs)
          }

          # save this folds trees
          save_trees(results)

          # ----------------------------------- Start Pruning ----------------------------------------
          if(pkg.env$pruning){

            pkg.env$using_cpp <- FALSE

            # ----------------------------------- Start CCP Pruning ----------------------------------
            if (pkg.env$prune_type %in% c("all", "ccp")){
              # here perform ccp - need to test a parameter first to see if ccp required.
              ccp_results <- perform_ccp_driver(list(current_tree))
              # get alpha-df from ccp_results
              alpha_df <- ccp_results[[1]]
              if(nrow(alpha_df) > 0){
                # go make predictions on our subtrees
                res <- pruning_make_predictions(loop_count,
                                                     j,
                                                     alpha_df,
                                                     current_tree,
                                                     test,
                                                     useIdentity,
                                                     classify,
                                                     colname = "collapse_this_node",
                                                     pred_type = "ccp")
                ccp_new_df <- res[[1]]
                ccp_predictions_df <- res[[2]]
                # save our ccp data for later use. get using res$ccp_pruned_data().
                save_ccp_subtree_data(ccp_new_df)
                # save our predictions for later use. get using res$ccp_predictions().
                save_ccp_predictions(ccp_predictions_df)
              }
            }

            # ----------------------------------- End CCP Pruning ------------------------------------

          } # End if Pruning

          # clear for next fold
          results <- list()

        } # end for fold_size

      } # end for pkg.env$n_trees

      # completed all trees/folds
      msg  <- "%s %s() tree inference complete."
      msgs <- sprintf(msg, Sys.time(), get_model_name())
      message(msgs)

      save_count_tree_node_vector(number_tree_nodes)
      save_count_tree_leaf_vector(number_tree_leaves)

      # set name for S3 class
      class(pkg.env$folds_trees) <- "hhcartr"
      return(pkg.env$folds_trees)
    }

  )

  ## Define the value of the list within the current environment.
  assign('this', parms, envir=thisEnv)

  ## Set the name for the class
  class(parms) <- append(class(parms),"HHDecisionTreeCore")
  return(parms)
}
