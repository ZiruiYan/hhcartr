# source: HHDecisionTree.R

#######################################################################################################
#' HHDecisionTree.
#'
#' This function instantiates a HHDecisionTree model, it is used to induce classification or regression
#' trees depending upon the value of the response parameter. It supports the parameters listed below.
#'
#' @param response The response parameter is used to specify what type of model to build, either 'classify'
#' for a classification tree model or 'regressor' for a regression tree model. The default is 'classify'.
#' @param n_min The n_min parameter is used to stop splitting a node when a
#' minimum number of samples at that node has been reached. The default value is 2.
#' @param min_node_impurity The min node impurity parameter is used to stop splitting a node
#' if the node impurity at that node is less than this value. The node impurity is calculated
#' using the hyperplane Gini index. The default value is 0.2.
#' @param n_trees The n_trees parameter is used to specify the number of trees to use(grow) per
#' fold or trial. The default value is 1.
#' @param n_folds The n_folds parameter is used to specify the number of folds to use i.e. split
#' the input data into n_folds equal amounts, for n_folds times, use one portion of the input
#' data as a test dataset, and the remaining n_folds-1 portions as the training dataset. The model
#' is then trained using these training and test datasets, once training is complete the next fold or
#' portion of the input dataset is treated as the test dataset and the remainder the training
#' dataset, the model is then trained again. This process is repeated until all portions or folds
#' of the input dataset have been used as a test dataset. When n_folds=1 the testSize parameter
#' determines the size of the test dataset. The default value is 5.
#' @param testSize The testSize parameter determines how much of the input dataset is to be used
#' as the test dataset. The remainder is used as the training dataset. This parameter is only used
#' when the parameter n_folds=1. For values of n_folds greater than one, the computed fold size will
#' govern the test dataset size used (see the n_folds parameter for more details). The default value
#' is 0.2.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#' original training data to find the optimal splits rather than using the reflected data. The
#' default value is FALSE.
#' @param pruning The pruning parameter when set TRUE will result in tree pruning after all trees
#' are induced. The default value is FALSE.
#' @param dataDescription The dataDescription parameter is a short description used to describe
#' the dataset being modelled. It is used is output displays and plots as documentation. The
#' default value is “Unknown”.
#' @param control The control parameter is used to specify parameters for the mni.control
#' function. See documentation for mni.control for supported parameters.
#' @param prune_control The prune_control parameter is used to specify parameters for
#' the prune.control function. This parameter is only used when 'pruning = TRUE'. See
#' documentation for prune.control for supported parameters.
#' @param show_progress The show_progress parameter when set TRUE will cause progress messages
#' to be displayed as trees are induced. A value of FALSE will result in no progress messages being
#' displayed. The default value is TRUE.
#' @param seed Specify a seed to seed the RNG. Acceptable values are 1-9999. If no
#' value is specified a random integer in the range 1-9999 is used.
#'
#' @return Returns pkg.env$folds_trees, a list of all trees induced during training.
#'

#' @export
HHDecisionTree <- function(response          = "classify",
                           n_min             = 2,
                           min_node_impurity = 0.2,
                           n_trees           = 1,
                           n_folds           = 5,
                           testSize          = 0.20,
                           useIdentity       = FALSE,
                           pruning           = FALSE,
                           dataDescription   = "Unknown",
                           control           = mni.control(n_folds = 5),
                           prune_control     = prune.control(prune_type = "all",
                                                             prune_stochastic_max_nodes = 14,
                                                             prune_stochastic_max_depth = 20,
                                                             prune_stochastic_samples   = 3000),
                           show_progress     = FALSE,
                           seed              = NA){
  #
  hhdtr = HHDecisionTreeCore(response              = response,
                             n_min                 = n_min,
                             min_node_impurity     = min_node_impurity,
                             n_trees               = n_trees,
                             n_folds               = n_folds,
                             sample_size           = 1.0,
                             testSize              = testSize,
                             sampleWithReplacement = FALSE,
                             useIdentity           = useIdentity,
                             dataDescription       = dataDescription,
                             max_features          = "None",
                             pruning               = pruning,
                             parallelize           = FALSE,
                             number_cpus           = 1,
                             show_progress         = show_progress,
                             seed                  = seed,
                             control               = control,
                             prune_control         = prune_control,
                             debug_msgs            = FALSE)
  return(hhdtr)
}
