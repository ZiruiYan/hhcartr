# source: hhcartr_base_functions.R
#
# Contains functions that are common to all hhcartr models.

# hhcartr models - HHDecisionTree
#                - HHRandomForest

# base functions - validate_parameters
#                - gini_
#                - compute_max_features
#                - find_number_valid_feature_columns
#                - check_package
#                - notin
#                - is.not.null
#                - mni.control
#                - prune.control

validate_parameters <- function(response,
                                n_min,
                                n_folds,
                                n_trees,
                                min_node_impurity,
                                sample_size,
                                testSize,
                                sampleWithReplacement,
                                useIdentity,
                                pruning,
                                parallelize,
                                use_smote    = FALSE,
                                useOOBEE     = FALSE,
                                calcVarImp   = FALSE,
                                max_features = NA){

  # validate response parameter
  # valid parameter values
  response_choices <- c("classify", "regressor")
  # validate values of response parameter
  checkmate::assertChoice(response, response_choices)

  # validate n_min parameter
  checkmate::assertInt(n_min, lower = 1)

  # validate n_folds parameter
  checkmate::assertInt(n_folds, lower = 1)

  # validate n_trees parameter
  # use max_features to determine if using HHRandomForest or HHDecisionTree; if its not NA then
  # using HHRandomForest so can specify an upper limit of more than one.
  if(is.na(max_features)){
    checkmate::assertInt(n_trees, lower = 1, upper = 1)
  } else {
    checkmate::assertInt(n_trees, lower = 1, upper = 10000)
  }

  # validate min_node_impurity parameter
  if(typeof(min_node_impurity) == "character"){
    checkmate::assert_character(min_node_impurity, pattern = "auto")
  } else {
    checkmate::assert_number(min_node_impurity, lower = 0.0, upper = 1.0)
  }
  # validate sample_size parameter
  checkmate::assert_number(sample_size, lower = 0.10, upper = 1.0)
  # validate testSize parameter
  checkmate::assert_number(testSize, lower = 0.0, upper = 0.75)

  # valid parameter values
  choices <- c(TRUE, FALSE)
  # validate values of sampleWithReplacement parameter
  checkmate::assertChoice(sampleWithReplacement, choices)
  # validate values of useIdentity parameter
  checkmate::assertChoice(useIdentity, choices)
  # validate values of parallelize parameter
  checkmate::assertChoice(parallelize, choices)
  # validate values of pruning parameter
  checkmate::assertChoice(pruning, choices)
  # validate values of use_smote parameter
  checkmate::assertChoice(use_smote, choices)
  # validate values for the use_oobee parameter
  checkmate::assertChoice(useOOBEE, choices)
  # validate values for the calcVarImp parameter
  checkmate::assertChoice(calcVarImp, choices)

  if(!is.na(max_features)){
    # validate max_features parameter, can have the
    # following values: "sqrt", "log2", None, an int or
    # a type float.
    if(is.integer(max_features)){
      checkmate::assertInteger(max_features, lower = 1)
    } else if(is.double(max_features)){
      checkmate::assert_number(max_features, lower = 1.0)
    } else {
      choices <- c("sqrt", "log2", "None")
      checkmate::assertChoice(max_features, choices)
    }
  }

  # validate values of the classify parameter
  #classify_choices <- c(TRUE, FALSE)
  #checkmate::assertChoice(classify, classify_choices)

  # validate values of the modelName parameter
  #modelName_choices <- c("HHDecisionTreeClassifier", "HHDecisionTreeRegressor")
  #checkmate::assertChoice(modelName, modelName_choices)
}

# Calulate Gini-index
gini_ <- function(y, length_of_y){
  # Compute Gini impurity of a non-empty node.
  # Gini impurity is defined as Î£ p(1-p) over all classes, with p the frequency of a
  # class within the node. Since Î£ p = 1, this is equivalent to 1 - Î£ p^2.

  m                     <- length_of_y
  num_samples_per_class <- data.frame(table(y))
  G                     <- 1.0 - sum((num_samples_per_class$Freq/m)**2)
  return (G)
}

compute_max_features <- function(max_features, n_features_){
  # for random forests compute the number of features to randomly select from the
  # total number of features in the training set.
  if(max_features == "None"){
    return(n_features_)
  } else if(max_features == "sqrt"){
    return(as.integer(sqrt(n_features_)))
  } else if(is.integer(max_features)){
    return(max_features)
  } else if(is.double(max_features)){
    return(as.integer(max_features))
  } else {
    if(pkg.env$show_progress){
      message("compute_max_features() max_features unknown, defaulting to n_features_.")
    }
    return(n_features_)
  }
}

find_number_valid_feature_columns <- function(X, n_features_){
  # find number of feature columns in current training set.
  debug_msg <- FALSE
  # create list of indices to represent each column.
  initial_feature_list <- 1:n_features_
  for(k in 1:n_features_){
    # all the same values in current column?
    if(length(unique(X[,k])) == 1){
      # yes, so remove from the list.
      initial_feature_list <- initial_feature_list[!initial_feature_list %in% k]
      if(debug_msg){
        msg <- "find_number_valid_feature_columns() Skipping column %s as all values are the same."
        msgs <- sprintf(msg, k)
        message(msgs)
      }
    }
  }
  # if(length(initial_feature_list)==0) then no valid columns.
  return(initial_feature_list)
}

check_package <- function(pkgname){
  package.check <- lapply(pkgname, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      # no ! - can't install packages in my package.
      #install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

# notin function
`%notin%` <- Negate(`%in%`)

# is.not.null function
is.not.null <- function(x) !is.null(x)

#################################################################################################
#'
#' mni.control
#'
#' This internal function is used to validate the parameters specified on the control
#' parameter.
#'
#' The following parameters are supported:
#'
#' @param mni_trials The number of times the process is repeated i.e. how many times the
#' n-fold cross-validation process is repeated. The resulting value of min_node_impurity
#' is the mean of mni_trials trials. The default value is 1.
#' @param mni_n_folds The number of folds to use when evaluating values of min_node_impurity.
#' Any integer value in the range 1 to the number of observations in the training dataset is
#' accepted. The default value is 10.
#' @param mni_n_trees The number of trees to use when evaluating values of min_node_impurity.
#' At the time of writing the only allowable value is 1. The default value is 1.
#' @param mni_size After a value of min_node_impurity is tried, the next value is calculated
#' by adding mni_size to the previous value. A value in the range 0.001 to 0.10 is accepted.
#' The default value is 0.01.
#' @param mni_start The first value of min_node_impurity to be evaluated. A value in the range
#' 0.001 to 1.0 is accepted. The default value is 0.05.
#' @param mni_numvals The number of min_node_impurity values to try while attempting to find
#' the optimum. Any integer value in the range 1 to 1000 is accepted. The default value is 50.
#' @param ... parameter catch all.
#'
#' @return Returns a list of all validated parameters.
#'
#' @export
mni.control <- function(mni_trials  = 1,
                        mni_n_folds = 10,
                        mni_n_trees = 1,
                        mni_size    = 0.01,
                        mni_start   = 0.05,
                        mni_numvals = 50,
                        ...){
  mni_parms <- c(as.list(environment()), list(...))

  # validate mni_trials parameter
  checkmate::assertInt(mni_parms$mni_trials, lower = 1)

  # validate mni_n_folds parameter
  checkmate::assertInt(mni_parms$mni_n_folds, lower = 1)

  # validate mni_n_trees parameter
  checkmate::assertInt(mni_parms$mni_n_trees, lower = 1, upper = 1)

  # validate mni_size parameter
  checkmate::assert_number(mni_parms$mni_size, lower = 0.001, upper = 0.10)

  # validate mni_start parameter
  checkmate::assert_number(mni_parms$mni_start, lower = 0.001, upper = 1.0)

  # validate mni_numvals parameter
  checkmate::assertInt(mni_parms$mni_numvals, lower = 1, upper = 1000)

  # parameters all validated, now save
  outp <- list(mni_parms$mni_trials,
               mni_parms$mni_n_folds,
               mni_parms$mni_n_trees,
               mni_parms$mni_size,
               mni_parms$mni_start,
               mni_parms$mni_numvals)
  return(outp)
}

#################################################################################################
#'
#' prune.control
#'
#' This internal function is used to validate the parameters specified on the
#' prune_control parameter.
#'
#' The following parameters are supported:
#'
#' @param prune_type The prune type required, valid values are 'all', 'ccp' and
#' stochastic'. The default value is 'all'.
#' @param prune_stochastic_max_nodes The prune_stochastic_max_nodes parameter specifies
#' the number of internal nodes to randomly sample on each prune_stochastic_samples. The
#' value specified must be an even number as an equal number of left and right internal nodes
#' will form the sample. The prune_stochastic_max_nodes parameter can have a value of any even
#' integer in the range two to twenty-four. The default value is 10.
#' @param prune_stochastic_max_depth When sampling internal nodes, the
#' prune_stochastic_max_depth parameter specifies the maximum decision tree depth to select
#' internal nodes from. Internal nodes occurring in the original decision tree at depths
#' greater than prune_stochastic_max_depth are not eligible for sampling. Any positive integer
#' in the range two to the maximum depth of the current tree is accepted.
#' The default value is 12.
#' @param prune_stochastic_samples The prune_stochastic_samples parameter specifies the number
#' of times internal nodes will be sampled from the current decision tree. The number of
#' internal nodes to be sampled each iteration is determined by the prune_stochastic_max_nodes
#' parameter, the internal nodes eligible to be sampled is determined by the
#' prune_stochastic_max_depth parameter. The prune_stochastic_samples parameter can have any
#' positive integer value greater than zero. The default value is 100.
#' @param ... parameter catch-all.
#'
#' @return Returns a list of validated parameters.
#'
#' @export
prune.control <- function(prune_type                  = "ccp",
                          prune_stochastic_max_nodes  = 10,
                          prune_stochastic_max_depth  = 10,
                          prune_stochastic_samples    = 100,
                          ...){
  prune_parms <- c(as.list(environment()), list(...))

  # validate prune_type parameter
  # valid parameter values are one of - "ccp", "stochastic", "all"
  choices <- c("ccp", "all") # only this value supported in V1
  checkmate::assertChoice(prune_parms$prune_type, choices)

  # validate prune_stochastic_max_nodes parameter
  checkmate::assertInt(prune_parms$prune_stochastic_max_nodes, lower = 2, upper = 24)
  # ensure value is divisible evenly by 2.
  checkmate::assert(prune_parms$prune_stochastic_max_nodes %% 2 == 0)

  # validate prune_stochastic_max_depth parameter
  checkmate::assertInt(prune_parms$prune_stochastic_max_depth, lower = 1)

  # validate prune_stochastic_iterations parameter
  checkmate::assertInt(prune_parms$prune_stochastic_samples, lower = 1, upper = 10000)

  # parameters all validated, now save
  outp <- list(prune_parms$prune_type,
               prune_parms$prune_stochastic_max_nodes,
               prune_parms$prune_stochastic_max_depth,
               prune_parms$prune_stochastic_samples)
  return(outp)
}
