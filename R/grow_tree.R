# Source: grow_tree.R

#################################################################################################
#'
#' grow_tree_
#'
#' This internal function is used to grow the decision tree, it is called recursively until
#' the stopping criteria are met.
#'
#' The following parameters are supported:
#'
#' @param node The current node to be tested for a split.
#' @param X The feature variables of the current node being processed.
#' @param y The target variable for the feature variables.
#' @param n_min The n_min parameter is used to terminate node splitting when a minimum number
#'        of samples at that node has been reached. The default value is 2.
#' @param min_node_impurity Splitting a node will stop when the impurity of a node is less than
#'        min_node_impurity. The node impurity is calculated using the hyperplane Gini index.
#'        The default value is 0.2.
#' @param useIdentity The useIdentity parameter when set TRUE will result in hhcartr using the
#'        original training data to find the optimal splits rather than using the reflected data.
#'        The default value is FALSE.
#' @param classify The classify parameter when set TRUE will result in hhcartr performing a
#'        classification, when set FALSE will perform a regression.
#' @param n_features The number of feature variables.
#' @param n_classes The number of classes in the target variable.
#' @param max_features The maximum number of features to consider in the current split.
#' @param depth This parameter is not used as yet.
#'
#' @return Returns the latest node of type NNode.
#'

grow_tree_ = function(node,
                      X,
                      y,
                      n_min,
                      min_node_impurity,
                      useIdentity,
                      classify = TRUE,
                      n_features,
                      n_classes,
                      max_features,
                      depth = 0){

  X_left  <- list()
  X_right <- list()
  y_left  <- list()
  y_right <- list()

  n_min   <- n_min
  min_node_impurity <- min_node_impurity

  if(classify){
    num_samples_per_class <- data.frame(table(y))
    most_frequent_class <- max(num_samples_per_class$Freq, na.rm = TRUE)
    # problem here
    predicted_class <- num_samples_per_class$y[num_samples_per_class$Freq == most_frequent_class]
    # predict class at random when more than one class is the most frequent
    if(length(predicted_class) > 1){
      predicted_class <- sample(predicted_class, 1)
    }
  } else {
    predicted_class <- mean(y)
    num_samples_per_class <- NA
  }

  if(typeof(y) == 'list'){
    length_of_y <- nrow(y)
  } else{
    length_of_y <- length(y)
  }

  # set basic fields in node object
  node$node_gini                  <- gini_(y, length_of_y)
  node$node_tot_samples           <- length_of_y
  node$node_num_samples_per_class <- num_samples_per_class
  node$node_predicted_class       <- predicted_class
  node$node_depth                 <- depth

  # set cost complexity pruning values we currently know
  num_wrongly_class <- node$node_tot_samples - (num_samples_per_class$Freq[num_samples_per_class$y == predicted_class])
  node$node_number_misclassified <- num_wrongly_class
  node$node_r_t                  <- num_wrongly_class / node$node_tot_samples
  node$node_p_t                  <- node$node_tot_samples / pkg.env$num_training_samples
  node$node_R_t                  <- node$node_r_t * node$node_p_t

  # check the features, we are looking for the situation where all in each column all the row values
  # are the same.
  force_terminal_node_SAME <- FALSE
  valid_feature_list       <- find_number_valid_feature_columns(X, n_features)
  if(length(valid_feature_list) == 0 & !useIdentity){
    force_terminal_node_SAME <- TRUE
  }
  #
  force_terminal_node <- FALSE
  if(classify & !useIdentity){
    if(nrow(num_samples_per_class) == 2 & min(num_samples_per_class$Freq) == 1){
      force_terminal_node <- TRUE
    }
  }

  if ((classify & length_of_y > n_min & gini_(y, length_of_y) > min_node_impurity & !force_terminal_node & !force_terminal_node_SAME) |
      (!classify & length_of_y > n_min & !force_terminal_node_SAME)){

    results <- hhcart_reflect_feature_space_g(X,
                                              y,
                                              useIdentity  = useIdentity,
                                              max_features = max_features,
                                              n_features   = n_features,
                                              n_classes    = n_classes,
                                              classify     = classify,
                                              depth        = depth)
    idx <- results[1]
    idx <- idx[[1]]
    thr <- results[2]
    thr <- thr[[1]]
    vals <- results[3]
    vals <- vals[[1]]
    hh <- results[4]
    hh <- hh[[1]]
    using_householder <- results[5]
    using_householder <- using_householder[[1]]

    if (is.not.null(idx)){
      newt <- thr
      indices_left <- as.numeric(vals[,idx]) <= as.numeric(newt)

      X_left <- X[which(indices_left),]
      X_right <- X[which(!indices_left),]
      if(typeof(y) == 'list'){
        y_left <- y[which(indices_left),]
        y_right <- y[which(!indices_left),]
      } else{
        yt <- t(y)
        y_left <- yt[which(indices_left)]
        y_right <- yt[which(!indices_left)]
      }

      node$node_feature_index      <- idx
      node$node_threshold          <- newt
      node$node_householder_matrix <- hh
      node$node_type               <- "INTERNAL"
      node$node_using_householder  <- using_householder
      node$node_vals               <- vals
      node$node_indices_left       <- indices_left
      node$node_not_indices_left   <- !indices_left

      if((length(y_left) == 0 & length(y_right) != 0) | (length(y_left) != 0 & length(y_right) == 0)){
        # here then force terminal node
        node["node_children_left"]     <- NA
        node["node_children_left_NA"]  <- TRUE
        node["node_children_right"]    <- NA
        node["node_children_right_NA"] <- TRUE
        node$node_type                 <- "TERMINAL"
        increment_node_count()
        tree_depth_ <- depth
        if(node$node_children_left_NA & node$node_children_right_NA){
          increment_leaf_count()
        }
        # return terminal node
        return(node)
      }

      # here check if lefts are all the same value, if yes force terminal node
      if(length(y_left) != 0){
        increment_objectid_count()
        new_left_node <- NNode(NA, NA, NA, NA, NA, get_objectid_count())
        ln <- grow_tree_(new_left_node,
                         X_left,
                         y_left,
                         n_min,
                         min_node_impurity,
                         useIdentity,
                         classify     = classify,
                         n_features   = n_features,
                         n_classes    = n_classes,
                         max_features = max_features,
                         depth + 1)
        node["node_children_left"]    <- ln
        node["node_children_left_NA"] <- FALSE
        ln["node_parent"]             <- node
      } else {
        node["node_children_left"]    <- NA
        node["node_children_left_NA"] <- TRUE
      }

      # here check if rights are all the same value, if yes force terminal node
      if(length(y_right) != 0){
        increment_objectid_count()
        new_right_node <- NNode(NA, NA, NA, NA, NA, get_objectid_count())
        rn <- grow_tree_(new_right_node,
                         X_right,
                         y_right,
                         n_min,
                         min_node_impurity,
                         useIdentity,
                         classify     = classify,
                         n_features   = n_features,
                         n_classes    = n_classes,
                         max_features = max_features,
                         depth + 1)

        node["node_children_right"]    <- rn
        node["node_children_right_NA"] <- FALSE
        rn["node_parent"]              <- node
      } else{
        node["node_children_right"]    <- NA
        node["node_children_right_NA"] <- TRUE
      }

    } # if idx is not none
  } else {
    node["node_children_left"]     <- NA
    node["node_children_left_NA"]  <- TRUE
    node["node_children_right"]    <- NA
    node["node_children_right_NA"] <- TRUE
    node$node_type                 <- "TERMINAL"
  }

  increment_node_count()
  tree_depth_ <- depth
  if(node$node_children_left_NA & node$node_children_right_NA){
    increment_leaf_count()
  }

  # could add node to a list
  return(node)
} # end grow_tree_()
