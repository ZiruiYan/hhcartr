# source: node.R

############################################################################################################
#' NNode returns a tree node.
#'
#' This internal function is used to create a tree node. Remaining parameters will be initialised
#' as the tree induction process progresses.
#'
#' @param gini The gini index of the node.
#' @param num_samples The number of samples at this node.
#' @param num_samples_per_class A table showing class distribution at this node.
#' @param predicted_class If a leaf node, the predicted class of this node.
#' @param parent_node The parent node object for this child node.
#' @param objectid A unique id for this node object.
#' @param oob_row_indices For a root node the out-of-bag indices from original training
#' set used to create this tree.
#' @return a tree node of type hash.
#'

NNode <- function(gini,
                  num_samples,
                  num_samples_per_class,
                  predicted_class,
                  parent_node,
                  objectid,
                  oob_row_indices = NA){
  # create a hash object, will be used to represent decision tree root, internal and
  # leaf nodes.
  hx <- hash::hash()
  hx["node_gini"]                  <- gini
  hx["node_tot_samples"]           <- num_samples
  hx["node_num_samples_per_class"] <- num_samples_per_class     #typeof class
  hx["node_predicted_class"]       <- predicted_class
  hx["node_feature_index"]         <- NA
  hx["node_threshold"]             <- NA
  hx["node_householder_matrix"]    <- NA
  hx["node_children_left"]         <- NA
  hx["node_children_right"]        <- NA
  hx["node_parent"]                <- parent_node
  hx["node_parentid"]              <- NA
  hx["node_objectid"]              <- objectid
  hx["node_type"]                  <- NA
  hx["node_using_householder"]     <- NA
  hx["node_children_left_NA"]      <- TRUE
  hx["node_children_right_NA"]     <- TRUE
  hx["node_oob_training_indices"]  <- oob_row_indices
  hx["node_depth"]                 <- 0
  hx["node_vals"]                  <- NA
  hx["node_indices_left"]          <- NA
  hx["node_not_indices_left"]      <- NA
  hx["node_reverse_cond"]          <- FALSE
  # fields for cost complexity pruning
  # r(t) misclassification error at this node t
  hx["node_r_t"]                   <- 0
  # p(t) proportion of data items reaching this node t
  hx["node_p_t"]                   <- 0
  # R(t) = r(t)*p(t)  training error at node t.
  hx["node_R_t"]                   <- 0
  # the number of samples misclassified at this node t
  hx["node_number_misclassified"]  <- 0
  #
  hx["node_processed_as_subtree"]  <- FALSE
  #
  hx["node_subtree_num"]           <- NA
  # return node.
  return(hx)
}
