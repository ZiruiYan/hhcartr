# source: hhcartr_cpp_support.R

convert_tree_nodes <- function(i, root_node, subtree_count, node_count, df, node_bucket, subtreemode, nodeleftorright){

  node_count <- node_count + 1
  newrow <- data.frame(Tree_num          = i,
                       SubTree_num       = subtree_count,
                       Node_num          = node_count,
                       Node_type         = root_node$node_type,
                       Misclassified_num = root_node$node_number_misclassified,
                       Samples_num       = root_node$node_tot_samples,
                       Depth             = root_node$node_depth,
                       Object_id         = root_node$node_objectid,
                       Parent_id         = 0,
                       NodeLeftorRight   = nodeleftorright,
                       r_t               = root_node$node_r_t,
                       p_t               = root_node$node_p_t,
                       R_t               = root_node$node_R_t)

  newrow$cpp_node_children_left_NA   <- root_node$node_children_left_NA
  newrow$cpp_node_children_right_NA  <- root_node$node_children_right_NA
  newrow$cpp_node_using_householder  <- root_node$node_using_householder
  newrow$cpp_node_feature_index      <- root_node$node_feature_index
  newrow$cpp_node_threshold          <- root_node$node_threshold
  newrow$cpp_node_child_left         <- -1
  newrow$cpp_node_child_right        <- -1
  newrow$cpp_node_predicted_class    <- as.integer(root_node$node_predicted_class)
  newrow$cpp_node_householder_matrix <- list(root_node$node_householder_matrix)

  if(!root_node$node_children_left_NA){
    newrow$cpp_node_child_left       <- root_node$node_children_left$node_objectid
  }
  if(!root_node$node_children_right_NA){
    newrow$cpp_node_child_right      <- root_node$node_children_right$node_objectid
  }

  # see if we an find the parent node
  if(typeof(root_node$node_parent) == "S4"){
    newrow$Parent_id <- root_node$node_parent$node_objectid
  }
  df <- rbind(df, newrow)

  # does this node have a left child node?
  if(!root_node$node_children_left_NA){
    ln <- root_node$node_children_left
    results <- convert_tree_nodes(i,
                                  ln,
                                  subtree_count,
                                  node_count,
                                  df,
                                  node_bucket,
                                  FALSE,
                                  "L")
    node_count  <- results[[1]]
    df          <- results[[2]]
    node_bucket <- results[[3]]
  } else {
    outp <- list(node_count, df, node_bucket)
    return(outp)
  }
  # does this node have a right child node?
  if(!root_node$node_children_right_NA){
    rn <- root_node$node_children_right
    results <- convert_tree_nodes(i,
                                  rn,
                                  subtree_count,
                                  node_count,
                                  df,
                                  node_bucket,
                                  FALSE,
                                  "R")
    node_count  <- results[[1]]
    df          <- results[[2]]
    node_bucket <- results[[3]]
  } else {
    outp <- list(node_count, df, node_bucket)
    return(outp)
  }
  outp <- list(node_count, df, node_bucket)
  return(outp)
}

convert_tree_to_dataframe <- function(treeobjs, df){

  loop_start <- 1
  loop_end   <- length(treeobjs)
  node_bucket <- list()

  # for each tree...
  for(i in loop_start:loop_end){
    subtree_count <- 1
    node_count <- 0
    root_node <- treeobjs[[i]]
    root_node$node_subtree_num <- subtree_count
    results <- convert_tree_nodes(i,
                                  root_node,
                                  subtree_count,
                                  node_count,
                                  df,
                                  node_bucket,
                                  FALSE,
                                  "ROOT")
    node_count  <- results[[1]]
    df          <- results[[2]]
    node_bucket <- results[[3]]
  }
  return(df)
}


convert_list_trees <- function(list_trees){
  # Need to convert our trees in list_trees to a matrix format.
  # Only supports a list of one at the moment.
  if(length(list_trees) != 1){
    stop("hhcartr(convert_list_trees) More than one tree is not supported.")
  }

  # create empty dataframe
  df <- structure(list(Tree_num          = integer(),
                       SubTree_num       = integer(),
                       Node_num          = integer(),
                       Node_type         = character(),
                       Misclassified_num = integer(),
                       Samples_num       = integer(),
                       Depth             = integer(),
                       Object_id         = integer(),
                       Parent_id         = integer(),
                       NodeLeftorRight   = character(),
                       r_t               = double(),
                       p_t               = double(),
                       R_t               = double(),
                       cpp_node_children_left_NA   = logical(),
                       cpp_node_children_right_NA  = logical(),
                       cpp_node_using_householder  = logical(),
                       cpp_node_feature_index      = integer(),
                       cpp_node_threshold          = double(),
                       cpp_node_child_left         = integer(),
                       cpp_node_child_right        = integer(),
                       cpp_node_predicted_class    = integer(),
                       cpp_node_householder_matrix = vector()),
                  class             = "data.frame")
  x <- c("Tree_num",
         "SubTree",
         "Node_num",
         "Node_type",
         "Misclassified_num",
         "Samples_num",
         "Depth",
         "Object_id",
         "Parent_id",
         "Node_Left_Right",
         "r_t",
         "p_t",
         "R_t",
         "cpp_node_children_left_NA",
         "cpp_node_children_right_NA",
         "cpp_node_using_householder",
         "cpp_node_feature_index",
         "cpp_node_threshold",
         "cpp_node_child_left",
         "cpp_node_child_right",
         "cpp_node_predicted_class",
         "cpp_node_householder_matrix")
  colnames(df) <- x
  df <- convert_tree_to_dataframe(list_trees, df)
  pkg.env$using_cpp <- TRUE
  #
  hh <- df[,22]
  num.el <- sapply(hh, length)
  # will get a length of 0 for NULL and length of 1 for TRUE/FALSE
  indx <- which(num.el == 1 | num.el == 0)
  veclen <- max(num.el)
  hh[indx] <- list(rep(-999, veclen))
  mhh <- matrix(unlist(hh), ncol = veclen, byrow = TRUE)
  res <- list(df[, c(14:21, 8)], mhh)
  return(res)
}

pad_to_max_length <- function(x, maxlength){
  # already at max length?
  if(length(x) == maxlength){
    # yes, so nothing to do...
    return(x)
  }
  # calculate pad length.
  padlength = maxlength - length(x)
  padvals = list(rep(-999, padlength))
  # add our required number of pad values.
  x = c(x, padvals)
  return(x)
}
