# source: hhcartr_perform_ccp.R

extract_node_info <- function(i, root_node, subtree_count, node_count, df, node_bucket, subtreemode, nodeleftorright){

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

  # see if we an find the parent node
  if(typeof(root_node$node_parent) == "S4"){
    newrow$Parent_id <- root_node$node_parent$node_objectid
  }
  df <- rbind(df, newrow)

  # does this node have a left child node?
  if(!root_node$node_children_left_NA){
    ln <- root_node$node_children_left
    results <- extract_node_info(i,
                                 ln,
                                 subtree_count,
                                 node_count,
                                 df,
                                 node_bucket,
                                 FALSE,
                                 "L")
    node_count <- results[[1]]
    df <- results[[2]]
    node_bucket <- results[[3]]
    if(ln$node_type == "INTERNAL" & !ln$node_processed_as_subtree & !subtreemode){
      ln$node_processed_as_subtree <- TRUE
      node_bucket <- append(node_bucket, ln)
    }
  } else {
    outp <- list(node_count, df, node_bucket)
    return(outp)
  }
  # does this node have a right child node?
  if(!root_node$node_children_right_NA){
    rn <- root_node$node_children_right
    results <- extract_node_info(i,
                                 rn,
                                 subtree_count,
                                 node_count,
                                 df,
                                 node_bucket,
                                 FALSE,
                                 "R")
    node_count <- results[[1]]
    df <- results[[2]]
    node_bucket <- results[[3]]
    if(rn$node_type == "INTERNAL" & !rn$node_processed_as_subtree & !subtreemode){
      rn$node_processed_as_subtree <- TRUE
      node_bucket <- append(node_bucket, rn)
    }
  } else {
    outp <- list(node_count, df, node_bucket)
    return(outp)
  }
  outp <- list(node_count, df, node_bucket)
  return(outp)
}

perform_cost_complexity_pruning <- function(treeobjs, df){
  loop_start <- 1
  loop_end   <- length(treeobjs)
  node_bucket <- list()

  # for each tree...
  for(i in loop_start:loop_end){
    subtree_count <- 1
    node_count <- 0
    root_node <- treeobjs[[i]]
    root_node$node_subtree_num <- subtree_count
    results <- extract_node_info(i,
                                  root_node,
                                  subtree_count,
                                  node_count,
                                  df,
                                  node_bucket,
                                  FALSE,
                                  "ROOT")
    node_count <- results[[1]]
    df <- results[[2]]
    node_bucket <- results[[3]]

    # process each subtree of tree
    if(length(node_bucket) == 0){
      return(df)
    }
    for(j in 1:length(node_bucket)){ ##-
      node_count <- 0 ##-
      subtree_count <- subtree_count + 1 ##-
      root_node <- node_bucket[[j]] ##-
      root_node$node_subtree_num <- subtree_count ##-
      results <- extract_node_info(i,
                                    root_node,
                                    subtree_count,
                                    node_count,
                                    df,
                                    node_bucket,
                                    TRUE,
                                    "SUBTREE_ROOT") ##-
      node_count <- results[[1]] ##-
      df <- results[[2]] ##-
    }
  }
  return(df)
}

#######################################################################################################
#' perform_ccp_driver.
#'
#' This function performs CCP pruning on the supplied tree.
#'
#' @param treeobjs The current HHCARTR tree that has just been induced contained
#' within a list() object. CCP pruning will be performed on this tree.
#'
#' @return A dataframe containing information for each subtree.
#'
perform_ccp_driver <- function(treeobjs){
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
                       R_t               = double()),
                       class             = "data.frame")
  # df column headings
  x <- c("Tree_num", "SubTree", "Node_num", "Node_type", "Misclassified_num",
         "Samples_num",
         "Depth", "Object_id", "Parent_id", "Node_Left_Right", "r_t", "p_t", "R_t")
  colnames(df) <- x
  # go perform the actual ccp.
  df <- perform_cost_complexity_pruning(treeobjs, df)

  # here want to calculate R(i) for each leaf i.e. TERMINAL nodes only.
  # df - holds all subtrees.
  df$R_i <- NA
  total_number_samples_in_tree <- df$Samples_num[1]
  # df$R_i only relevant for df$Node_type == "TERMINAL" i.e. leaf nodes.
  df$R_i <- df$Misclassified_num / total_number_samples_in_tree

  #
  alpha_list <- list()
  moretrees <- TRUE
  seekalpha_subtreesubset_list <- list()

  # preserve original input.
  base_trees <- df
  seekalpha_phase <- 1

  while(moretrees){
    df$R_T_t <- NA
    df$Num_Leaves <- NA
    df$alpha <- NA
    res <- seekalpha(df, base_trees)
    df <- res[[1]]

    if(typeof(df) == "logical"){
      moretree <- FALSE
      break
    }

    smallest_alpha <- res[[2]]
    subtree_with_smallest_alpha <- res[[3]]
    collapse_this_node_with_smallest_alpha <- res[[4]]
    number_internal_nodes_deleted <- res[[5]]
    total_number_nodes_deleted <- res[[6]]
    seekalpha_subtreesubset <- res[[7]]
    seekalpha_subtreesubset <- cbind(seekalpha_phase,
                                     seekalpha_subtreesubset)
    seekalpha_subtreesubset_list <- rbind(seekalpha_subtreesubset_list,
                                          seekalpha_subtreesubset)
    seekalpha_phase <- seekalpha_phase + 1
    alpha_list <- append(alpha_list,
                         list(smallest_alpha,
                              subtree_with_smallest_alpha,
                              collapse_this_node_with_smallest_alpha,
                              number_internal_nodes_deleted,
                              total_number_nodes_deleted))
  }

  # save phase data
  save_ccp_phase_data(seekalpha_subtreesubset_list)

  res <- NA
  if(length(alpha_list) == 0){
    empty_df <- base_trees[FALSE,]
    res <- list(empty_df)
  } else {
    alpha_list_df <- data.frame(matrix(unlist(alpha_list),
                                       nrow = length(alpha_list) / 5,
                                       byrow = T))
    colnames(alpha_list_df) <- c("alpha",
                                 "subtree_with_smallest_alpha",
                                 "collapse_this_node",
                                 "number_internal_nodes_deleted",
                                 "total_number_nodes_deleted")
    res <- list(alpha_list_df)
  }
  return(res)
}

compute_R_T_t <- function(df, subtree){
  R_i_for_subtree <- df[which(df$Node_type == "TERMINAL" & df$SubTree_num == subtree),]$R_i
  Number_leaves_on_subtree <- length(R_i_for_subtree)
  R_T_t <- sum(R_i_for_subtree)
  res <- list(R_T_t, Number_leaves_on_subtree)
  return(res)
}


seekalpha <- function(df, base_trees){
  # for each row
  for(i in 1:nrow(df)) {
    row <- df[i,]
    # do stuff with row
    if(row$Node_type == "INTERNAL" & row$SubTree_num != 1 & row$NodeLeftorRight == "SUBTREE_ROOT"){
      res <- compute_R_T_t(df, row$SubTree_num) # was base_trees instead of df.
      row$R_T_t <- res[[1]]
      row$Num_Leaves <- res[[2]]
      row$alpha <- (row$R_t - row$R_T_t) / (row$Num_Leaves - 1)
      df[i,] <- row
    }
  }

  # find all valid subtrees so we can compare their alpha values.
  # looking for the tree with the smallest alpha; in the event of a tie,
  # select the tree that removes the most leaves (reduces complexity the most).
  subtreesubset <- df[which(df$NodeLeftorRight == "SUBTREE_ROOT"),]
  if(nrow(subtreesubset) == 0){
    return(NA)
  }
  #
  smallest_alpha <- subtreesubset[which.min(subtreesubset$alpha),]
  # verify only one row
  if(nrow(smallest_alpha) > 1){
    browser()
  }

  #
  subtree_with_smallest_alpha <- smallest_alpha$SubTree_num
  collapse_this_node_with_smallest_alpha <- smallest_alpha$Object_id
  original_tree_df <- df[which(df$SubTree_num == 1),]
  collapse_this <- df[which(df$SubTree_num == subtree_with_smallest_alpha),]
  # without elements that are collapse_this_node_with_smallest_alpha
  remove_these <- collapse_this$Object_id[collapse_this$Object_id != collapse_this_node_with_smallest_alpha]
  #
  original_tree_df <- df
  # now collapse the node - collapse_this_node_with_smallest_alpha
  for(i in 1:nrow(original_tree_df)){
    rowi <- original_tree_df[i,]
    if(rowi$Object_id == collapse_this_node_with_smallest_alpha){
      if(rowi$Node_type == "INTERNAL"){
        # turn INTERNAL node into a leaf i.e. collapse the node
        original_tree_df[i,]$Node_type <- "TERMINAL"
        original_tree_df[i,]$NodeLeftorRight <- NA
      }
    }
  }

  new_df <- original_tree_df[which(original_tree_df$Object_id %notin% remove_these),]
  number_internal_nodes_deleted <- nrow(base_trees[which(base_trees$SubTree_num == subtree_with_smallest_alpha & base_trees$Node_type == "INTERNAL"),])
  number_terminal_nodes_deleted <- nrow(base_trees[which(base_trees$SubTree_num == subtree_with_smallest_alpha & base_trees$Node_type == "TERMINAL"),])
  total_number_nodes_deleted <- (number_internal_nodes_deleted - 1) + number_terminal_nodes_deleted

  # we can now repeat this process with new_df
  results <- list(new_df,
                  smallest_alpha$alpha,
                  subtree_with_smallest_alpha,
                  collapse_this_node_with_smallest_alpha,
                  number_internal_nodes_deleted,
                  total_number_nodes_deleted,
                  subtreesubset)
  return(results)
} # end seekalpha()
