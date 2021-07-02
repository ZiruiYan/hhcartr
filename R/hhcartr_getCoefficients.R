# Source: hhcartr_getCoefficients.R

#########################################################################################
#'
#' getCoefficients Get the coefficents from the house holder matrix for the selected node.
#'
#' This function getCoefficients() returns the coefficients from the appropriate column
#' of the house holder matrix for the selected internal node or for all internal nodes if
#' no parameter was specified.
#'
#' @param n_node The internal node number the user wants coefficients for. If no node
#' number is supplied then coefficients for all internal nodes of the current tree are
#' returned.
#' @param fold The fold of the corresponding tree we want coefficients for.
#' @return A dataframe containing the requested information. NA is returned if the requested
#' node was either not found or was a leaf node.
#'

getCoefficients <- function(n_node, fold){
  # at least one parameter must be specified.
  if((is.na(n_node)) & (is.na(fold))){
    stop("hhcartr(getCoefficients) one or both the n_node and fold parameters must be specified.")
  }
  # allres is a list() of all induced trees.
  allres <- pkg.env$folds_trees
  num_trees <- length(allres)
  # ensure allres contains at least one tree before proceeding.
  if(num_trees == 0){
    stop("hhcartr(getCoefficients) no trees found. Run fit() first.")
  }
  # validate fold parameter value.
  if(!is.na(fold)){
    # validate tree number aka fold parameter.
    checkmate::assertInt(fold, lower = 1, upper = num_trees)
  }

  # if only a node number is given and no fold, if num_trees=1 then ok, fold=num_trees
  if((!is.na(n_node)) & (is.na(fold))){
    if(num_trees == 1){
      fold <- num_trees
    } else {
      stop("hhcartr(getCoefficients) no fold specified when more than one tree.")
    }
  }

  # get tree for selected fold.
  fold_tree <- allres[[fold]]
  # need to convert tree to df.
  res               <- convert_list_trees(list(fold_tree))
  list_trees_matrix <- res[[1]]
  hh                <- res[[2]]

  df <- cbind(list_trees_matrix$Object_id, hh, list_trees_matrix$cpp_node_using_householder)
  features <- c("Node", sprintf("x%d", seq(1,ncol(hh))), "UseHH")
  colnames(df) <- features
  df <- as.data.frame(df)
  # select only internal nodes
  df <- df[!is.na(df[, "UseHH"]),]
  df <- df[,-ncol(df)]
  # have the tree we want, now check if we want all coefficients or for a specific node
  if(!is.na(n_node)){
    # validate n_node parameter.
    checkmate::assertInt(n_node, lower = 0, upper = 999999)
    # return coefficients for selected node.
    df <- df[df[, "Node"] == n_node,]
    if(nrow(df) == 0){
      msgs <- "Node %s is not an internal node. Only internal nodes supported."
      msg <- sprintf(msgs, n_node)
      message(msg)
      df <- NA
    }
  }
  return(df)
}
