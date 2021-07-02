# source: hhcartr_reflect_feature_space.R

#######################################################################################################
#'
#' hhcart_reflect_feature_space_g
#'
#' This function is in an internal only function. It is used to reflect the feature data if the
#' data is found to be suitable, otherwise node splitting will be done using the original data
#' and using axis-parallel splits.
#'
#' The following parameters are supported:
#'
#' X_matrix, X, y, most_freq_class, n_classes, max_features, n_features, colx
#'
#' @param X The feature variables of the current node being processed.
#' @param y The target variable for the corresponding feature variables.
#' @param useIdentity A flag, if FALSE the node split will be done using an oblique split
#' after transforming the training data using a householder transform. If TRUE the 'transform'
#' will be the identity and the split will be done on the original data.
#' @param max_features The maximum number of features to consider in the current split.
#' @param n_features The number of feature variables.
#' @param n_classes The number of classes in the target variable.
#' @param classify A flag, if TRUE this is a classification problem, if FALSE this is a regression
#' problem.
#' @param depth The current tree depth.
#' @param colx The eigenvector column to use.
#'
#' @return Returns a list containing idx, thr, X_house, householder_matrix, using_householder if split
#'         was for a classification problem and reflected data used, otherwise the results from
#'         split_using_original_data are returned if useIdentity was TRUE. For a regression problem
#'         a list containing the following is returned: idx, thr, as.matrix(X), NULL, FALSE.
#'
hhcart_reflect_feature_space_g <- function(X,
                                           y,
                                           useIdentity  = FALSE,
                                           max_features = "None",
                                           n_features   = n_features,
                                           n_classes    = n_classes,
                                           classify     = TRUE,
                                           depth        = depth,
                                           colx         = 1){

  debug_msg <- FALSE

  # if using a regressor can't use HH matrices, so split on raw data.
  if(!classify){
    results      <- hhcartr_regressor_find_better_split(X, y, max_features)
    idx          <- as.numeric(results[1])
    thr          <- as.numeric(results[2])
    func_results <- list(idx, thr, as.matrix(X), NULL, FALSE)
    return (func_results)
  }

  # find the most frequent class
  most_freq_class <- names(which.max(table(y)))
  # more than one most frequent class, if so select one at random
  if(length(most_freq_class)>1){
    # generate random integer between 1 and length(most_freq_class)
    most_freq_class <- sample(1:length(most_freq_class), 1)
  }
  # find indices of the training samples that match the most frequent class and those that don't
  if(typeof(y)=='list'){
    a_idx <- which(y$y %in% c(most_freq_class))
    b_idx <- which(y$y %notin% c(most_freq_class))
  } else {
    a_idx <- which(y %in% most_freq_class)
    b_idx <- which(y %notin% most_freq_class)
  }
  # make sure nothing has gone wrong with index creation
  if(length(a_idx)==0 | length(b_idx)==0){
    stop("hhcartr(hhcart_reflect_feature_space) a_idx or b_idx must never be ZERO.")
  }
  # create the A and B matrices
  A_matrix <- X[a_idx,]
  B_matrix <- X[b_idx,]
  # initialise needed variables
  newH_A   <- NA
  X_houseA <- NA
  newH_B   <- NA
  X_houseB <- NA
  gidxA    <- NA
  gidxB    <- NA

  # if using identity, just split using the original data
  if(useIdentity){
    res <- split_using_original_data(X, y, most_freq_class, TRUE, n_classes = n_classes, max_features = max_features, depth = depth)
    return(res)
  }

  # A_matrix must have more than 1 row for cov
  if (length(a_idx)>1){
    results1 <- reflect_feature_space(A_matrix, X, y, most_freq_class, n_classes, max_features, n_features, depth, colx)
    if(length(results1)==1){return(results1)}
    idxA     <- as.numeric(results1[1])
    thrA     <- as.numeric(results1[2])
    gidxA    <- as.numeric(results1[3])
    X_houseA <- results1[[4]]
    newH_A   <- results1[[5]]
  }

  # B_matrix must have more than 1 column for cov
  if (length(b_idx)>1){
    results1 <- reflect_feature_space(B_matrix, X, y, most_freq_class, n_classes, max_features, n_features, depth, colx)
    if(length(results1)==1){return(results1)}
    idxB     <- as.numeric(results1[1])
    thrB     <- as.numeric(results1[2])
    gidxB    <- as.numeric(results1[3])
    X_houseB <- results1[[4]]
    newH_B   <- results1[[5]]
  }

  # if we are here, a_idx==b_idx==1 should never happen as we should not get called
  # when number of rows in X is two, but when we can get situation of 1 sample in either
  # the A or B matrix.
  if(length(a_idx) == 1){
    func_results <- list(idxB, thrB, X_houseB, newH_B[,idxB], TRUE)
    return (func_results)
  }
  if(length(b_idx) == 1){
    func_results <- list(idxA, thrA, X_houseA, newH_A[,idxA], TRUE)
    return (func_results)
  }

  if(gidxA <= gidxB){
    householder_matrix <- newH_A
    X_house            <- X_houseA
    idx                <- idxA
    thr                <- thrA
    using_householder  <- TRUE
  } else {
    householder_matrix <- newH_B
    X_house            <- X_houseB
    idx                <- idxB
    thr                <- thrB
    using_householder  <- TRUE
  }

  # save reflected X data to a csv
  # filename format: Sys.time()_X_house_depth.csv
  #soutput_filename <- "./reflected_data/%s_X_house_%s.csv"
  #output_filename  <- sprintf(soutput_filename, Sys.time(), depth)
  #output_filename <- chartr(old = ":", new = "-", output_filename)
  #output_filename <- chartr(old = " ", new = "-", output_filename)
  #write.csv(cbind(X_house, y), output_filename)
  #
  func_results <- list(idx, thr, X_house, householder_matrix[,idx], using_householder)
  return (func_results)
} # end hhcart_reflect_feature_space_g()

#######################################################################################################
#'
#' split_using_original_data
#'
#' This function is in an internal only function. It performs an axis-parallel split on the original
#' data.
#'
#' The following parameters are supported:
#'
#' @param X The feature variables of the current node being processed.
#' @param y The target variable for the corresponding feature variables.
#' @param most_freq_class The most frequently occurring class at the current node.
#' @param split_original A flag, TRUE if splitting on original data otherwise FALSE.
#' @param n_classes The number of classes in the target variable.
#' @param max_features The maximum number of features to consider in the current split.
#' @param depth The depth of the current tree.
#'
#' @return returns a list containing the following: idx, thr, X, NULL, FALSE.
#'
split_using_original_data <- function(X, y, most_freq_class, split_original, n_classes, max_features, depth){
  # just use original data
  X_house            <- as.matrix(X)
  results            <- best_split_(X_house, y,
                                    most_freq_class,
                                    split_original,
                                    n_classes    = n_classes,
                                    max_features = max_features,
                                    depth        = depth)
  idx                <- as.numeric(results[1])
  thr                <- as.numeric(results[2])
  householder_matrix <- NULL
  using_householder  <- FALSE
  func_results       <- list(idx, thr, X_house, householder_matrix, using_householder)
  return (func_results)
}

#######################################################################################################
#'
#' reflect_feature_space
#'
#' This function is in an internal only function. It applies the householder transformation
#' to the feature data.
#'
#' The following parameters are supported:
#'
#' @param X_matrix The current node to be tested for a split.
#' @param X The feature variables of the current node being processed.
#' @param y The target variable for the corresponding feature variables.
#' @param most_freq_class The most frequently occurring class at the current node.
#' @param n_classes The number of classes in the target variable.
#' @param max_features The maximum number of features to consider in the current split.
#' @param n_features The number of feature variables.
#' @param depth The depth of the current tree.
#' @param colx The eigenvector column to use.
#'
#' @return Returns idxA, thrA, gidxA, X_houseA, newH_A if split is on reflected data, otherwise
#'         a list containing idx, thr, X, NULL, FALSE is returned when the split is made using the
#'         original data when the first eigen vector equals the first column of the identity matrix.
#'
reflect_feature_space <- function(X_matrix, X, y, most_freq_class, n_classes, max_features, n_features, depth, colx){

  X_matrix_subset <- X_matrix

  # method = c("pearson", "kendall", "spearman") default is "pearson".
  cov_mat1 <- cov(X_matrix_subset, method = "pearson")
  ev1 <- eigen(cov_mat1)
  # eig_vals1 not used as yet.
  #eig_vals1 <- ev1$values
  eig_vecs1 <- ev1$vectors
  #colx <- which.max(eig_vals1)
  mu1 <- eig_vecs1[, colx]
  # eigen_vectors not used as yet.
  #eigen_vectors <- eig_vecs1
  #
  I_A <- diag(n_features)
  e_A <- I_A[, 1]
  if(all(mu1 == e_A)){
    # split on original data and exit
    res <- split_using_original_data(X,
                                     y,
                                     most_freq_class,
                                     TRUE,
                                     n_classes = n_classes,
                                     max_features = max_features)
    reslist <- list(res)
    return(reslist)

  } else if(length(mu1) > 1){
    uA_          <- (e_A - mu1)
    uA           <- (uA_ %*% t(uA_))/(norm(as.matrix(uA_), "2")^2)
    newH_A       <- I_A - (2 * uA)
  } else if(is.na(mu1)){
    stop("hhcartr(hhcart_reflect_feature_space) mu1 is NA.")
  } else {
    message(mu1)
    stop("hhcartr(hhcart_reflect_feature_space) mu1 in unknown state.")
  }
  #
  X_houseA <- as.matrix(X) %*% t(newH_A)
  results  <- best_split_(X_houseA, y,
                          most_freq_class,
                          FALSE,
                          n_classes    = n_classes,
                          max_features = max_features,
                          depth        = depth)
  idxA     <- as.numeric(results[1])
  thrA     <- as.numeric(results[2])
  gidxA    <- as.numeric(results[3])
  reslist  <- list(idxA, thrA, gidxA, X_houseA, newH_A)
  return(reslist)
}
