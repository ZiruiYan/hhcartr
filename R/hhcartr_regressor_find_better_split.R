# source: hhcartr_regressor_find_better_split.R

###################################################################################################
#' hhcartr_regressor_find_better_split finds the best feature to split on.
#'
#' This internal function is used by the regressor model to find the feature column that will offer
#' the best split based on sum of squares.
#'
#' @param X feature variables to search for the best split.
#' @param y target variable.
#' @param max_features the maximum number of features to use when splitting a node
#' @return a list of the following variables (var_idx_, split_, score_)
#'

hhcartr_regressor_find_better_split <- function(X, y, max_features){
  n_features_   <- dim(X)[2]
  n_            <- dim(X)[1]
  min_leaf_     <- pkg.env$n_min
  score_        <- Inf
  split_        <- Inf
  var_idx_      <- Inf

  for(idx in (1:n_features_)){
    Xy          <- cbind(X[,idx], y)
    Xy          <- Xy[order(-Xy[,1]),]

    rhs_cnt     <- nrow(Xy)
    rhs_sum     <- sum(Xy[,2])      # sum column 2
    rhs_sumsq   <- sum((Xy[,2])^2)  # sum of squares column 2

    lhs_cnt     <- 0
    lhs_sum     <- 0.0
    lhs_sumsq   <- 0.0

    looplim     <- n_ - 1

    for(i in (1:looplim)){
      xi        <- Xy[i, 1]
      yi        <- as.numeric(Xy[i, 2])
      lhs_cnt   <- lhs_cnt + 1
      rhs_cnt   <- rhs_cnt - 1
      lhs_sum   <- lhs_sum + yi
      rhs_sum   <- rhs_sum - yi
      lhs_sumsq <- lhs_sumsq + (yi^2)
      rhs_sumsq <- rhs_sumsq - (yi^2)

      if(i < min_leaf_ | xi == Xy[i + 1, 1]){
        next
      }

      lhs_std <- std_dev(lhs_cnt, lhs_sum, lhs_sumsq)
      rhs_std <- std_dev(rhs_cnt, rhs_sum, rhs_sumsq)
      curr_score <- (lhs_std * lhs_cnt) + (rhs_std * rhs_cnt)
      if(curr_score < score_){
        var_idx_ <- idx
        score_   <- curr_score
        split_   <- xi
      }
    }
  } # end for
  results <- list(var_idx_, split_, score_)
  return(results)
}

std_dev <- function(cnt, sum_, sumsq_){
  lr_diff <- (sumsq_ / cnt) - (sum_ / cnt)^2
  if(lr_diff <= 0.0){ return(lr_diff) }
  return(sqrt(lr_diff))
}
