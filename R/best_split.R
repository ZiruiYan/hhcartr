# source: best_split.R

###################################################################################################
#' best_split_ finds the best feature column to split on.
#'
#' This internal function is used to find the feature column that will offer the best
#' split based on using the Gini index or gini hyperplane index.
#'
#' @param X feature variables to search for the best split.
#' @param y target variable.
#' @param most_freq_class the most frequent class in the target variable.
#' @param split_original boolean to indicate whether to split on original data or reflected data.
#' @param n_classes number of classes in the y column
#' @param max_features the maximum number of features to use when splitting a node
#' @param depth the depth of the current tree.
#' @return a list of the following variables (best_idx, best_thr, best_gini)
#'

best_split_ <- function(X,
                        y,
                        most_freq_class,
                        split_original,
                        n_classes,
                        max_features,
                        depth){
  debug_msg <- FALSE

  if(typeof(y) == "list"){
    m <- dim(y)[1]
  } else{
    m <- length(y)
  }

  if(m<1){
    return(NULL, NULL, NULL)
  }

  # initialise some variables.
  num_parent <- as.vector(data.frame(table(y))$Freq)
  best_gini  <- Inf
  best_idx   <- Inf
  best_thr   <- Inf
  best_left  <- NULL
  best_right <- NULL

  # get index of the most frequent class ie. Matrix A class.
  matrix_a_idx <- which(levels(as.factor(y)) == most_freq_class)

  # number of feature columns
  n_features_ <- dim(X)[2]

  # here we want to check if we are doing random columns
  # i.e.RandomForest. If so, we will also need to know
  # how many feature columns to select randomly.
  initial_feature_list <- 1:n_features_
  if(length(initial_feature_list) < max_features){
    max_features <- length(initial_feature_list)
  }
  look_at_these_features <- sample(initial_feature_list, max_features, replace = FALSE)
  look_at_these_features <- sort(look_at_these_features, decreasing = FALSE)

  for(k in 1:n_features_){
    saved_look_at_these_features <- look_at_these_features
    if(isTRUE(all.equal(unname(X[,k]), rep(max(X[,k]), length(X[,k]))))){
      look_at_these_features <- look_at_these_features[!look_at_these_features %in% k]
      if(debug_msg){
        msg  <- "best_split_() skipping column %s as all values are the same."
        msgs <- sprintf(msg, k)
        message(msgs)
      }
    }
  }
  if(length(look_at_these_features) == 0){
    look_at_these_features <- initial_feature_list[initial_feature_list %notin% saved_look_at_these_features]
    if(debug_msg){
      msg  <- "best_split_() selected features all same, using remainder...%s."
      msgs <- sprintf(msg, look_at_these_features)
      message(msgs)
    }
  }

  for(idx in look_at_these_features){
    # check if all values in a column are the same, if so skip that column
    Xy        <- cbind(X[,idx], y)
    Xy        <- Xy[order(-Xy[,1]),]
    threshold <- Xy[,1]
    classes   <- Xy[,2]
    if(is.character(classes)){
      classes <- as.factor(classes)
    }

    num_parent <- rep(0, n_classes)
    ddd        <- data.frame(table(y))
    for(t in 1:nrow(ddd)){
      num_parent[ddd[t,]$y] <- ddd[t,]$Freq
    }

    num_left <- rep(0, n_classes)
    num_right <- num_parent
    limit <- m #-1
    for(i in (2:limit)){
      # classes holds the y variable
      dd <- classes[i-1]
      cc <- as.integer(dd)
      num_left[cc] <- num_left[cc] + 1
      num_right[cc] <- num_right[cc] - 1

      if(split_original){
        # calculate gini-index when we split on original feature space
        L          <- sum(num_left) / (sum(num_left) + sum(num_right))
        gini_right <- (1 - L) * (sum(num_right / m * (1 - num_right / m)))
        gini_left  <- L * (sum(num_left / m * (1 - num_left / m)))
        gini       <- gini_left + gini_right
      } else {
        # calculate hyperplane gini index for current split
        L          <- sum(num_left) / limit
        L_A        <- num_left[matrix_a_idx] / limit
        R_A        <- num_right[matrix_a_idx] / limit
        gini       <- (2 * L * ((1 - L_A) * L_A)) + (2 * (1 - L) * (1 - R_A) * R_A)
      }

      # check if we have found a new best split, if so save feature index, gini, threshold
      # get better results when only update when get improvement only ie. < rather than <=
      if(gini <= best_gini){
        best_gini  <- gini
        best_idx   <- idx
        best_left  <- num_left
        best_right <- num_right
        if(is.character(threshold)){
          best_thr  <- (as.numeric(as.character(threshold[i])) + as.numeric(as.character(threshold[i-1]))) / 2
        } else {
          best_thr  <- (threshold[i] + threshold[i-1]) / 2
        }
      }

      if(threshold[i] == threshold[i-1]){
        # if threshold all same values make sure we
        # update best_idx and best_thr at least once.
        if(!is.null(best_idx) & !is.null(best_thr)){
          next
        }
      }
    }
  }
  # never happens but check anyway!
  if(is.null(best_idx)){
    stop("hhcartr(best_split_) best_idx = NULL")
  }
  results <- list(best_idx, best_thr, best_gini)
  return(results)
}
