#' Balance Scale Dataset.
#'
#' This data set was generated to model psychological experimental results. Each
#' example is classified as having the balance scale tip to the right, tip to the left,
#' or be balanced. The attributes are the left weight, the left distance, the right
#' weight, and the right distance. The correct way to find the class is the greater of
#' (left-distance x left-weight) and (right-distance x right-weight). If they are equal,
#' it is balanced.
#'
#' @docType data
#'
#' @usage data(balance)
#'
#' @format A data frame with 625 rows and 4 variables:
#' \describe{
#'   \item{Left-Weight}{Left-Weight, one of 1, 2, 3, 4, or 5}
#'   \item{Left-Distance}{Left-Distance, one of 1, 2, 3, 4, or 5}
#'   \item{Right-Weight}{Right-Weight, one of 1, 2, 3, 4, or 5}
#'   \item{Right-Distance}{Right-Distance, one of 1, 2, 3, 4, or 5}
#'   \item{Class Name}{Class Name: one of L, B or R)}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Balance+Scale}
"balance"
