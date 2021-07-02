#' Haberman's Survival Dataset.
#'
#' The dataset contains cases from a study that was conducted between 1958 and 1970 at the
#' University of Chicago's Billings Hospital on the survival of patients who had undergone
#' surgery for breast cancer.
#'
#' @docType data
#'
#' @usage data(survival)
#'
#' @format A data frame with 306 rows and 3 variables:
#' \describe{
#'   \item{Age}{Age of patient at time of operation (numerical)}
#'   \item{Year}{Patient's year of operation (year - 1900, numerical)}
#'   \item{Positive Nodes}{Number of positive axillary nodes detected (numerical)}
#'   \item{Survival status}{(class attribute) -- 1 = the patient survived 5 years or longer
#'   -- 2 = the patient died within 5 year}
#' }
#'
#' @keywords datasets survival haberman
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Haberman's+Survival}
"survival"
