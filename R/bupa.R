#' Liver Disorders Dataset.
#'
#' The first 5 variables are all blood tests which are thought to be sensitive to liver
#' disorders that might arise from excessive alcohol consumption. Each line in the dataset
#' constitutes the record of a single male individual.
#'
#' @docType data
#'
#' @usage data(bupa)
#'
#' @format A data frame with 345 rows and 7 variables:
#' \describe{
#'     \item{mcv}{mean corpuscular volume}
#'     \item{alkphos}{alkaline phosphotase}
#'     \item{sgpt}{alanine aminotransferase}
#'     \item{sgot}{aspartate aminotransferase}
#'     \item{gammagt}{gamma-glutamyl transpeptidase}
#'     \item{drinks}{number of half-pint equivalents of alcoholic beverages drunk per day}
#'     \item{selector}{field created by the BUPA researchers to split the data into train/test sets}
#' }
#'
#' @keywords datasets bupa liver disorder liver
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Letter+Recognition}
"bupa"
