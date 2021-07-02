#' Letter Recognition Dataset.
#'
#' The objective is to identify each of a large number of black-and-white rectangular pixel
#' displays as one of the 26 capital letters in the English alphabet. The character images
#' were based on 20 different fonts and each letter within these 20 fonts was randomly
#' distorted to produce a file of 20,000 unique stimuli. Each stimulus was converted into 16
#' primitive numerical attributes (statistical moments and edge counts) which were then
#' scaled to fit into a range of integer values from 0 through 15. We typically train on the
#' first 16000 items and then use the resulting model to predict the letter category for the
#' remaining 4000. See the article cited above for more details.
#'
#' @docType data
#'
#' @usage data(letters)
#'
#' @format A data frame with 20,000 rows and 16 variables:
#' \describe{
#'     \item{x-box}{horizontal position of box (integer)}
#'     \item{y-box}{vertical position of box (integer)}
#'     \item{width}{width of box (integer)}
#'     \item{high}{height of box (integer)}
#'     \item{onpix}{total # on pixels (integer)}
#'     \item{x-bar}{mean x of on pixels in box (integer)}
#'     \item{y-bar}{mean y of on pixels in box (integer)}
#'     \item{x2bar}{mean x variance (integer)}
#'     \item{y2bar}{mean y variance (integer)}
#'     \item{xybar}{mean x y correlation (integer)}
#'     \item{x2ybr}{mean of x * x * y (integer)}
#'     \item{xy2br}{mean of x * y * y (integer)}
#'     \item{x-ege}{mean edge count left to right (integer)}
#'     \item{xegvy}{correlation of x-ege with y (integer)}
#'     \item{y-ege}{mean edge count bottom to top (integer)}
#'     \item{yegvx}{correlation of y-ege with x (integer)}
#'     \item{lettr}{(class attribute) capital letter (26 values from A to Z)}
#' }
#'
#' @keywords datasets letter letters
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Letter+Recognition}
"letters"
