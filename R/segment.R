#' Image Segmentation Dataset.
#'
#' The instances were drawn randomly from a database of 7 outdoor images. The images were
#' hand segmented to create a classification for every pixel. Each instance is a 3x3 region.
#'
#'
#' @docType data
#'
#' @usage data(segment)
#'
#' @format A data frame with 2310 rows and 19 variables:
#' \describe{
#'     \item{region-centroid-col}{the column of the center pixel of the region.}
#'     \item{region-centroid-row}{the row of the center pixel of the region.}
#'     \item{region-pixel-count}{the number of pixels in a region = 9.}
#'     \item{short-line-density-5}{ the results of a line extractoin algorithm that counts how many
#'      lines of length 5 (any orientation) with low contrast, less than or equal to 5, go through
#'      the region.}
#'     \item{short-line-density-2}{same as short-line-density-5 but counts lines of high contrast,
#'     greater than 5.}
#'     \item{vedge-mean}{measure the contrast of horizontally adjacent pixels in the region. There
#'     are 6, the mean and standard deviation are given. This attribute is used as a vertical edge
#'     detector.}
#'     \item{vegde-sd}{measure the contrast of horizontally adjacent pixels in the region. There
#'     are 6, the mean and standard deviation are given. This attribute is used as a vertical edge
#'     detector.}
#'     \item{hedge-mean}{measures the contrast of vertically adjacent pixels. Used for horizontal
#'     line detection.}
#'     \item{hedge-sd}{measures the contrast of vertically adjacent pixels. Used for horizontal
#'     line detection.}
#'     \item{intensity-mean}{the average over the region of (R + G + B)/3.}
#'     \item{rawred-mean}{the average over the region of the R value.}
#'     \item{rawblue-mean}{the average over the region of the B value.}
#'     \item{rawgreen-mean}{the average over the region of the G value.}
#'     \item{exred-mean}{measure the excess red: (2R - (G + B)).}
#'     \item{exblue-mean}{measure the excess blue: (2B - (G + R)).}
#'     \item{exgreen-mean}{measure the excess green: (2G - (R + B)).}
#'     \item{value-mean}{3-d nonlinear transformation of RGB. (Algorithm can be found in
#'     Foley and VanDam, Fundamentals of Interactive Computer Graphics).}
#'     \item{saturatoin-mean}{3-d nonlinear transformation of RGB. (Algorithm can be found in
#'     Foley and VanDam, Fundamentals of Interactive Computer Graphics).}
#'     \item{hue-mean}{3-d nonlinear transformation of RGB. (Algorithm can be found in
#'     Foley and VanDam, Fundamentals of Interactive Computer Graphics).}
#' }
#'
#' @keywords datasets segment
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/image+segmentation}
"segment"
