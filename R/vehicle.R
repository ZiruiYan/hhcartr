#' Statlog (Vehicle Silhouettes) Dataset.
#'
#' The purpose is to classify a given silhouette as one of four types of vehicle, using a set of
#' features extracted from the silhouette. The vehicle may be viewed from one of many different
#' angles.
#'
#'
#' @docType data
#'
#' @usage data(vehicle)
#'
#' @format A data frame with 846 rows and 19 variables:
#' \describe{
#'     \item{COMPACTNESS}{(average perim)**2/area.}
#'     \item{CIRCULARITY}{(average radius)**2/area.}
#'     \item{DISTANCE CIRCULARITY}{area/(av.distance from border)**2.}
#'     \item{RADIUS RATIO}{(max.rad-min.rad)/av.radius.}
#'     \item{PR.AXIS ASPECT RATIO}{(minor axis)/(major axis).}
#'     \item{MAX.LENGTH ASPECT RATIO}{(length perp. max length)/(max length).}
#'     \item{SCATTER RATIO}{(inertia about minor axis)/(inertia about major axis).}
#'     \item{ELONGATEDNESS}{area/(shrink width)**2.}
#'     \item{PR.AXIS RECTANGULARITY}{area/(pr.axis length*pr.axis width).}
#'     \item{MAX.LENGTH RECTANGULARITY}{area/(max.length*length perp. to this).}
#'     \item{SCALED VARIANCE ALONG MAJOR AXIS}{(2nd order moment about minor axis)/area.}
#'     \item{SCALED VARIANCE ALONG MINOR AXIS}{(2nd order moment about major axis)/area.}
#'     \item{SCALED RADIUS OF GYRATION}{(mavar+mivar)/area.}
#'     \item{SKEWNESS ABOUT MAJOR AXIS}{(3rd order moment about major axis)/sigma_min**3.}
#'     \item{SKEWNESS ABOUT MINOR AXIS}{(3rd order moment about minor axis)/sigma_maj**3.}
#'     \item{KURTOSIS ABOUT MINOR AXIS}{(4th order moment about major axis)/sigma_min**4.}
#'     \item{KURTOSIS ABOUT MAJOR AXIS}{(4th order moment about minor axis)/sigma_maj**4.}
#'     \item{HOLLOWS RATIO}{(area of hollows)/(area of bounding polygon).}
#'     \item{class}{4 classes, OPEL, SAAB, BUS, VAN.}
#' }
#'
#' @keywords datasets vehicle
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/statlog+(vehicle+silhouettes)}
"vehicle"
