#' Boston Housing dataset.
#'
#' Concerns housing values in suburbs of Boston.
#'
#' For regression models, build models to predict the MEDV field. For classification models
#' hhcartr provides a binary target variable calculated using MEDV > 20 which can then be
#' used to build a classification model.
#'
#' @docType data
#'
#' @usage data(housing)
#'
#' @format A data frame with 506 rows and 13 variables:
#' \describe{
#'   \item{CRIM}{per capita crime rate by town}
#'   \item{ZN}{proportion of residential land zoned for lots over 25,000 sq.ft.}
#'   \item{INDUS}{proportion of non-retail business acres per town}
#'   \item{CHAS}{Charles River dummy variable (= 1 if tract bounds river, 0 otherwise)}
#'   \item{NOX}{nitric oxides concentration (parts per 10 million)}
#'   \item{RM}{average number of rooms per dwelling}
#'   \item{AGE}{proportion of owner-occupied units built prior to 1940}
#'   \item{DIS}{weighted distances to five Boston employment centres}
#'   \item{RAD}{index of accessibility to radial highways}
#'   \item{TAX}{full-value property-tax rate per $10,000}
#'   \item{PTRATIO}{pupil-teacher ratio by town}
#'   \item{B}{1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town}
#'   \item{LSTAT}{percentage lower status of the population}
#'   \item{MEDV}{Median value of owner-occupied homes in $1000s}
#' }
#'
#' @keywords datasets boston housing
#'
#' @source \url{http://archive.ics.uci.edu/ml/machine-learning-databases/housing/}
"housing"
