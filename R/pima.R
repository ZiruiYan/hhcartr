#' PIMA Indians diabetes dataset.
#'
#' This dataset is originally from the National Institute of Diabetes and Digestive and
#' Kidney Diseases. The objective of the dataset is to diagnostically predict whether or not
#' a patient has diabetes, based on certain diagnostic measurements included in the dataset.
#' Several constraints were placed on the selection of these instances from a larger database.
#' In particular, all patients here are females at least 21 years old of Pima Indian heritage.
#'
#' The datasets consists of several medical predictor variables and one target variable,
#' Outcome. Predictor variables includes the number of pregnancies the patient has had, their
#' BMI, insulin level, age, and so on.
#'
#' @docType data
#'
#' @usage data(pima)
#'
#' @format A data frame with 768 rows and 8 variables:
#' \describe{
#'   \item{Pregnancies}{Number of times pregnant}
#'   \item{Glucose}{Plasma glucose concentration a 2 hours in an oral glucose tolerance test}
#'   \item{BloodPressure}{Diastolic blood pressure (mm Hg)}
#'   \item{SkinThickness}{Triceps skin fold thickness (mm)}
#'   \item{Insulin}{2-Hour serum insulin (mu U/ml)}
#'   \item{BMI}{Body mass index (weight in kg/(height in m)^2)}
#'   \item{DiabetesPedigreeFunction}{Diabetes pedigree function}
#'   \item{Age}{Age (years)}
#'   \item{Outcome}{Outcome: target variable, whether patient had diabetes, 268 of 768 are 1, the others are 0 (1 = yes; 0 = no)}
#' }
#'
#' @keywords datasets pima diabetes
#'
#' @source \url{https://www.kaggle.com/uciml/pima-indians-diabetes-database}
"pima"
