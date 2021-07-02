# source: hhcartr_export_setDataDescription.R

###########################################################################################
#' setDataDescription Save brief description of the dataset used by current model.
#'
#' This function setDataDescription updates global variable pkg.env$model_data_description
#' with a brief description of the dataset being used to train the current model. It is
#' later used in command output displays and plots.
#'
#' @param new_description the new dataset description to be saved for the current model.
#'
#' @return nothing.
#'
#' @example man/examples/setDataDescription.R
#'
#' @export
setDataDescription <- function(new_description){
  pkg.env$model_data_description <- new_description
}
