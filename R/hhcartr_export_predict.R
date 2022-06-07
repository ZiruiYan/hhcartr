# source: hhcartr_export_predict.R

#################################################################################################
#'
#' predict - Create generic S3method to make predictions via predict.hhcartr.
#' Needs export entry in the NAMESPACE file.
#'
#' This function creates a generic S3method predict which is used to call predict.hhcartr when
#' an object of type hhcartr passed to the predict function, i.e. an object that is returned
#' from the fit() function. The object created from the predict function supports the accuracy and
#' predictions methods. The accuracy method returns the accuracy achieved on the test_data and the
#' method predictions returns the actual predictions made on the test_data.
#'
#' @param object Unused parameter.
#' @param ... Unused parameter.
#' @param test_data The test dataset the user wants to make predictions on.
#'
#' @return exposes the accuracy() and predictions() methods.
#'
#' @example man/examples/predict.R
#'

#' @export
predict.hhcartr <- function(object, ..., test_data){

  # get parameters used to create the model
  useIdentity <-pkg.env$useIdentity
  classify <- pkg.env$classify
  if(is.na(useIdentity) | is.na(classify)){
    stop("hhcartr(predict.hhcartr) Run the fit() function before trying to make predictions.")
  }

  # need to validate the test_data here - it must have the y column as the last column.
  hhcart_verify_input_data(test_data[,1:ncol(test_data) - 1],
                           as.factor(test_data[,ncol(test_data)]),
                           classify = classify)

  # make sure the y column is a factor.
  test_data[,ncol(test_data)] <- as.factor(test_data[,ncol(test_data)])

  # go and make predictions on the test set
  prediction_output <- make_predictions(object,
                                        test_data,
                                        useIdentity,
                                        classify,
                                        objectid = 999999)

  # tree accuracy in [[1]], mr in [[2]], predictions for each tree in [[3]]
  stats <- prediction_output[[1]]

  # predictions for each row on each tree
  preds <- prediction_output[[3]]
  
  # numbers for each tree
  numbers_tree <- prediction_output[[4]]

  df <- data.frame()
  for (i in seq_along(stats)){
    nRow <- data.frame(Fold = i, Accuracy = round(stats[[i]], 2))
    df <- rbind(df, nRow)
  }

  # display the accuracy results.
  msg <- "Predicting on the Test data of the %s dataset..."
  msgs <- sprintf(msg, get_data_description())
  message(msgs)
  msg <- "Test Data Accuracy: Mean accuracy-[%s]"
  msgs <- sprintf(msg, round(mean(df$Accuracy), 2))
  message(msgs)

  parms <- list(
    accuracy = function(){
      return(df)
    },
    predictions = function(){
      return(preds)
    }
    numbers = function(){
      return(numbers_tree)
    }
  )
  class(parms) <- append(class(parms), "predict")
  return(parms)
}
