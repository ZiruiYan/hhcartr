# source: hhcartr_export_print.R

#################################################################################################
#'
#' print.hhcartr - Create generic S3method to print results via print.hhcartr. Needs export entry
#' in the NAMESPACE file.
#'
#' This function will generate a ggplot showing test set accuracy for each tree fold/trial. In
#' future will need to provide support to allow user to customize the plots.
#'
#' @param x   Unused parameter.
#' @param ... Unused parameter.
#'
#' @return    A ggplot of training accuracy.
#'
#' @example man/examples/print.R
#'

#' @export
print.hhcartr <- function(x, ...){
  # verify package ggplot2 installed and loaded
  packages <- c("ggplot2")
  check_package(packages)

  # initialise variables
  acc_data     <- NA
  subtitleskl  <- ""
  main         <- ""
  xlab         <- ""
  #alpha        <- 0.95

  # get parameters used to create the model
  classify     <- pkg.env$classify

  run_stats_data <- get_run_stats(classify = classify)
  if(is.na(run_stats_data)){
    stop("No data to display.")
  }
  run_stats_data <- run_stats_data[[1]]
  # need to verify run_stats_data before plotting histogram

  subtitleskl <- "Mean %.1f%%, +/- 1 std of %.1f%% (%.1f%% and %.1f%%)"
  if(classify){
    acc_data   <- unlist(run_stats_data["Accuracy"])
    main       <- "HHCART(G) Classification Accuracy Distribution \nfor dataset: %s."
    xlab       <- "Accuracy(%)"
  } else {
    acc_data   <- unlist(run_stats_data["RMSE"])
    main       <- "HHCART(G) Regression RMSE Distribution \nfor dataset: %s."
    xlab       <- "RMSE"
  }
  #p            <- ((1.0 - alpha) / 2.0) * 100
  #lower        <- max(0.0, quantile(acc_data, p/100))
  #p            <- (alpha + ((1.0 - alpha) / 2.0)) * 100
  #upper        <- min(100.0, quantile(acc_data, p/100))

  meanacc      <- mean(acc_data)
  # calculate +/- 1 standard deviation from the mean.
  stddev       <- sd(acc_data)
  lower        <- meanacc - stddev
  upper        <- meanacc + stddev
  mains        <- sprintf(main, get_data_description())
  subtitle     <- sprintf(subtitleskl, meanacc, stddev, lower, upper)
  ylab         <- "Frequency (count)"
  df_acc_data  <- data.frame(acc_data)

  p <- ggplot(df_acc_data, aes_string(x="acc_data")) +
    geom_histogram(fill="darkseagreen1", position="dodge", bins = 30) +
    geom_vline(xintercept = meanacc, linetype="dotted", color = "blue", size=1.5) +
    geom_vline(xintercept = lower,   linetype="dotted", color = "red",  size=1.0) +
    geom_vline(xintercept = upper,   linetype="dotted", color = "red",  size=1.0) +
    ggtitle(label=mains, subtitle=subtitle) +
    # Change x and y axis labels, and limits
    scale_x_continuous(name=xlab) +
    scale_y_continuous(name=ylab) +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 14),     # Center title position and size
      plot.subtitle = element_text(hjust = 0.5),                # Center subtitle
      plot.caption  = element_text(hjust = 0, face = "italic"), # move caption to the left
      plot.margin   = unit(c(1, 1, 1, 1),"cm")
    )
  return(p)
}
