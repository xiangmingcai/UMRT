#' Scatter Plot with Optional Scaling and Sampling
#'
#' This function generates a scatter plot of two selected columns from a data frame or matrix.
#' It supports optional log or arcsinh scaling, and random sampling of rows for visualization.
#'
#' @param data A data frame or matrix containing numeric values.
#' @param x_col A string specifying the column name to be used for the x-axis.
#' @param y_col A string specifying the column name to be used for the y-axis.
#' @param scale Optional. A string specifying the transformation to apply: either \code{"log"} or \code{"arcsinh"}.
#'              If \code{NULL}, no transformation is applied.
#' @param scale_factor A numeric value used as the base for log scaling or the divisor for arcsinh transformation. Default is 10.
#' @param sample_size An integer specifying the number of rows to randomly sample from \code{data} for plotting.
#'                    If \code{NULL}, all rows are used.
#'
#' @return A ggplot object showing the scatter plot of the selected columns, with optional transformation and sampling.
#'
#' @examples
#' library(ggplot2)
#' set.seed(42)
#' df <- data.frame(A = rnorm(1000), B = rnorm(1000))
#' CheckScatter(df, "A", "B", scale = "log", scale_factor = 10, sample_size = 200)
#'
#' @import ggplot2
#' @export

CheckScatter <- function(data, x_col, y_col, scale = NULL, scale_factor = 10, sample_size = 100) {
  
  # check colnames
  if (!(x_col %in% colnames(data))) stop(paste("Colname", x_col, "is not in data."))
  if (!(y_col %in% colnames(data))) stop(paste("Colname", y_col, "is not in data."))
  
  # sample data
  if(!is.null(sample_size)){
    set.seed(123)
    sample_data <- data[sample(nrow(data), sample_size, replace = FALSE), ]
  }else{
    sample_data <- data
  }
  
  
  # origianl value
  orig_x <- sample_data[,x_col]
  orig_y <- sample_data[,y_col]
  
  # inital lab
  lab_extent <- ""
  
  # transformation
  if (!is.null(scale) && scale == "log") {
    lab_extent <- paste0(" (scale: log ", scale_factor, ")")
    trans_x <- sign(orig_x) * log(base = scale_factor, x = abs(orig_x))
    trans_y <- sign(orig_y) * log(base = scale_factor, x = abs(orig_y))

    # generate breaks and transform back to labels
    breaks_x <- pretty(trans_x, n = 5)
    labels_x <- round(sign(breaks_x) * scale_factor ^ abs(breaks_x), 2)
    
    breaks_y <- pretty(trans_y, n = 5)
    labels_y <- round(sign(breaks_y) * scale_factor ^ abs(breaks_y), 2)
    
    
  } else if (!is.null(scale) && scale == "arcsinh") {
    lab_extent <- paste0(" (scale: arcsinh ", scale_factor, ")")
    trans_x <- asinh(orig_x / scale_factor)
    trans_y <- asinh(orig_y / scale_factor)

    # generate breaks and transform back to labels
    breaks_x <- pretty(trans_x, n = 5)
    labels_x <- round(scale_factor * sinh(breaks_x), 2)
    
    breaks_y <- pretty(trans_y, n = 5)
    labels_y <- round(scale_factor * sinh(breaks_y), 2)
    
  } else {
    trans_x <- orig_x
    trans_y <- orig_y
    breaks_x <- pretty(trans_x, n = 5)
    labels_x <- breaks_x
    breaks_y <- pretty(trans_y, n = 5)
    labels_y <- breaks_y
  }
  
  # make plot data
  plot_data <- data.frame(x = trans_x, y = trans_y)

  # draw
  ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(color = "blue", size = 1) +
    scale_x_continuous(breaks = breaks_x, labels = labels_x) +
    scale_y_continuous(breaks = breaks_y, labels = labels_y) +
    labs(title = paste("Scatter Plot"),
         x = paste0(x_col, lab_extent),
         y = paste0(y_col, lab_extent)) +
    theme_bw()
}
