#' Check and Plot a Signature Row from a Matrix
#'
#' This function checks whether a specified signature (row) exists in a matrix, verifies that all values in the row are numeric,
#' and then plots the signal across channels using a line and point plot.
#'
#' @param mtx A matrix or data frame where rows represent signatures and columns represent channels.
#' @param select_signature A string specifying the name of the row (signature) to be checked and plotted.
#'
#' @return A ggplot object showing the signal profile of the selected signature across channels.
#'         If the signature is not found or contains non-numeric values, an error is raised.
#'
#' @examples
#' library(ggplot2)
#' library(tibble)
#' mtx <- matrix(runif(30), nrow = 3, ncol = 10)
#' rownames(mtx) <- c("SigA", "SigB", "SigC")
#' colnames(mtx) <- paste0("Ch", 1:10)
#' CheckSig(mtx, "SigB")
#'
#' @import ggplot2
#' @importFrom tibble tibble
#' @export

CheckSig = function(mtx, select_signature){
  
  if (!(select_signature %in% rownames(mtx))) {
    stop(paste0("The channel ", select_signature, " is not found in the row names of input mtx. Please check it first."))
  }
  
  selected_row <- mtx[select_signature, ]
  
  #check format
  numeric_check_row = sapply(sapply(selected_row, as.numeric), is.na)
  if(any(numeric_check_row)){
    true_names <- names(numeric_check_row[numeric_check_row == TRUE])
    message_string <- paste("The following items in the selected row is not numeric: ", paste(true_names, collapse = ", "))
    stop(message_string)
  }
  
  plot_data <- tibble(
    Channel = names(selected_row),
    Signal = as.numeric(selected_row)
  )
  
  ggplot(plot_data, aes(x = Channel, y = Signal)) +
    geom_line(group = 1, color = "#4c8dae") +
    geom_point(color = "#ff7500", size = 2) +
    labs(title = paste0("Signature of ",select_signature),
         x = "Channel",
         y = "Signal") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}