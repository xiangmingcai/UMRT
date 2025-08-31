#' Compare Two Signal Signatures from a Matrix
#'
#' This function compares two signal signatures (rows) from a matrix by checking their existence,
#' validating that all values are numeric, and plotting their signal profiles across channels.
#'
#' @param mtx A matrix or data frame where rows represent signal signatures and columns represent channels.
#' @param sig1 A string specifying the name of the first signature to compare.
#' @param sig2 A string specifying the name of the second signature to compare.
#'
#' @return A ggplot object showing the signal profiles of the two selected signatures across channels.
#'         If either signature is not found or contains non-numeric values, an error is raised.
#'
#' @examples
#' library(ggplot2)
#' library(tibble)
#' mtx <- matrix(runif(20), nrow = 2, ncol = 10)
#' rownames(mtx) <- c("Sig1", "Sig2")
#' colnames(mtx) <- paste0("Ch", 1:10)
#' CompareSig(mtx, "Sig1", "Sig2")
#'
#' @import ggplot2
#' @importFrom tibble tibble
#' @export

CompareSig = function(mtx, sig1, sig2){
  
  if (!(sig1 %in% rownames(mtx))) {
    stop(paste0("The channel ", sig1, " is not found in the row names of input mtx. Please check it first."))
  }
  if (!(sig2 %in% rownames(mtx))) {
    stop(paste0("The channel ", sig2, " is not found in the row names of input mtx. Please check it first."))
  }
  
  selected_row_1 <- mtx[sig1, ]
  selected_row_2 <- mtx[sig2, ]
  
  #check format
  numeric_check_row = sapply(sapply(selected_row_1, as.numeric), is.na)
  if(any(numeric_check_row)){
    true_names <- names(numeric_check_row[numeric_check_row == TRUE])
    message_string <- paste("The following items in the selected row is not numeric: ", paste(true_names, collapse = ", "))
    stop(message_string)
  }
  
  numeric_check_row = sapply(sapply(selected_row_2, as.numeric), is.na)
  if(any(numeric_check_row)){
    true_names <- names(numeric_check_row[numeric_check_row == TRUE])
    message_string <- paste("The following items in the selected row is not numeric: ", paste(true_names, collapse = ", "))
    stop(message_string)
  }
  
  plot_data_1 <- tibble(
    Channel = names(selected_row_1),
    Signal = as.numeric(selected_row_1),
    Group = sig1
  )
  plot_data_2 <- tibble(
    Channel = names(selected_row_2),
    Signal = as.numeric(selected_row_2),
    Group = sig2
  )
  
  plot_data = rbind(plot_data_1,plot_data_2)
  ggplot(plot_data, aes(x = Channel, y = Signal, group = Group, color = Group)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = setNames(c("#4c8dae", "#ff7500"), c(sig1, sig2))) +
    labs(title = paste0("Comparison: ", sig1, " vs ", sig2),
         x = "Channel",
         y = "Signal") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}