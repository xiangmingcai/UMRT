#' Correct a Signal by Subtracting a Reference Signal Scaled by a Correction Factor
#'
#' This function adjusts an input signal by subtracting a reference signal divided by a correction factor.
#' It is typically used to correct for background or leakage effects in signal processing.
#'
#' @param InputSig A named numeric vector representing the input signal to be corrected.
#' @param LefSig A named numeric vector representing the reference signal (e.g., leakage or background).
#'               Must have the same names and order as \code{InputSig}.
#' @param correct_factor A single numeric value greater than 0 used to scale the reference signal before subtraction.
#'
#' @return A numeric vector of the corrected signal, with the same names as \code{InputSig}.
#'
#' @examples
#' InputSig <- c(A = 0.1, B = 1, C = 0.3)
#' LefSig <- c(A = 0.2, B = 0.4, C = 1)
#' correct_factor <- 2
#' CorrectSig(InputSig, LefSig, correct_factor)
#'
#' @export

CorrectSig = function(InputSig, LefSig, correct_factor){
  
  #check if InputSig and LefSig have the same names
  if (!all(names(InputSig) == names(LefSig))) {
    stop("InputSig and LefSig must have the same names and order.")
  }
  #check if correct_factor is a numeric >0
  if (!is.numeric(correct_factor) || length(correct_factor) != 1 || correct_factor <= 0) {
    stop("correct_factor must be a single numeric value greater than 0.")
  }
  
  CorrectedSig = InputSig - LefSig / correct_factor
  
  return(CorrectedSig)
  
}
