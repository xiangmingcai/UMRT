#' Estimate Leakage Signal from Background and Positive Signal Matrices
#'
#' This function computes a leakage signal (`LefSig`) by subtracting the column-wise medians of a background signal matrix (`B_neg`)
#' from a positive signal matrix (`B_pos`). It then zeroes out the specified positive variable column and projects the result using
#' a transformation matrix `A`. The final leakage signal is obtained by taking the row-wise median of the projected matrix.
#'
#' @param B_neg A numeric matrix representing background (negative) signals. Each column corresponds to a variable.
#' @param B_pos A numeric matrix representing positive signals. Must have the same column names as `B_neg`.
#' @param positive_var A string or numeric index indicating the column in `B_pos` that corresponds to the positive variable.
#'                     If a string, it must match one of the column names in `A`.
#' @param A A numeric matrix used to project the adjusted signal matrix. Its column names must match those of `B_neg` and `B_pos`.
#'
#' @return A list containing:
#' \describe{
#'   \item{LefSig}{A numeric vector of the estimated leakage signal, computed as the row-wise median of the projected matrix.}
#'   \item{medianPosValue}{The median value of the positive variable column before it was zeroed.}
#' }
#'
#' @examples
#' set.seed(123)
#' B_neg <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' B_pos <- matrix(rnorm(100, mean = 1), nrow = 10, ncol = 10)
#' colnames(B_neg) <- colnames(B_pos) <- paste0("V", 1:10)
#' A <- matrix(runif(100), nrow = 10, ncol = 10)
#' colnames(A) <- paste0("V", 1:10)
#' result <- LeftSig(B_neg, B_pos, positive_var = "V5", A)
#' result$LefSig
#' result$medianPosValue
#'
#' @export

LeftSig = function(B_neg, B_pos, positive_var, A) {
  #format requirement:
  #B_neg: (cell x fluors)
  #B_pos: (cell x fluors)
  #A: (detectors x fluors)
  
  # Check if B_neg and B_pos have the same column names
  if (!identical(colnames(B_neg), colnames(B_pos))) {
    stop("B_neg and B_pos must have the same column names in the same order.")
  }
  
  # Check if A has column names
  if (is.null(colnames(A))) {
    stop("Matrix A must have column names.")
  }
  
  # Check if column names of B_neg/B_pos match those of A
  if (!all(colnames(B_neg) %in% colnames(A))) {
    stop("Column names of B_neg/B_pos must be present in matrix A.")
  }
  
  # Check if positive_var is a valid column name or index
  if (is.character(positive_var)) {
    if (!(positive_var %in% colnames(B_pos))) {
      stop("positive_var must be a valid column name in B_pos.")
    }
    positive_var_index <- which(colnames(B_pos) == positive_var)
  } else if (is.numeric(positive_var)) {
    if (positive_var < 1 || positive_var > ncol(B_pos)) {
      stop("positive_var index is out of bounds.")
    }
    positive_var_index <- positive_var
  } else {
    stop("positive_var must be either a column name or a numeric index.")
  }
  
  # Compute leakage signal
  B_neg_Median <- t(as.matrix(apply(B_neg, 2, median)))
  B_dif <- B_pos - matrix(rep(B_neg_Median, each = nrow(B_pos)), nrow = nrow(B_pos))
  B_lef <- as.matrix(B_dif)
  medianPosValue <- median(B_lef[, positive_var_index])
  B_lef[, positive_var_index] <- 0
  R_lef <- A %*% t(B_lef)
  LefSig <- apply(R_lef, 1, median)
  
  return(list(LefSig = LefSig, medianPosValue = medianPosValue))
}
