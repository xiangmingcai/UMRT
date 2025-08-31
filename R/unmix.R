#' Unmix Fluorescence Signals Using a Reference Matrix
#'
#' This function performs signal unmixing by projecting cell-level fluorescence data onto a reference matrix.
#' It uses the Moore-Penrose pseudoinverse of matrix \code{A} to estimate the contribution of each fluorophore
#' to the observed signal in each cell.
#'
#' @param data A numeric matrix of dimensions (cells x detectors), representing observed fluorescence signals per cell.
#'             Column names must match the row names of \code{A}.
#' @param A A numeric matrix of dimensions (detectors x fluors), representing the reference signal matrix.
#'          Row names must match the column names of \code{data}.
#'
#' @return A numeric matrix of dimensions (cells x fluors), representing the unmixed signal for each fluorophore per cell.
#'
#' @examples
#' library(MASS)  # for ginv
#' set.seed(123)
#' data <- matrix(runif(30), nrow = 10, ncol = 3)
#' colnames(data) <- c("D1", "D2", "D3")
#' A <- matrix(runif(9), nrow = 3, ncol = 3)
#' rownames(A) <- c("D1", "D2", "D3")
#' colnames(A) <- c("FITC", "PE", "APC")
#' unmixed <- unmix(data, A)
#' head(unmixed)
#'
#' @importFrom MASS ginv
#' @export

unmix = function(data, A){
  #format requirement:
  #data: (cell x detectors)
  #A: (detectors x fluors)
  
  # Check if data and A have the same column names
  if (!identical(colnames(data), rownames(A))) {
    stop("column names of data must be same as row names of A, also in the same order.")
  }
  
  R = t(as.matrix(data))#R (detectors x cells)
  A_pinv = ginv(A)
  B =  A_pinv %*% R #B (fluors x cells)
  out = t(B) #out (cells x fluors)
  colnames(out) = colnames(A)
  return(out)
}