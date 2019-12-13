#' @md
#' @examples 
#'   mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
#'   mat[2, 1] <- NA
#'   mat[3, 3] <- Inf
#'   mat[4, 1] <- 0
#'   
#'   print(mat)
#'   
#'   <%= rowName %>(mat)
#'   <%= colName %>(mat)
#'   
