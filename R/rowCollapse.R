#' Extract one cell from each row (column) of a matrix-like object
#'
#' Extract one cell from each row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowCollapse
#' @templateVar colName colCollapse
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template dimParameter
#' @param idxs An index \code{\link{vector}} with the position to extract.
#'   It is recycled to match the number of rows (column)
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCollapse}()} and
#'   \code{matrixStats::\link[matrixStats:rowCollapse]{colCollapse}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @examples 
#'   mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
#'   mat[2, 1] <- NA
#'   mat[3, 3] <- Inf
#'   mat[4, 1] <- 0
#'   
#'   print(mat)
#'   
#'   rowCollapse(mat, idxs = 2)
#'   rowCollapse(mat, idxs = c(1,1,2,3,2))
#'   
#'   colCollapse (mat, idxs = 4)
#'
#' @keywords array iteration robust univar
#'
#' @name rowCollapse
#' @export
setGeneric("rowCollapse", function(x, idxs, rows = NULL, ...) standardGeneric("rowCollapse"),
           signature = "x"
)

.matrixStats_rowCollapse <- function(x, idxs, rows = NULL, dim. = dim(x), ...){
  matrixStats::rowCollapse(x, idxs = idxs, rows = rows, dim. = dim., ...)
}

#' @rdname rowCollapse
setMethod("rowCollapse", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCollapse)



#' @rdname rowCollapse
#' @name colCollapse
#' @export
setGeneric("colCollapse", function(x, idxs = idxs, cols = NULL, ...) standardGeneric("colCollapse"),
           signature = "x"
)

.matrixStats_colCollapse <- function(x, idxs, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCollapse(x, idxs = idxs, cols = cols, dim. = dim., ...)
}

#' @rdname rowCollapse
setMethod("colCollapse", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCollapse)

