#' Check if any elements in a row (column) of a matrix-like object is equal to
#' a value
#'
#' Check if any elements in a row (column) of a matrix-like object is equal to
#' a value.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowAnys
#' @templateVar colName colAnys
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template valueParameter
#' @template dimParameter
#' @template na_rmParameter
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowAlls]{rowAnys}()} and
#'   \code{matrixStats::\link[matrixStats:rowAlls]{colAnys}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link{any}()}.
#' }
#' 
#' @template standardExamples
#'
#'
#' @name rowAnys
#' @export
setGeneric("rowAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("rowAnys"),
           signature = "x"
)

.matrixStats_rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::rowAnys(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowAnys
setMethod("rowAnys", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowAnys)



#' @rdname rowAnys
#' @name colAnys
#' @export
setGeneric("colAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("colAnys"),
           signature = "x"
)

.matrixStats_colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::colAnys(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowAnys
setMethod("colAnys", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colAnys)

