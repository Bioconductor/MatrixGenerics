#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object
#'
#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowLogSumExps
#' @templateVar colName colLogSumExps
#' 
#' @template matrixStatsLink
#' 
#' @param lx An NxK matrix-like object. Typically `lx` are `log(x)` values.
#' @param rows,cols A \code{\link{vector}} indicating the subset (and/or 
#' columns) to operate over. If \code{\link{NULL}}, no subsetting is done.
#' @param ... Additional arguments passed to specific methods.
#' @template na_rmParameter
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowLogSumExps}()} and
#'   \code{matrixStats::\link[matrixStats:rowLogSumExps]{colLogSumExps}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item [rowSums2()]
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowLogSumExps
#' @export
setGeneric("rowLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowLogSumExps"),
           signature = "lx"
)

.matrixStats_rowLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ...){
  matrixStats::rowLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowLogSumExps
setMethod("rowLogSumExps", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowLogSumExps)



#' @rdname rowLogSumExps
#' @name colLogSumExps
#' @export
setGeneric("colLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colLogSumExps"),
           signature = "lx"
)

.matrixStats_colLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ...){
  matrixStats::colLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowLogSumExps
setMethod("colLogSumExps", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colLogSumExps)

