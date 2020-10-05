#' Calculates the standard deviation of the difference between each element of
#' a row (column) of a matrix-like object
#'
#' Calculates the standard deviation of the difference between each element of
#' a row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowSdDiffs
#' @templateVar colName colSdDiffs
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template diff_trimParameters
#' @template na_rmParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:varDiff]{rowSdDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:varDiff]{colSdDiffs}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item for the direct standard deviation see [rowSds()].
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowSdDiffs
#' @export
setGeneric("rowSdDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0,  ...) standardGeneric("rowSdDiffs"),
           signature = "x"
)

.matrixStats_rowSdDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowSdDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowSdDiffs
setMethod("rowSdDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowSdDiffs)



#' @rdname rowSdDiffs
#' @name colSdDiffs
#' @export
setGeneric("colSdDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("colSdDiffs"),
           signature = "x"
)

.matrixStats_colSdDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::colSdDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowSdDiffs
setMethod("colSdDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colSdDiffs)

