#' Calculates the interquartile range of the difference between each element of
#' a row (column) of a matrix-like object
#'
#' Calculates the interquartile range of the difference between each element of
#' a row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowIQRDiffs
#' @templateVar colName colIQRDiffs
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
#' \item \code{matrixStats::\link[matrixStats:varDiff]{rowIQRDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:varDiff]{colIQRDiffs}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For the direct interquartile range see also [rowIQRs].
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowIQRDiffs
#' @export
setGeneric("rowIQRDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("rowIQRDiffs"),
           signature = "x"
)

.matrixStats_rowIQRDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowIQRDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowIQRDiffs
setMethod("rowIQRDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowIQRDiffs)



#' @rdname rowIQRDiffs
#' @name colIQRDiffs
#' @export
setGeneric("colIQRDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("colIQRDiffs"),
           signature = "x"
)

.matrixStats_colIQRDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::colIQRDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowIQRDiffs
setMethod("colIQRDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colIQRDiffs)

