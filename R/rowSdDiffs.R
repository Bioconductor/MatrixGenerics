#' Calculates the standard deviation of the difference between each element of
#' a row (column) of a matrix-like object
#'
#' Calculates the standard deviation of the difference between each element of
#' a row (column) of a matrix-like object.
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

.default_rowSdDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowSdDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowSdDiffs
setMethod("rowSdDiffs", signature = "matrix", .default_rowSdDiffs)

#' @rdname rowSdDiffs
setMethod("rowSdDiffs", signature = "numeric", .default_rowSdDiffs)

#' @rdname rowSdDiffs
setMethod("rowSdDiffs", signature = "array", .default_rowSdDiffs)




#' @rdname rowSdDiffs
#' @name colSdDiffs
#' @export
setGeneric("colSdDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("colSdDiffs"),
           signature = "x"
)

.default_colSdDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::colSdDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ...)
}

#' @rdname rowSdDiffs
setMethod("colSdDiffs", signature = "matrix", .default_colSdDiffs)

#' @rdname rowSdDiffs
setMethod("colSdDiffs", signature = "numeric", .default_colSdDiffs)

#' @rdname rowSdDiffs
setMethod("colSdDiffs", signature = "array", .default_colSdDiffs)



