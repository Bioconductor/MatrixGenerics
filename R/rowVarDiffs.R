#' Calculates the variance of the difference between each element of a row (column) of a matrix-like object
#'
#' Calculates the variance of the difference between each element of a row (column) of a matrix-like object
#' 
#' @templateVar rowName rowVarDiffs
#' @templateVar colName colVarDiffs
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
#' \item \code{matrixStats::\link[matrixStats]{rowVarDiffs}()} and
#'   \code{matrixStats::\link[matrixStats]{colVarDiffs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item for the direct variance see [rowVars()].
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowVarDiffs
#' @export
setGeneric("rowVarDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0,  ...) standardGeneric("rowVarDiffs"),
           signature = "x"
)

.default_rowVarDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  matrixStats::rowVarDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim)
}

#' @rdname rowVarDiffs
setMethod("rowVarDiffs", signature = "matrix", .default_rowVarDiffs)

#' @rdname rowVarDiffs
setMethod("rowVarDiffs", signature = "numeric", .default_rowVarDiffs)

#' @rdname rowVarDiffs
setMethod("rowVarDiffs", signature = "array", .default_rowVarDiffs)




#' @rdname rowVarDiffs
#' @name colVarDiffs
#' @export
setGeneric("colVarDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("colVarDiffs"),
           signature = "x"
)

.default_colVarDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  matrixStats::colVarDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff =diff, trim = trim)
}

#' @rdname rowVarDiffs
setMethod("colVarDiffs", signature = "matrix", .default_colVarDiffs)

#' @rdname rowVarDiffs
setMethod("colVarDiffs", signature = "numeric", .default_colVarDiffs)

#' @rdname rowVarDiffs
setMethod("colVarDiffs", signature = "array", .default_colVarDiffs)



