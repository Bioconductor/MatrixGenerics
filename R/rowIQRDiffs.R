#' Calculates the interquartile range of the difference between each element of a row (column) of a matrix-like object
#'
#' Calculates the interquartile range of the difference between each element of a row (column) of a matrix-like object
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
#' \item \code{matrixStats::\link[matrixStats]{rowIQRDiffs}()} and
#'   \code{matrixStats::\link[matrixStats]{colIQRDiffs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowIQRDiffs
#' @export
setGeneric("rowIQRDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0,  ...) standardGeneric("rowIQRDiffs"),
           signature = "x"
)

.default_rowIQRDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  matrixStats::rowIQRDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim)
}

#' @rdname rowIQRDiffs
setMethod("rowIQRDiffs", signature = "matrix", .default_rowIQRDiffs)

#' @rdname rowIQRDiffs
setMethod("rowIQRDiffs", signature = "numeric", .default_rowIQRDiffs)

#' @rdname rowIQRDiffs
setMethod("rowIQRDiffs", signature = "array", .default_rowIQRDiffs)




#' @rdname rowIQRDiffs
#' @name colIQRDiffs
#' @export
setGeneric("colIQRDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...) standardGeneric("colIQRDiffs"),
           signature = "x"
)

.default_colIQRDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  matrixStats::colIQRDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff =diff, trim = trim)
}

#' @rdname rowIQRDiffs
setMethod("colIQRDiffs", signature = "matrix", .default_colIQRDiffs)

#' @rdname rowIQRDiffs
setMethod("colIQRDiffs", signature = "numeric", .default_colIQRDiffs)

#' @rdname rowIQRDiffs
setMethod("colIQRDiffs", signature = "array", .default_colIQRDiffs)



