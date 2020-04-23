#' Calculates the interquartile range for each row (column) of a matrix-like
#' object
#'
#' Calculates the interquartile range for each row (column) of a matrix-like
#' object.
#' 
#' @templateVar rowName rowIQRs
#' @templateVar colName colIQRs
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowIQRs}()} and
#'   \code{matrixStats::\link[matrixStats:rowIQRs]{colIQRs}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For a non-robust analog, see \code{\link{rowSds}()}. For a more
#'  robust version see [rowMads()]
#' \item \code{stats::\link[stats]{IQR}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowIQRs
#' @export
setGeneric("rowIQRs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowIQRs"),
           signature = "x"
)

.default_rowIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::rowIQRs(x, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowIQRs
setMethod("rowIQRs", signature = "matrix", .default_rowIQRs)

#' @rdname rowIQRs
setMethod("rowIQRs", signature = "numeric", .default_rowIQRs)

#' @rdname rowIQRs
setMethod("rowIQRs", signature = "array", .default_rowIQRs)




#' @rdname rowIQRs
#' @name colIQRs
#' @export
setGeneric("colIQRs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colIQRs"),
           signature = "x"
)

.default_colIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::colIQRs(x, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowIQRs
setMethod("colIQRs", signature = "matrix", .default_colIQRs)

#' @rdname rowIQRs
setMethod("colIQRs", signature = "numeric", .default_colIQRs)

#' @rdname rowIQRs
setMethod("colIQRs", signature = "array", .default_colIQRs)



