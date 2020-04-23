#' Calculates the difference between each element of a row (column) of a
#' matrix-like object
#'
#' Calculates the difference between each element of a row (column) of a
#' matrix-like object.
#' 
#' @templateVar rowName rowDiffs
#' @templateVar colName colDiffs
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @param lag An integer specifying the lag.
#' @param differences An integer specifying the order of difference.
#' @template dimParameter
#'
#' @return Returns a \code{\link{numeric}} \code{\link{matrix}} with one column
#' (row) less than x: \eqn{Nx(K-1)} or \eqn{(N-1)xK}.
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:rowDiffs]{colDiffs}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link{diff}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowDiffs
#' @export
setGeneric("rowDiffs", function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,  ...) standardGeneric("rowDiffs"),
           signature = "x"
)

.default_rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...){
  matrixStats::rowDiffs(x, rows = rows, cols = cols, lag = lag, differences = differences, dim. = dim., ...)
}

#' @rdname rowDiffs
setMethod("rowDiffs", signature = "matrix", .default_rowDiffs)

#' @rdname rowDiffs
setMethod("rowDiffs", signature = "numeric", .default_rowDiffs)

#' @rdname rowDiffs
setMethod("rowDiffs", signature = "array", .default_rowDiffs)




#' @rdname rowDiffs
#' @name colDiffs
#' @export
setGeneric("colDiffs", function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, ...) standardGeneric("colDiffs"),
           signature = "x"
)

.default_colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ...){
  matrixStats::colDiffs(x, rows = rows, cols = cols, lag = lag, differences = differences, dim. = dim., ...)
}

#' @rdname rowDiffs
setMethod("colDiffs", signature = "matrix", .default_colDiffs)

#' @rdname rowDiffs
setMethod("colDiffs", signature = "numeric", .default_colDiffs)

#' @rdname rowDiffs
setMethod("colDiffs", signature = "array", .default_colDiffs)



