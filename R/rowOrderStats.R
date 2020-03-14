#' Calculates an order statistic for each row (column) of a matrix-like object
#'
#' Calculates an order statistic for each row (column) of a matrix-like object
#' 
#' @templateVar rowName rowOrderStats
#' @templateVar colName colOrderStats
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters\
#' @param which An integer index in \[1,K\] (\[1,N\]) indicating which order statistic to be returned
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowOrderStats}()} and
#'   \code{matrixStats::\link[matrixStats]{colOrderStats}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowOrderStats
#' @export
setGeneric("rowOrderStats", function(x, rows = NULL, cols = NULL, which, ...) standardGeneric("rowOrderStats"),
           signature = "x"
)

.default_rowOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x)){
  matrixStats::rowOrderStats(x, rows = rows, cols = cols, which = which, dim. = dim.)
}

#' @rdname rowOrderStats
setMethod("rowOrderStats", signature = "matrix", .default_rowOrderStats)

#' @rdname rowOrderStats
setMethod("rowOrderStats", signature = "numeric", .default_rowOrderStats)

#' @rdname rowOrderStats
setMethod("rowOrderStats", signature = "array", .default_rowOrderStats)




#' @rdname rowOrderStats
#' @name colOrderStats
#' @export
setGeneric("colOrderStats", function(x, rows = NULL, cols = NULL, which, ...) standardGeneric("colOrderStats"),
           signature = "x"
)

.default_colOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x)){
  matrixStats::colOrderStats(x, rows = rows, cols = cols, which = which, dim. = dim.)
}

#' @rdname rowOrderStats
setMethod("colOrderStats", signature = "matrix", .default_colOrderStats)

#' @rdname rowOrderStats
setMethod("colOrderStats", signature = "numeric", .default_colOrderStats)

#' @rdname rowOrderStats
setMethod("colOrderStats", signature = "array", .default_colOrderStats)



