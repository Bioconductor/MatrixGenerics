#' Calculates an order statistic for each row (column) of a matrix-like object
#'
#' Calculates an order statistic for each row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowOrderStats
#'
#' @templateVar rowName rowOrderStats
#' @templateVar colName colOrderStats
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @param which An integer index in \[1,K\] (\[1,N\]) indicating which order
#'   statistic to be returned
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowOrderStats}()} and
#'   \code{matrixStats::\link[matrixStats:rowOrderStats]{colOrderStats}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @examples 
#'     mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
#'     mat[2, 1] <- 2
#'     mat[3, 3] <- Inf
#'     mat[4, 1] <- 0
#'     
#'     print(mat)
#'     
#'     rowOrderStats(mat, which = 1)
#'     colOrderStats(mat, which = 3)
#'
#' @keywords array iteration robust univar
setGeneric("rowOrderStats", function(x, rows = NULL, cols = NULL, which, ...) standardGeneric("rowOrderStats"),
           signature = "x"
)

.matrixStats_rowOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...){
  matrixStats::rowOrderStats(x, rows = rows, cols = cols, which = which, dim. = dim., ...)
}

#' @export
#' @rdname rowOrderStats
setMethod("rowOrderStats", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowOrderStats)

#' @export
#' @rdname rowOrderStats
## Default method with user-friendly fallback mechanism.
setMethod("rowOrderStats", "ANY", make_default_method_def("rowOrderStats"))



#' @export
#' @rdname rowOrderStats
setGeneric("colOrderStats", function(x, rows = NULL, cols = NULL, which, ...) standardGeneric("colOrderStats"),
           signature = "x"
)

.matrixStats_colOrderStats <- function(x, rows = NULL, cols = NULL, which, dim. = dim(x), ...){
  matrixStats::colOrderStats(x, rows = rows, cols = cols, which = which, dim. = dim., ...)
}

#' @export
#' @rdname rowOrderStats
setMethod("colOrderStats", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colOrderStats)

#' @export
#' @rdname rowOrderStats
## Default method with user-friendly fallback mechanism.
setMethod("colOrderStats", "ANY", make_default_method_def("colOrderStats"))

