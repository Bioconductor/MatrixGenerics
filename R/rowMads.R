#' Calculates the median absolute deviation for each row (column) of a
#' matrix-like object
#'
#' Calculates the median absolute deviation for each row (column) of a
#' matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowMads
#' @templateVar colName colMads
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means
#' @param constant A scale factor. See \code{stats::\link[stats]{mad}()} for 
#'   details.
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowSds]{rowMads}()} and
#'   \code{matrixStats::\link[matrixStats:rowSds]{colMads}()} which are used
#'   when  the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For non-robust standard deviation estimates, see
#'   \code{\link{rowSds}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowMads
#' @export
setGeneric("rowMads", function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, ...) standardGeneric("rowMads"),
           signature = "x"
)

.matrixStats_rowMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::rowMads(x, rows = rows, cols = cols, center = center, constant = constant, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowMads
setMethod("rowMads", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMads)



#' @rdname rowMads
#' @name colMads
#' @export
setGeneric("colMads", function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, ...) standardGeneric("colMads"),
           signature = "x"
)

.matrixStats_colMads <- function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::colMads(x, rows = rows, cols = cols, center = center, constant = constant, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowMads
setMethod("colMads", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMads)

