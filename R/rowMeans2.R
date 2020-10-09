#' Calculates the mean for each row (column) of a matrix-like object
#'
#' Calculates the mean for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMeans2
#' 
#' @templateVar rowName rowMeans2
#' @templateVar colName colMeans2
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowMeans2}()} and
#'   \code{matrixStats::\link[matrixStats:rowMeans2]{colMeans2}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also \code{\link[base:colSums]{rowMeans}()} for the
#'   corresponding function in base R.
#' \item For variance estimates, see \code{\link{rowVars}()}.
#' \item See also the base R version \code{base::\link{rowMeans}()}.
#' }
#' 
#' @template standardExamples
#'   
#' 
#' @keywords array iteration robust univar
setGeneric("rowMeans2", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowMeans2"),
           signature = "x"
)

.matrixStats_rowMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::rowMeans2(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @export
#' @rdname rowMeans2
setMethod("rowMeans2", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMeans2)

#' @export
#' @rdname rowMeans2
## Default method with user-friendly fallback mechanism.
setMethod("rowMeans2", "ANY", make_default_method_def("rowMeans2"))



#' @export
#' @rdname rowMeans2
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colMeans2"),
           signature = "x"
)

.matrixStats_colMeans2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::colMeans2(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @export
#' @rdname rowMeans2
setMethod("colMeans2", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMeans2)

#' @export
#' @rdname rowMeans2
## Default method with user-friendly fallback mechanism.
setMethod("colMeans2", "ANY", make_default_method_def("colMeans2"))

