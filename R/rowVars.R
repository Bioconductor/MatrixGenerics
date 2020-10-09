#' Calculates the variance for each row (column) of a matrix-like object
#'
#' Calculates the variance for each row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowVars
#'
#' @templateVar rowName rowVars
#' @templateVar colName colVars
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means.
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowVars}()} and
#'   \code{matrixStats::\link[matrixStats:rowVars]{colVars}()} which are used 
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For standard deviation estimates, see \code{\link{rowSds}()}.
#' \item \code{stats::\link[stats:cor]{var}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("rowVars"),
           signature = "x"
)

.matrixStats_rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::rowVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @export
#' @rdname rowVars
setMethod("rowVars", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowVars)

#' @export
#' @rdname rowVars
## Default method with user-friendly fallback mechanism.
setMethod("rowVars", "ANY", make_default_method_def("rowVars"))



#' @export
#' @rdname rowVars
setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("colVars"),
           signature = "x"
)

.matrixStats_colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::colVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @export
#' @rdname rowVars
setMethod("colVars", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colVars)

#' @export
#' @rdname rowVars
## Default method with user-friendly fallback mechanism.
setMethod("colVars", "ANY", make_default_method_def("colVars"))

