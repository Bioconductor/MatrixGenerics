#' Check if all elements in a row (column) of a matrix-like object are equal to 
#' a value
#'
#' Check if all elements in a row (column) of a matrix-like object are equal to 
#' a value.
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowAlls
#'
#' @templateVar rowName rowAlls
#' @templateVar colName colAlls
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template valueParameter
#' @template dimParameter
#' @template na_rmParameter
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowAlls]{rowAlls}()} and
#'   \code{matrixStats::\link[matrixStats:rowAlls]{colAlls}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if \emph{any} element is equal to a value, see 
#'   \code{\link{rowAnys}()}.
#' \item \code{base::\link{all}()}.
#' }
#' 
#' @template standardExamples
setGeneric("rowAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("rowAlls"),
           signature = "x"
)

.matrixStats_rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::rowAlls(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ...)
}

#' @export
#' @rdname rowAlls
setMethod("rowAlls", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowAlls)

#' @export
#' @rdname rowAlls
## Default method with user-friendly fallback mechanism.
setMethod("rowAlls", "ANY", make_default_method_def("rowAlls"))



#' @export
#' @rdname rowAlls
setGeneric("colAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("colAlls"),
           signature = "x"
)

.matrixStats_colAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::colAlls(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ...)
}

#' @export
#' @rdname rowAlls
setMethod("colAlls", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colAlls)

#' @export
#' @rdname rowAlls
## Default method with user-friendly fallback mechanism.
setMethod("colAlls", "ANY", make_default_method_def("colAlls"))

