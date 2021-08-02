#' Calculates the minimum for each row (column) of a matrix-like object
#'
#' Calculates the minimum for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMins
#'
#' @templateVar rowName rowMins
#' @templateVar colName colMins
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowRanges]{rowMins}()} and
#'   \code{matrixStats::\link[matrixStats:rowRanges]{colMins}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For max estimates, see \code{\link{rowMaxs}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowMins", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowMins"),
           signature = "x"
)

.matrixStats_rowMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowMins(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowMins
setMethod("rowMins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMins)

#' @export
#' @rdname rowMins
## Default method with user-friendly fallback mechanism.
setMethod("rowMins", "ANY", make_default_method_def("rowMins"))



#' @export
#' @rdname rowMins
setGeneric("colMins", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colMins"),
           signature = "x"
)

.matrixStats_colMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colMins(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowMins
setMethod("colMins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMins)

#' @export
#' @rdname rowMins
## Default method with user-friendly fallback mechanism.
setMethod("colMins", "ANY", make_default_method_def("colMins"))

