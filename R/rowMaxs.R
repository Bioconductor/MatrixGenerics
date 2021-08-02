#' Calculates the maximum for each row (column) of a matrix-like object
#'
#' Calculates the maximum for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMaxs
#'
#' @templateVar rowName rowMaxs
#' @templateVar colName colMaxs
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
#' \item \code{matrixStats::\link[matrixStats:rowRanges]{rowMaxs}()} and
#'   \code{matrixStats::\link[matrixStats:rowRanges]{colMaxs}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowMaxs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowMaxs"),
           signature = "x"
)

.matrixStats_rowMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowMaxs(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowMaxs
setMethod("rowMaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMaxs)

#' @export
#' @rdname rowMaxs
## Default method with user-friendly fallback mechanism.
setMethod("rowMaxs", "ANY", make_default_method_def("rowMaxs"))



#' @export
#' @rdname rowMaxs
setGeneric("colMaxs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colMaxs"),
           signature = "x"
)

.matrixStats_colMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colMaxs(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowMaxs
setMethod("colMaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMaxs)

#' @export
#' @rdname rowMaxs
## Default method with user-friendly fallback mechanism.
setMethod("colMaxs", "ANY", make_default_method_def("colMaxs"))

