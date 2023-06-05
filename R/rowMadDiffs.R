#' Calculates the mean absolute deviation of the difference between each
#' element of a row (column) of a matrix-like object
#'
#' Calculates the mean absolute deviation of the difference between each
#' element of a row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMadDiffs
#'
#' @templateVar rowName rowMadDiffs
#' @templateVar colName colMadDiffs
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template diff_trimParameters
#' @template na_rmParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:varDiff]{rowMadDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:varDiff]{colMadDiffs}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowMadDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = NA) standardGeneric("rowMadDiffs"),
           signature = "x"
)

.matrixStats_rowMadDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = NA){
  matrixStats::rowMadDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ..., useNames = !isFALSE(useNames))
}

#' @export
#' @rdname rowMadDiffs
setMethod("rowMadDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMadDiffs)

#' @export
#' @rdname rowMadDiffs
## Default method with user-friendly fallback mechanism.
setMethod("rowMadDiffs", "ANY", make_default_method_def("rowMadDiffs"))



#' @export
#' @rdname rowMadDiffs
setGeneric("colMadDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = NA) standardGeneric("colMadDiffs"),
           signature = "x"
)

.matrixStats_colMadDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = NA){
  matrixStats::colMadDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff =diff, trim = trim, ..., useNames = !isFALSE(useNames))
}

#' @export
#' @rdname rowMadDiffs
setMethod("colMadDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMadDiffs)

#' @export
#' @rdname rowMadDiffs
## Default method with user-friendly fallback mechanism.
setMethod("colMadDiffs", "ANY", make_default_method_def("colMadDiffs"))

