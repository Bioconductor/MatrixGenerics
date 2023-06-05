#' Calculates the variance of the difference between each element of a row
#' (column) of a matrix-like object
#'
#' Calculates the variance of the difference between each element of a row
#' (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowVarDiffs
#'
#' @templateVar rowName rowVarDiffs
#' @templateVar colName colVarDiffs
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
#' \item \code{matrixStats::\link[matrixStats:varDiff]{rowVarDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:varDiff]{colVarDiffs}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item for the direct variance see [rowVars()].
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowVarDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = TRUE) standardGeneric("rowVarDiffs"),
           signature = "x"
)

.matrixStats_rowVarDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = TRUE){
  matrixStats::rowVarDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ..., useNames = useNames)
}

#' @export
#' @rdname rowVarDiffs
setMethod("rowVarDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowVarDiffs)

#' @export
#' @rdname rowVarDiffs
## Default method with user-friendly fallback mechanism.
setMethod("rowVarDiffs", "ANY", make_default_method_def("rowVarDiffs"))



#' @export
#' @rdname rowVarDiffs
setGeneric("colVarDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = TRUE) standardGeneric("colVarDiffs"),
           signature = "x"
)

.matrixStats_colVarDiffs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ..., useNames = TRUE){
  matrixStats::colVarDiffs(x, rows = rows, cols = cols, na.rm = na.rm, diff = diff, trim = trim, ..., useNames = useNames)
}

#' @export
#' @rdname rowVarDiffs
setMethod("colVarDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colVarDiffs)

#' @export
#' @rdname rowVarDiffs
## Default method with user-friendly fallback mechanism.
setMethod("colVarDiffs", "ANY", make_default_method_def("colVarDiffs"))

