#' Calculates the difference between each element of a row (column) of a
#' matrix-like object
#'
#' Calculates the difference between each element of a row (column) of a
#' matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowDiffs
#'
#' @templateVar rowName rowDiffs
#' @templateVar colName colDiffs
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @param lag An integer specifying the lag.
#' @param differences An integer specifying the order of difference.
#' @template dimParameter
#' @template useNamesParameter
#'
#' @return Returns a \code{\link{numeric}} \code{\link{matrix}} with one column
#' (row) less than x: \eqn{Nx(K-1)} or \eqn{(N-1)xK}.
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowDiffs}()} and
#'   \code{matrixStats::\link[matrixStats:rowDiffs]{colDiffs}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link{diff}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowDiffs", function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,  ..., useNames = NA) standardGeneric("rowDiffs"),
           signature = "x"
)

.matrixStats_rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowDiffs(x, rows = rows, cols = cols, lag = lag, differences = differences, dim. = dim., ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowDiffs
setMethod("rowDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowDiffs)

#' @export
#' @rdname rowDiffs
## Default method with user-friendly fallback mechanism.
setMethod("rowDiffs", "ANY", make_default_method_def("rowDiffs"))



#' @export
#' @rdname rowDiffs
setGeneric("colDiffs", function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, ..., useNames = NA) standardGeneric("colDiffs"),
           signature = "x"
)

.matrixStats_colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, dim. = dim(x), ..., useNames = NA){
  matrixStats::colDiffs(x, rows = rows, cols = cols, lag = lag, differences = differences, dim. = dim., ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowDiffs
setMethod("colDiffs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colDiffs)

#' @export
#' @rdname rowDiffs
## Default method with user-friendly fallback mechanism.
setMethod("colDiffs", "ANY", make_default_method_def("colDiffs"))

