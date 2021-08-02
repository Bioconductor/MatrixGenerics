#' Tabulates the values in a matrix-like object by row (column)
#'
#' Tabulates the values in a matrix-like object by row (column).
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowTabulates
#'
#' @templateVar rowName rowTabulates
#' @templateVar colName colTabulates
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @param values the values to search for.
#' @template useNamesParameter
#'
#' @template returnMatrix_JDim
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowTabulates}()} and
#'   \code{matrixStats::\link[matrixStats:rowTabulates]{colTabulates}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link{table}()}
#' }
#'
#' @examples
#'   mat <- matrix(rpois(15, lambda = 3), nrow = 5, ncol = 3)
#'   mat[2, 1] <- NA_integer_
#'   mat[3, 3] <- 0L
#'   mat[4, 1] <- 0L
#'
#'   print(mat)
#'
#'   rowTabulates(mat)
#'   colTabulates(mat)
#'
#'   rowTabulates(mat, values = 0)
#'   colTabulates(mat, values = 0)
#'
#' @keywords array iteration robust univar
setGeneric("rowTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ..., useNames = NA) standardGeneric("rowTabulates"),
           signature = "x"
)

.matrixStats_rowTabulates <- function(x, rows = NULL, cols = NULL, values = NULL, ..., useNames = NA){
  matrixStats::rowTabulates(x, rows = rows, cols = cols, values = values, ..., useNames = NA)
}

#' @export
#' @rdname rowTabulates
setMethod("rowTabulates", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowTabulates)

#' @export
#' @rdname rowTabulates
## Default method with user-friendly fallback mechanism.
setMethod("rowTabulates", "ANY", make_default_method_def("rowTabulates"))



#' @export
#' @rdname rowTabulates
setGeneric("colTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ..., useNames = NA) standardGeneric("colTabulates"),
           signature = "x"
)

.matrixStats_colTabulates <- function(x, rows = NULL, cols = NULL, values = NULL, ..., useNames = NA){
  matrixStats::colTabulates(x, rows = rows, cols = cols, values = values, ..., useNames = NA)
}

#' @export
#' @rdname rowTabulates
setMethod("colTabulates", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colTabulates)

#' @export
#' @rdname rowTabulates
## Default method with user-friendly fallback mechanism.
setMethod("colTabulates", "ANY", make_default_method_def("colTabulates"))

