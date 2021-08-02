#' Count how often an element in a row (column) of a matrix-like object is
#' equal to a value
#'
#' Count how often an element in a row (column) of a matrix-like object is
#' equal to a value.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowCounts
#'
#' @templateVar rowName rowCounts
#' @templateVar colName colCounts
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template valueParameter
#' @template dimParameter
#' @template na_rmParameter
#' @template useNamesParameter
#'
#' @template returnVectorInt
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCounts}()} and
#'   \code{matrixStats::\link[matrixStats:rowCounts]{colCounts}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if any element is equal to a value, see
#'   \code{\link{rowAnys}()}. To check if all elements are equal, see
#'   \code{\link{rowAlls}()}.
#' }
#'
#' @template standardExamples
#'
#' @examples
#'   rowCounts(mat, value = 0)
#'   colCounts(mat, value = Inf, na.rm = TRUE)
setGeneric("rowCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowCounts"),
           signature = "x"
)

.matrixStats_rowCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowCounts(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowCounts
setMethod("rowCounts", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCounts)

#' @export
#' @rdname rowCounts
## Default method with user-friendly fallback mechanism.
setMethod("rowCounts", "ANY", make_default_method_def("rowCounts"))



#' @export
#' @rdname rowCounts
setGeneric("colCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ..., useNames = NA) standardGeneric("colCounts"),
           signature = "x"
)

.matrixStats_colCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colCounts(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowCounts
setMethod("colCounts", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCounts)

#' @export
#' @rdname rowCounts
## Default method with user-friendly fallback mechanism.
setMethod("colCounts", "ANY", make_default_method_def("colCounts"))

