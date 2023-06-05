#' Check if any elements in a row (column) of a matrix-like object is equal to
#' a value
#'
#' Check if any elements in a row (column) of a matrix-like object is equal to
#' a value.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowAnys
#'
#' @templateVar rowName rowAnys
#' @templateVar colName colAnys
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template valueParameter
#' @template dimParameter
#' @template na_rmParameter
#' @template useNamesParameter

#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowAlls]{rowAnys}()} and
#'   \code{matrixStats::\link[matrixStats:rowAlls]{colAnys}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if \emph{all} elements are equal to a value, see
#'   \code{\link{rowAlls}()}.
#' \item \code{base::\link{any}()}.
#' }
#'
#' @template standardExamples
setGeneric("rowAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowAnys"),
           signature = "x"
)

.matrixStats_rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowAnys(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowAnys
setMethod("rowAnys", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowAnys)

#' @export
#' @rdname rowAnys
## Default method with user-friendly fallback mechanism.
setMethod("rowAnys", "ANY", make_default_method_def("rowAnys"))



#' @export
#' @rdname rowAnys
setGeneric("colAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ..., useNames = NA) standardGeneric("colAnys"),
           signature = "x"
)

.matrixStats_colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colAnys(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim., ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowAnys
setMethod("colAnys", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colAnys)

#' @export
#' @rdname rowAnys
## Default method with user-friendly fallback mechanism.
setMethod("colAnys", "ANY", make_default_method_def("colAnys"))

