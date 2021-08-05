#' Calculates the median for each row (column) of a matrix-like object
#'
#' Calculates the median for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMedians
#'
#' @templateVar rowName rowMedians
#' @templateVar colName colMedians
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
#' \item \code{matrixStats::\link[matrixStats]{rowMedians}()} and
#'   \code{matrixStats::\link[matrixStats:rowMedians]{colMedians}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowMedians", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowMedians"),
           signature = "x"
)

.matrixStats_rowMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowMedians(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowMedians
setMethod("rowMedians", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowMedians)

#' @export
#' @rdname rowMedians
## Default method with user-friendly fallback mechanism.
setMethod("rowMedians", "ANY", make_default_method_def("rowMedians"))



#' @export
#' @rdname rowMedians
setGeneric("colMedians", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colMedians"),
           signature = "x"
)

.matrixStats_colMedians <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colMedians(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowMedians
setMethod("colMedians", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colMedians)

#' @export
#' @rdname rowMedians
## Default method with user-friendly fallback mechanism.
setMethod("colMedians", "ANY", make_default_method_def("colMedians"))

