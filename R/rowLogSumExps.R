#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object
#'
#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowLogSumExps
#'
#' @templateVar rowName rowLogSumExps
#' @templateVar colName colLogSumExps
#'
#' @template matrixStatsLink
#'
#' @param lx An NxK matrix-like object. Typically `lx` are `log(x)` values.
#' @param rows,cols A \code{\link{vector}} indicating the subset (and/or
#' columns) to operate over. If \code{\link{NULL}}, no subsetting is done.
#' @param ... Additional arguments passed to specific methods.
#' @template na_rmParameter
#' @template dimParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowLogSumExps}()} and
#'   \code{matrixStats::\link[matrixStats:rowLogSumExps]{colLogSumExps}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item [rowSums2()]
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowLogSumExps"),
           signature = "lx"
)

.matrixStats_rowLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ..., useNames = NA){
  matrixStats::rowLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowLogSumExps
setMethod("rowLogSumExps", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowLogSumExps)

#' @export
#' @rdname rowLogSumExps
## Default method with user-friendly fallback mechanism.
setMethod("rowLogSumExps", "ANY", make_default_method_def("rowLogSumExps"))



#' @export
#' @rdname rowLogSumExps
setGeneric("colLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colLogSumExps"),
           signature = "lx"
)

.matrixStats_colLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ..., useNames = NA){
  matrixStats::colLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowLogSumExps
setMethod("colLogSumExps", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colLogSumExps)

#' @export
#' @rdname rowLogSumExps
## Default method with user-friendly fallback mechanism.
setMethod("colLogSumExps", "ANY", make_default_method_def("colLogSumExps"))

