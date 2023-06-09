#' Calculates the weighted median for each row (column) of a matrix-like object
#'
#' Calculates the weighted median for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowWeightedMedians
#'
#' @templateVar rowName rowWeightedMedians
#' @templateVar colName colWeightedMedians
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template weightParam
#' @template na_rmParameter
#' @template useNamesParameter
#'
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowWeightedMedians}()} and
#'   \code{matrixStats::\link[matrixStats:rowWeightedMedians]{colWeightedMedians}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowMedians] for the corresponding unweighted function.
#' }
#'
#' @template weightedExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowWeightedMedians"),
           signature = "x"
)

.matrixStats_rowWeightedMedians <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA){
  matrixStats::rowWeightedMedians(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ..., useNames = (isTRUE(useNames) || !(is.null(w) || isFALSE(useNames))))
}

#' @export
#' @rdname rowWeightedMedians
setMethod("rowWeightedMedians", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowWeightedMedians)

#' @export
#' @rdname rowWeightedMedians
## Default method with user-friendly fallback mechanism.
setMethod("rowWeightedMedians", "ANY", make_default_method_def("rowWeightedMedians"))



#' @export
#' @rdname rowWeightedMedians
setGeneric("colWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colWeightedMedians"),
           signature = "x"
)

.matrixStats_colWeightedMedians <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA){
  matrixStats::colWeightedMedians(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ..., useNames = (isTRUE(useNames) || !(is.null(w) || isFALSE(useNames))))
}

#' @export
#' @rdname rowWeightedMedians
setMethod("colWeightedMedians", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colWeightedMedians)

#' @export
#' @rdname rowWeightedMedians
## Default method with user-friendly fallback mechanism.
setMethod("colWeightedMedians", "ANY", make_default_method_def("colWeightedMedians"))

