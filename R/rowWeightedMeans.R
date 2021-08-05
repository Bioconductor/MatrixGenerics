#' Calculates the weighted mean for each row (column) of a matrix-like object
#'
#' Calculates the weighted  mean for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowWeightedMeans
#'
#' @templateVar rowName rowWeightedMeans
#' @templateVar colName colWeightedMeans
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template weightParam
#' @template na_rmParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowWeightedMeans}()} and
#'   \code{matrixStats::\link[matrixStats:rowWeightedMeans]{colWeightedMeans}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowMeans2] for the corresponding unweighted function.
#' }
#'
#' @template weightedExamples
#'
#'
#' @keywords array iteration robust univar
setGeneric("rowWeightedMeans", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowWeightedMeans"),
           signature = "x"
)

.matrixStats_rowWeightedMeans <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA){
  matrixStats::rowWeightedMeans(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ..., useNames = useNames)
}

#' @export
#' @rdname rowWeightedMeans
setMethod("rowWeightedMeans", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowWeightedMeans)

#' @export
#' @rdname rowWeightedMeans
## Default method with user-friendly fallback mechanism.
setMethod("rowWeightedMeans", "ANY", make_default_method_def("rowWeightedMeans"))



#' @export
#' @rdname rowWeightedMeans
setGeneric("colWeightedMeans", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colWeightedMeans"),
           signature = "x"
)

.matrixStats_colWeightedMeans <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA){
  matrixStats::colWeightedMeans(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ..., useNames = useNames)
}

#' @export
#' @rdname rowWeightedMeans
setMethod("colWeightedMeans", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colWeightedMeans)

#' @export
#' @rdname rowWeightedMeans
## Default method with user-friendly fallback mechanism.
setMethod("colWeightedMeans", "ANY", make_default_method_def("colWeightedMeans"))

