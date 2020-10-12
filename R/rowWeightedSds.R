#' Calculates the weighted standard deviation for each row (column) of a
#' matrix-like object
#'
#' Calculates the weighted standard deviation for each row (column) of a
#' matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowWeightedSds
#' 
#' @templateVar rowName rowWeightedSds
#' @templateVar colName colWeightedSds
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template weightParam
#' @template na_rmParameter
#' 
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:weightedVar]{rowWeightedSds}()} and
#'   \code{matrixStats::\link[matrixStats:weightedVar]{colWeightedSds}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowSds] for the corresponding unweighted function.
#' }
#' 
#' @template weightedExamples
#' 
#' @keywords array iteration robust univar
setGeneric("rowWeightedSds", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowWeightedSds"),
           signature = "x"
)

.matrixStats_rowWeightedSds <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::rowWeightedSds(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @export
#' @rdname rowWeightedSds
setMethod("rowWeightedSds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowWeightedSds)

#' @export
#' @rdname rowWeightedSds
## Default method with user-friendly fallback mechanism.
setMethod("rowWeightedSds", "ANY", make_default_method_def("rowWeightedSds"))



#' @export
#' @rdname rowWeightedSds
setGeneric("colWeightedSds", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colWeightedSds"),
           signature = "x"
)

.matrixStats_colWeightedSds <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::colWeightedSds(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @export
#' @rdname rowWeightedSds
setMethod("colWeightedSds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colWeightedSds)

#' @export
#' @rdname rowWeightedSds
## Default method with user-friendly fallback mechanism.
setMethod("colWeightedSds", "ANY", make_default_method_def("colWeightedSds"))

