#' Calculates the weighted median absolute deviation for each row (column) of a
#' matrix-like object
#'
#' Calculates the weighted  median absolute deviation for each row (column) of
#' a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowWeightedMads
#' 
#' @templateVar rowName rowWeightedMads
#' @templateVar colName colWeightedMads
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template weightParam
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means
#' @param constant A scale factor. See \code{stats::\link[stats]{mad}()} for
#'   details.
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:weightedMad]{rowWeightedMads}()} and
#'   \code{matrixStats::\link[matrixStats:weightedMad]{colWeightedMads}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowMads] for the corresponding unweighted function.
#' }
#' 
#' @template weightedExamples
#' 
#' @keywords array iteration robust univar
setGeneric("rowWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, constant = 1.4826, center = NULL, ...) standardGeneric("rowWeightedMads"),
           signature = "x"
)

.matrixStats_rowWeightedMads <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,  constant = 1.4826, center = NULL, ...){
  matrixStats::rowWeightedMads(x, w = w, rows = rows, cols = cols, na.rm = na.rm, constant = constant, center = center, ...)
}

#' @export
#' @rdname rowWeightedMads
setMethod("rowWeightedMads", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowWeightedMads)

#' @export
#' @rdname rowWeightedMads
## Default method with user-friendly fallback mechanism.
setMethod("rowWeightedMads", "ANY", make_default_method_def("rowWeightedMads"))



#' @export
#' @rdname rowWeightedMads
setGeneric("colWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, constant = 1.4826, center = NULL, ...) standardGeneric("colWeightedMads"),
           signature = "x"
)

.matrixStats_colWeightedMads <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,  constant = 1.4826, center = NULL, ...){
  matrixStats::colWeightedMads(x, w = w, rows = rows, cols = cols, na.rm = na.rm, constant = constant, center = center, ...)
}

#' @export
#' @rdname rowWeightedMads
setMethod("colWeightedMads", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colWeightedMads)

#' @export
#' @rdname rowWeightedMads
## Default method with user-friendly fallback mechanism.
setMethod("colWeightedMads", "ANY", make_default_method_def("colWeightedMads"))

