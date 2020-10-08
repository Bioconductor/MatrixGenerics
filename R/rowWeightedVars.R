#' Calculates the weighted variance for each row (column) of a matrix-like
#' object
#'
#' Calculates the weighted variance for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowWeightedVars
#' @templateVar colName colWeightedVars
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
#' \item \code{matrixStats::\link[matrixStats:weightedVar]{rowWeightedVars}()} and
#'   \code{matrixStats::\link[matrixStats:weightedVar]{colWeightedVars}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowVars] for the corresponding unweighted function.
#' }
#' 
#' @template weightedExamples
#'   
#' 
#' @keywords array iteration robust univar
#'
#' @name rowWeightedVars
#' @export
setGeneric("rowWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowWeightedVars"),
           signature = "x"
)

.matrixStats_rowWeightedVars <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::rowWeightedVars(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowWeightedVars
setMethod("rowWeightedVars", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowWeightedVars)



#' @rdname rowWeightedVars
#' @name colWeightedVars
#' @export
setGeneric("colWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colWeightedVars"),
           signature = "x"
)

.matrixStats_colWeightedVars <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::colWeightedVars(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowWeightedVars
setMethod("colWeightedVars", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colWeightedVars)

