#' Calculates the weighted variance for each row (column) of a matrix-like
#' object
#'
#' Calculates the weighted variance for each row (column) of a matrix-like
#' object.
#'
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
#' \item \code{matrixStats::\link[matrixStats]{rowWeightedVars}()} and
#'   \code{matrixStats::\link[matrixStats]{colWeightedVars}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowVars] for the corresponding unweighted function.
#' }
#' 
#' @template weightedExamples
#'   
#' 
#' @keywords array iteration robust univar
#'
#' @name rowWeightedVars
#' @importFrom matrixStats colWeightedVars rowWeightedVars
#' @export
setGeneric("rowWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowWeightedVars"),
           signature = "x"
)

.default_rowWeightedVars <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::rowWeightedVars(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowWeightedVars
setMethod("rowWeightedVars", signature = "matrix", .default_rowWeightedVars)

#' @rdname rowWeightedVars
setMethod("rowWeightedVars", signature = "numeric", .default_rowWeightedVars)

#' @rdname rowWeightedVars
setMethod("rowWeightedVars", signature = "array", .default_rowWeightedVars)




#' @rdname rowWeightedVars
#' @name colWeightedVars
#' @export
setGeneric("colWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colWeightedVars"),
           signature = "x"
)

.default_colWeightedVars <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::colWeightedVars(x, w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @rdname rowWeightedVars
setMethod("colWeightedVars", signature = "matrix", .default_colWeightedVars)

#' @rdname rowWeightedVars
setMethod("colWeightedVars", signature = "numeric", .default_colWeightedVars)

#' @rdname rowWeightedVars
setMethod("colWeightedVars", signature = "array", .default_colWeightedVars)



