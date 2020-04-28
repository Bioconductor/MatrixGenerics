#' Calculates the weighted median absolute deviation for each row (column) of a
#' matrix-like object
#'
#' Calculates the weighted  median absolute deviation for each row (column) of
#' a matrix-like object.
#'
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
#' 
#' @keywords array iteration robust univar
#'
#' @name rowWeightedMads
#' @export
setGeneric("rowWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, constant = 1.4826, center = NULL, ...) standardGeneric("rowWeightedMads"),
           signature = "x"
)

.default_rowWeightedMads <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,  constant = 1.4826, center = NULL, ...){
  matrixStats::rowWeightedMads(x, w = w, rows = rows, cols = cols, na.rm = na.rm, constant = constant, center = center, ...)
}

#' @rdname rowWeightedMads
setMethod("rowWeightedMads", signature = "matrix", .default_rowWeightedMads)

#' @rdname rowWeightedMads
setMethod("rowWeightedMads", signature = "numeric", .default_rowWeightedMads)

#' @rdname rowWeightedMads
setMethod("rowWeightedMads", signature = "array", .default_rowWeightedMads)




#' @rdname rowWeightedMads
#' @name colWeightedMads
#' @export
setGeneric("colWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, constant = 1.4826, center = NULL, ...) standardGeneric("colWeightedMads"),
           signature = "x"
)

.default_colWeightedMads <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,  constant = 1.4826, center = NULL, ...){
  matrixStats::colWeightedMads(x, w = w, rows = rows, cols = cols, na.rm = na.rm, constant = constant, center = center, ...)
}

#' @rdname rowWeightedMads
setMethod("colWeightedMads", signature = "matrix", .default_colWeightedMads)

#' @rdname rowWeightedMads
setMethod("colWeightedMads", signature = "numeric", .default_colWeightedMads)

#' @rdname rowWeightedMads
setMethod("colWeightedMads", signature = "array", .default_colWeightedMads)



