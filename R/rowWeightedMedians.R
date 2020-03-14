#' Calculates the weighted medioan for each row (column) of a matrix-like object
#'
#' Calculates the weighted medioan for each row (column) of a matrix-like object.
#'
#' 
#' @templateVar rowName rowWeightedMedians
#' @templateVar colName colWeightedMedians
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
#' \item \code{matrixStats::\link[matrixStats]{rowWeightedMedians}()} and
#'   \code{matrixStats::\link[matrixStats]{colWeightedMedians}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item See also [rowMedians] for the corresponding unweighted function.
#' }
#' 
#' @template weightedExamples
#'   
#' 
#' @keywords array iteration robust univar
#'
#' @name rowWeightedMedians
#' @export
setGeneric("rowWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowWeightedMedians"),
           signature = "x"
)

.default_rowWeightedMedians <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  matrixStats::rowWeightedMedians(x, w = w, rows = rows, cols = cols, na.rm=na.rm)
}

#' @rdname rowWeightedMedians
setMethod("rowWeightedMedians", signature = "matrix", .default_rowWeightedMedians)

#' @rdname rowWeightedMedians
setMethod("rowWeightedMedians", signature = "numeric", .default_rowWeightedMedians)

#' @rdname rowWeightedMedians
setMethod("rowWeightedMedians", signature = "array", .default_rowWeightedMedians)




#' @rdname rowWeightedMedians
#' @name colWeightedMedians
#' @export
setGeneric("colWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colWeightedMedians"),
           signature = "x"
)

.default_colWeightedMedians <- function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  matrixStats::colWeightedMedians(x, w = w, rows = rows, cols = cols, na.rm=na.rm)
}

#' @rdname rowWeightedMedians
setMethod("colWeightedMedians", signature = "matrix", .default_colWeightedMedians)

#' @rdname rowWeightedMedians
setMethod("colWeightedMedians", signature = "numeric", .default_colWeightedMedians)

#' @rdname rowWeightedMedians
setMethod("colWeightedMedians", signature = "array", .default_colWeightedMedians)



