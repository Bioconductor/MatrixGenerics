#' Calculates the cumulative maxima for each row (column) of a matrix-like object
#'
#' Calculates the cumulative maxima for each row (column) of a matrix-like object
#' 
#' @templateVar rowName rowCumprods
#' @templateVar colName colCumprods
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template dimParameter
#'
#' @template returnMatrix_SameDimX
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCumprods}()} and
#'   \code{matrixStats::\link[matrixStats]{colCumprods}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCumprods
#' @export
setGeneric("rowCumprods", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCumprods"),
           signature = "x"
)

.default_rowCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x)){
  matrixStats::rowCumprods(x, rows = rows, cols = cols, dim. = dim.)
}

#' @rdname rowCumprods
setMethod("rowCumprods", signature = "matrix", .default_rowCumprods)

#' @rdname rowCumprods
setMethod("rowCumprods", signature = "numeric", .default_rowCumprods)

#' @rdname rowCumprods
setMethod("rowCumprods", signature = "array", .default_rowCumprods)




#' @rdname rowCumprods
#' @name colCumprods
#' @export
setGeneric("colCumprods", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCumprods"),
           signature = "x"
)

.default_colCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x)){
  matrixStats::colCumprods(x, rows = rows, cols = cols, dim. = dim.)
}

#' @rdname rowCumprods
setMethod("colCumprods", signature = "matrix", .default_colCumprods)

#' @rdname rowCumprods
setMethod("colCumprods", signature = "numeric", .default_colCumprods)

#' @rdname rowCumprods
setMethod("colCumprods", signature = "array", .default_colCumprods)



