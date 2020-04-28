#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object.
#' 
#' @templateVar rowName rowCummins
#' @templateVar colName colCummins
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
#' \item \code{matrixStats::\link[matrixStats:rowCumsums]{rowCummins}()} and
#'   \code{matrixStats::\link[matrixStats:rowCumsums]{colCummins}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For single minimum estimates, see \code{\link{rowMins}()}.
#' \item \code{base::\link{cummin}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCummins
#' @export
setGeneric("rowCummins", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCummins"),
           signature = "x"
)

.default_rowCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::rowCummins(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummins
setMethod("rowCummins", signature = "matrix", .default_rowCummins)

#' @rdname rowCummins
setMethod("rowCummins", signature = "numeric", .default_rowCummins)

#' @rdname rowCummins
setMethod("rowCummins", signature = "array", .default_rowCummins)




#' @rdname rowCummins
#' @name colCummins
#' @export
setGeneric("colCummins", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCummins"),
           signature = "x"
)

.default_colCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCummins(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummins
setMethod("colCummins", signature = "matrix", .default_colCummins)

#' @rdname rowCummins
setMethod("colCummins", signature = "numeric", .default_colCummins)

#' @rdname rowCummins
setMethod("colCummins", signature = "array", .default_colCummins)



