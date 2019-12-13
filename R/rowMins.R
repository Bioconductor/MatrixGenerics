#' Calculates the minimum for each row (column) of a matrix-like object
#'
#' Calculates the minimum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowMins
#' @templateVar colName colMins
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowMins}()} and
#'   \code{matrixStats::\link[matrixStats]{colMins}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For max estimates, see \code{\link{rowMaxs}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowMins
#' @export
setGeneric("rowMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMins"),
           signature = "x"
)

#' @rdname rowMins
setMethod("rowMins", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMins(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMins
setMethod("rowMins", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMins(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})




#' @rdname rowMins
#' @name colMins
#' @export
setGeneric("colMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMins"),
           signature = "x"
)

#' @rdname rowMins
setMethod("colMins", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMins(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMins
setMethod("colMins", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMins(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})



