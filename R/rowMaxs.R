#' Calculates the maximum for each row (column) of a matrix-like object
#'
#' Calculates the maximum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowMaxs
#' @templateVar colName colMaxs
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
#' \item \code{matrixStats::\link[matrixStats]{rowMaxs}()} and
#'   \code{matrixStats::\link[matrixStats]{colMaxs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowMaxs
#' @export
setGeneric("rowMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMaxs"),
           signature = "x"
)

#' @rdname rowMaxs
setMethod("rowMaxs", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMaxs
setMethod("rowMaxs", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})




#' @rdname rowMaxs
#' @name colMaxs
#' @export
setGeneric("colMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMaxs"),
           signature = "x"
)

#' @rdname rowMaxs
setMethod("colMaxs", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMaxs
setMethod("colMaxs", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})



