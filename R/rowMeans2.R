#' Calculates the mean for each row (column) of a matrix-like object
#'
#' Calculates the mean for each row (column) of a matrix-like object.
#' 
#' The S4 methods for \code{x} of type \code{\link[base]{matrix}} or 
#' \code{\link[base]{numeric}} call \code{matrixStats::\link[matrixStats]{rowMeans2}}
#' / \code{matrixStats::\link[matrixStats]{colMeans2}}.
#' 
#' @param x An NxK matrix-like object.
#' @param rows,cols A \code{\link[base]{vector}} indicating the subset (and/or 
#'   columns) to operate over. If \code{\link[base]{NULL}}, no subsetting is done.
#' @param na.rm If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s
#' are excluded first, otherwise not.
#' @param dim. An \code{\link[base]{integer}} \code{\link[base]{vector}} of
#' length two specifying the dimension of \code{x}, essential when x is a
#' \code{\link[base]{numeric}} vector.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' length N (K).
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowMeans2}()} and
#'   \code{matrixStats::\link[matrixStats]{colMeans2}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item See also \code{\link[base:colSums]{rowMeans}()} for the
#'   corresponding function in base R.
#' \item For variance estimates, see \code{\link{rowVars}()}.
#' }
#'
#' @keywords array iteration robust univar
#'
#' @name rowMeans2
#' @export
setGeneric("rowMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMeans2"),
           signature = "x"
)

#' @rdname rowMeans2
setMethod("rowMeans2", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMeans2
setMethod("rowMeans2", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})




#' @rdname rowMeans2
#' @name colMeans2
#' @export
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMeans2"),
           signature = "x"
)

#' @rdname rowMeans2
setMethod("colMeans2", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMeans2
setMethod("colMeans2", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})



