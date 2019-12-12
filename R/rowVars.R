#' Calculates the variance for each row (column) of a matrix-like object
#'
#' Calculates the variance for each row (column) of a matrix-like object.
#' 
#' The S4 methods for \code{x} of type \code{\link[base]{matrix}} or 
#' \code{\link[base]{numeric}} call \code{matrixStats::\link[matrixStats]{rowVars}}
#' / \code{matrixStats::\link[matrixStats]{colVars}}.
#' 
#' @param x An NxK matrix-like object.
#' @param rows,cols A \code{\link[base]{vector}} indicating the subset (and/or 
#'   columns) to operate over. If \code{\link[base]{NULL}}, no subsetting is done.
#' @param na.rm If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s
#' are excluded first, otherwise not.
#' @param center (optional) the center, defaults to the row means
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
#' \item \code{matrixStats::\link[matrixStats]{rowVars}()} and
#'   \code{matrixStats::\link[matrixStats]{colVars}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For standard deviation estimates, see \code{\link{rowSds}()}.
#' }
#'
#' @keywords array iteration robust univar
#'
#' @name rowVars
#' @export
setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowVars"),
           signature = "x"
)

#' @rdname rowVars
setMethod("rowVars", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, dim. = dim(x)){
  matrixStats::rowVars(x, rows = rows, cols = cols, na.rm=na.rm, center = center, dim. = dim.)
})

#' @rdname rowVars
setMethod("rowVars", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, dim. = dim(x)){
  matrixStats::rowVars(x, rows = rows, cols = cols, na.rm=na.rm, center = center, dim. = dim.)
})




#' @rdname rowVars
#' @name colVars
#' @export
setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, ...) standardGeneric("colVars"),
           signature = "x"
)

#' @rdname rowVars
setMethod("colVars", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, dim. = dim(x)){
  matrixStats::colVars(x, rows = rows, cols = cols, na.rm=na.rm, center = center, dim. = dim.)
})

#' @rdname rowVars
setMethod("colVars", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, dim. = dim(x)){
  matrixStats::colVars(x, rows = rows, cols = cols, na.rm=na.rm, center = center, dim. = dim.)
})



