#' Calculates the median for each row (column) of a matrix-like object
#'
#' Calculates the median for each row (column) of a matrix-like object.
#' 
#' The S4 methods for \code{x} of type \code{\link[base]{matrix}} or 
#' \code{\link[base]{numeric}} call \code{matrixStats::\link[matrixStats]{rowMedians}}
#' / \code{matrixStats::\link[matrixStats]{colMedians}}.
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
#' \item \code{matrixStats::\link[matrixStats]{rowMedians}()} and
#'   \code{matrixStats::\link[matrixStats]{colMedians}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#'
#' @keywords array iteration robust univar
#'
#' @name rowMedians
#' @export
setGeneric("rowMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMedians"),
           signature = "x"
)

#' @rdname rowMedians
setMethod("rowMedians", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMedians(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMedians
setMethod("rowMedians", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMedians(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})




#' @rdname rowMedians
#' @name colMedians
#' @export
setGeneric("colMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMedians"),
           signature = "x"
)

#' @rdname rowMedians
setMethod("colMedians", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMedians(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowMedians
setMethod("colMedians", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMedians(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})



