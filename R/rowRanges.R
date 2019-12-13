#' Calculates the mininum and maximum for each row (column) of a matrix-like object
#'
#' Calculates the mininum and maximum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowRanges
#' @templateVar colName colRanges
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' 
#' @return a \code{\link[base]{numeric}} \code{Nx2} (\code{Kx2}) \code{\link{matrix}}, where
#'   N (K) is the number of rows (columns) for which the ranges are calculated. 
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowRanges}()} and
#'   \code{matrixStats::\link[matrixStats]{colRanges}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For max estimates, see \code{\link{rowMaxs}()}.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust
#'
#' @name rowRanges
#' @export
setGeneric("rowRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowRanges"),
           signature = "x"
)

#' @rdname rowRanges
setMethod("rowRanges", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowRanges(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowRanges
setMethod("rowRanges", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowRanges(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})




#' @rdname rowRanges
#' @name colRanges
#' @export
setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colRanges"),
           signature = "x"
)

#' @rdname rowRanges
setMethod("colRanges", signature = "matrix", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colRanges(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})

#' @rdname rowRanges
setMethod("colRanges", signature = "numeric", function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colRanges(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
})



