#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object
#'
#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object.
#' 
#' @templateVar rowName rowRanges
#' @templateVar colName colRanges
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#' 
#' @return a \code{\link[base]{numeric}} \code{Nx2} (\code{Kx2}) 
#'   \code{\link{matrix}}, where N (K) is the number of rows (columns) for
#'   which the ranges are calculated. 
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowRanges}()} and
#'   \code{matrixStats::\link[matrixStats]{colRanges}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For max estimates, see \code{\link{rowMaxs}()}.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' \item \code{base::\link[base]{range}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust
#'
#' @name rowRanges
#' @export
setGeneric("rowRanges", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowRanges"),
           signature = "x"
)

.default_rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x)){
  matrixStats::rowRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim.)
}

#' @rdname rowRanges
setMethod("rowRanges", signature = "matrix", .default_rowRanges)

#' @rdname rowRanges
setMethod("rowRanges", signature = "numeric", .default_rowRanges)

#' @rdname rowRanges
setMethod("rowRanges", signature = "array", .default_rowRanges)




#' @rdname rowRanges
#' @name colRanges
#' @export
setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colRanges"),
           signature = "x"
)

.default_colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x)){
  matrixStats::colRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim.)
}

#' @rdname rowRanges
setMethod("colRanges", signature = "matrix", .default_colRanges)

#' @rdname rowRanges
setMethod("colRanges", signature = "numeric", .default_colRanges)

#' @rdname rowRanges
setMethod("colRanges", signature = "array", .default_colRanges)



