#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object
#'
#' Accurately calculates the logarithm of the sum of exponentials for each row
#' (column) of a matrix-like object.
#' 
#' @templateVar rowName rowLogSumExps
#' @templateVar colName colLogSumExps
#' 
#' @template matrixStatsLink
#' 
#' @param lx An NxK matrix-like object. Typically `lx` are `log(x)` values.
#' @param rows,cols A \code{\link[base]{vector}} indicating the subset (and/or 
#'   columns) to operate over. If \code{\link[base]{NULL}}, no subsetting is
#'   done.
#' @param ... Additional arguments passed to specific methods.
#' @template na_rmParameter
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowLogSumExps}()} and
#'   \code{matrixStats::\link[matrixStats]{colLogSumExps}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item [rowSums2()]
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowLogSumExps
#' @importFrom matrixStats colLogSumExps rowLogSumExps
#' @export
setGeneric("rowLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowLogSumExps"),
           signature = "lx"
)

.default_rowLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ...){
  matrixStats::rowLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowLogSumExps
setMethod("rowLogSumExps", signature = "matrix", .default_rowLogSumExps)

#' @rdname rowLogSumExps
setMethod("rowLogSumExps", signature = "numeric", .default_rowLogSumExps)

#' @rdname rowLogSumExps
setMethod("rowLogSumExps", signature = "array", .default_rowLogSumExps)




#' @rdname rowLogSumExps
#' @name colLogSumExps
#' @export
setGeneric("colLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colLogSumExps"),
           signature = "lx"
)

.default_colLogSumExps <- function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx), ...){
  matrixStats::colLogSumExps(lx, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowLogSumExps
setMethod("colLogSumExps", signature = "matrix", .default_colLogSumExps)

#' @rdname rowLogSumExps
setMethod("colLogSumExps", signature = "numeric", .default_colLogSumExps)

#' @rdname rowLogSumExps
setMethod("colLogSumExps", signature = "array", .default_colLogSumExps)



