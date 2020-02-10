#' Calculates the mean for each row (column) of a matrix-like object
#'
#' Calculates the mean for each row (column) of a matrix-like object.
#'
#' 
#' @templateVar rowName rowMeans2
#' @templateVar colName colMeans2
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
#' \item \code{matrixStats::\link[matrixStats]{rowMeans2}()} and
#'   \code{matrixStats::\link[matrixStats]{colMeans2}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item See also \code{\link[base:colSums]{rowMeans}()} for the
#'   corresponding function in base R.
#' \item For variance estimates, see \code{\link{rowVars}()}.
#' }
#' 
#' @template standardExamples
#'   
#' 
#' @keywords array iteration robust univar
#'
#' @name rowMeans2
#' @export
setGeneric("rowMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMeans2"),
           signature = "x"
)

.default_rowMeans2 <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowMeans2
setMethod("rowMeans2", signature = "matrix", .default_rowMeans2)

#' @rdname rowMeans2
setMethod("rowMeans2", signature = "numeric", .default_rowMeans2)

#' @rdname rowMeans2
setMethod("rowMeans2", signature = "array", .default_rowMeans2)




#' @rdname rowMeans2
#' @name colMeans2
#' @export
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMeans2"),
           signature = "x"
)

.default_colMeans2 <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMeans2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowMeans2
setMethod("colMeans2", signature = "matrix", .default_colMeans2)

#' @rdname rowMeans2
setMethod("colMeans2", signature = "numeric", .default_colMeans2)

#' @rdname rowMeans2
setMethod("colMeans2", signature = "array", .default_colMeans2)



