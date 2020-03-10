#' Calculates the sum for each row (column) of a matrix-like object
#'
#' Calculates the sum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowSums2
#' @templateVar colName colSums2
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowSums2}()} and
#'   \code{matrixStats::\link[matrixStats]{colSums2}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowSums2
#' @export
setGeneric("rowSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowSums2"),
           signature = "x"
)

.default_rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowSums2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowSums2
setMethod("rowSums2", signature = "matrix", .default_rowSums2)

#' @rdname rowSums2
setMethod("rowSums2", signature = "numeric", .default_rowSums2)

#' @rdname rowSums2
setMethod("rowSums2", signature = "array", .default_rowSums2)




#' @rdname rowSums2
#' @name colSums2
#' @export
setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colSums2"),
           signature = "x"
)

.default_colSums2 <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colSums2(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowSums2
setMethod("colSums2", signature = "matrix", .default_colSums2)

#' @rdname rowSums2
setMethod("colSums2", signature = "numeric", .default_colSums2)

#' @rdname rowSums2
setMethod("colSums2", signature = "array", .default_colSums2)



