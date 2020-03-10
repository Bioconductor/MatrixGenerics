#' Check if any elements in a row (column) of a matrix-like object is equal to a value
#'
#' Check if any elements in a row (column) of a matrix-like object is equal to a value
#' 
#' @templateVar rowName rowAnys
#' @templateVar colName colAnys
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template valueParameter
#' @template dimParameter
#' @template na_rmParameter
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowAnys}()} and
#'   \code{matrixStats::\link[matrixStats]{colAnys}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if any element is equal to a value, see \code{\link{rowAnys}()}.
#' }
#' 
#' @template standardExamples
#'
#'
#' @name rowAnys
#' @export
setGeneric("rowAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...) standardGeneric("rowAnys"),
           signature = "x"
)

.default_rowAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowAnys(x, rows = rows, cols = cols, value = value, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowAnys
setMethod("rowAnys", signature = "matrix", .default_rowAnys)

#' @rdname rowAnys
setMethod("rowAnys", signature = "numeric", .default_rowAnys)

#' @rdname rowAnys
setMethod("rowAnys", signature = "array", .default_rowAnys)




#' @rdname rowAnys
#' @name colAnys
#' @export
setGeneric("colAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...) standardGeneric("colAnys"),
           signature = "x"
)

.default_colAnys <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colAnys(x, rows = rows, cols = cols, value = value, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowAnys
setMethod("colAnys", signature = "matrix", .default_colAnys)

#' @rdname rowAnys
setMethod("colAnys", signature = "numeric", .default_colAnys)

#' @rdname rowAnys
setMethod("colAnys", signature = "array", .default_colAnys)



