#' Count how often an element in a row (column) of a matrix-like object is equal to a value
#'
#' Count how often an element in a row (column) of a matrix-like object is equal to a value
#' 
#' @templateVar rowName rowCounts
#' @templateVar colName colCounts
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template valueParameter
#' @template na_rmParameter
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCounts}()} and
#'   \code{matrixStats::\link[matrixStats]{colCounts}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if any element is equal to a value, see \code{\link{rowAnys}()}. To
#'   check if all elements are equal, see \code{\link{rowAlls}()}. 
#' }
#' 
#' @template standardExamples
#' 
#' @examples
#'   rowCounts(mat, value = 0)
#'   colCounts(mat, value = Inf, na.rm = TRUE)
#'
#'
#' @name rowCounts
#' @export
setGeneric("rowCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...) standardGeneric("rowCounts"),
           signature = "x"
)

.default_rowCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowCounts(x, rows = rows, cols = cols, value = value, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowCounts
setMethod("rowCounts", signature = "matrix", .default_rowCounts)

#' @rdname rowCounts
setMethod("rowCounts", signature = "numeric", .default_rowCounts)

#' @rdname rowCounts
setMethod("rowCounts", signature = "array", .default_rowCounts)




#' @rdname rowCounts
#' @name colCounts
#' @export
setGeneric("colCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...) standardGeneric("colCounts"),
           signature = "x"
)

.default_colCounts <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colCounts(x, rows = rows, cols = cols, value = value, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowCounts
setMethod("colCounts", signature = "matrix", .default_colCounts)

#' @rdname rowCounts
setMethod("colCounts", signature = "numeric", .default_colCounts)

#' @rdname rowCounts
setMethod("colCounts", signature = "array", .default_colCounts)



