#' Check if all elements in a row (column) of a matrix-like object are equal to 
#' a value
#'
#' Check if all elements in a row (column) of a matrix-like object are equal to 
#' a value.
#' 
#' @templateVar rowName rowAlls
#' @templateVar colName colAlls
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
#' \item \code{matrixStats::\link[matrixStats]{rowAlls}()} and
#'   \code{matrixStats::\link[matrixStats]{colAlls}()} which are used when
#'   the input is a \code{\link[base]{matrix}}, \code{\link[base]{array}}, or 
#'   \code{\link[base]{numeric}} vector.
#' \item For checks if \emph{any} element is equal to a value, see 
#'   \code{\link{rowAnys}()}.
#' \item \code{base::\link[base]{all}()}
#' }
#' 
#' @template standardExamples
#'
#'
#' @name rowAlls
#' @export
setGeneric("rowAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("rowAlls"),
           signature = "x"
)

.default_rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x)){
  matrixStats::rowAlls(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim.)
}

#' @rdname rowAlls
setMethod("rowAlls", signature = "matrix", .default_rowAlls)

#' @rdname rowAlls
setMethod("rowAlls", signature = "numeric", .default_rowAlls)

#' @rdname rowAlls
setMethod("rowAlls", signature = "array", .default_rowAlls)




#' @rdname rowAlls
#' @name colAlls
#' @export
setGeneric("colAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, ...) standardGeneric("colAlls"),
           signature = "x"
)

.default_colAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x)){
  matrixStats::colAlls(x, rows = rows, cols = cols, value = value, na.rm = na.rm, dim. = dim.)
}

#' @rdname rowAlls
setMethod("colAlls", signature = "matrix", .default_colAlls)

#' @rdname rowAlls
setMethod("colAlls", signature = "numeric", .default_colAlls)

#' @rdname rowAlls
setMethod("colAlls", signature = "array", .default_colAlls)



