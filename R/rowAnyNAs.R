#' Check if any elements in a row (column) of a matrix-like object is missing
#'
#' Check if any elements in a row (column) of a matrix-like object is missing.
#' 
#' @templateVar rowName rowAnyNAs
#' @templateVar colName colAnyNAs
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowAnyNAs}()} and
#'   \code{matrixStats::\link[matrixStats]{colAnyNAs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if any element is equal to a value, see 
#'   \code{\link{rowAnys}()}.
#' \item \code{base::\link[base]{is.na}()} and \code{base::\link[base]{any}()}.
#' }
#' 
#' @template standardExamples
#'
#'
#' @name rowAnyNAs
#' @export
setGeneric("rowAnyNAs", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowAnyNAs"),
           signature = "x"
)

.default_rowAnyNAs <- function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowAnyNAs(x, rows = rows, cols = cols, ...)
}

#' @rdname rowAnyNAs
setMethod("rowAnyNAs", signature = "matrix", .default_rowAnyNAs)

#' @rdname rowAnyNAs
setMethod("rowAnyNAs", signature = "numeric", .default_rowAnyNAs)

#' @rdname rowAnyNAs
setMethod("rowAnyNAs", signature = "array", .default_rowAnyNAs)




#' @rdname rowAnyNAs
#' @name colAnyNAs
#' @export
setGeneric("colAnyNAs", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colAnyNAs"),
           signature = "x"
)

.default_colAnyNAs <- function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colAnyNAs(x, rows = rows, cols = cols, ...)
}

#' @rdname rowAnyNAs
setMethod("colAnyNAs", signature = "matrix", .default_colAnyNAs)

#' @rdname rowAnyNAs
setMethod("colAnyNAs", signature = "numeric", .default_colAnyNAs)

#' @rdname rowAnyNAs
setMethod("colAnyNAs", signature = "array", .default_colAnyNAs)



