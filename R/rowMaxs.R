#' Calculates the maximum for each row (column) of a matrix-like object
#'
#' Calculates the maximum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowMaxs
#' @templateVar colName colMaxs
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
#' \item \code{matrixStats::\link[matrixStats]{rowMaxs}()} and
#'   \code{matrixStats::\link[matrixStats]{colMaxs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowMaxs
#' @export
setGeneric("rowMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMaxs"),
           signature = "x"
)

.default_rowMaxs <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::rowMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowMaxs
setMethod("rowMaxs", signature = "matrix", .default_rowMaxs)

#' @rdname rowMaxs
setMethod("rowMaxs", signature = "numeric", .default_rowMaxs)

#' @rdname rowMaxs
setMethod("rowMaxs", signature = "array", .default_rowMaxs)




#' @rdname rowMaxs
#' @name colMaxs
#' @export
setGeneric("colMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMaxs"),
           signature = "x"
)

.default_colMaxs <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMaxs(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowMaxs
setMethod("colMaxs", signature = "matrix", .default_colMaxs)

#' @rdname rowMaxs
setMethod("colMaxs", signature = "numeric", .default_colMaxs)

#' @rdname rowMaxs
setMethod("colMaxs", signature = "array", .default_colMaxs)



