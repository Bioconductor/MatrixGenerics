#' Calculates the median for each row (column) of a matrix-like object
#'
#' Calculates the median for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowMedians
#' @templateVar colName colMedians
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
#' \item \code{matrixStats::\link[matrixStats]{rowMedians}()} and
#'   \code{matrixStats::\link[matrixStats]{colMedians}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowMedians
#' @export
setGeneric("rowMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowMedians"),
           signature = "x"
)

.default_rowMedians <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x)){
  matrixStats::rowMedians(x, rows=rows, cols=cols, na.rm=na.rm, dim.=dim.)
}


#' @rdname rowMedians
setMethod("rowMedians", signature = "matrix", .default_rowMedians)

#' @rdname rowMedians
setMethod("rowMedians", signature = "numeric", .default_rowMedians)

#' @rdname rowMedians
setMethod("rowMedians", signature = "array", .default_rowMedians)




#' @rdname rowMedians
#' @name colMedians
#' @export
setGeneric("colMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colMedians"),
           signature = "x"
)

.default_colMedians <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, dim. = dim(x)){
  matrixStats::colMedians(x, rows = rows, cols = cols, na.rm=na.rm, dim. = dim.)
}

#' @rdname rowMedians
setMethod("colMedians", signature = "matrix", .default_colMedians)

#' @rdname rowMedians
setMethod("colMedians", signature = "numeric", .default_colMedians)

#' @rdname rowMedians
setMethod("colMedians", signature = "array", .default_colMedians)

