#' Calculates the variance for each row (column) of a matrix-like object
#'
#' Calculates the variance for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowVars
#' @templateVar colName colVars
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means.
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowVars}()} and
#'   \code{matrixStats::\link[matrixStats]{colVars}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For standard deviation estimates, see \code{\link{rowSds}()}.
#' \item \code{stats::\link[stats]{var}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowVars
#' @importFrom matrixStats colVars rowVars
#' @export
setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("rowVars"),
           signature = "x"
)

.default_rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::rowVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @rdname rowVars
setMethod("rowVars", signature = "matrix", .default_rowVars)

#' @rdname rowVars
setMethod("rowVars", signature = "numeric", .default_rowVars)

#' @rdname rowVars
setMethod("rowVars", signature = "array", .default_rowVars)




#' @rdname rowVars
#' @name colVars
#' @export
setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("colVars"),
           signature = "x"
)

.default_colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::colVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @rdname rowVars
setMethod("colVars", signature = "matrix", .default_colVars)

#' @rdname rowVars
setMethod("colVars", signature = "numeric", .default_colVars)

#' @rdname rowVars
setMethod("colVars", signature = "array", .default_colVars)



