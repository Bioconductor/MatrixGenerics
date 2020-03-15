#' Tabulates the values in a matrix-like object by row (column)
#'
#' Tabulates the values in a matrix-like object by row (column)
#' 
#' @templateVar rowName rowTabulates
#' @templateVar colName colTabulates
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @param values the values to search for.
#'
#' @template returnMatrix_JDim
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowTabulates}()} and
#'   \code{matrixStats::\link[matrixStats]{colTabulates}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link[base]{table}()}
#' }
#' 
#' @examples 
#'   mat <- matrix(rpois(15, lambda = 3), nrow = 5, ncol = 3)
#'   mat[2, 1] <- NA_integer_
#'   mat[3, 3] <- 0L
#'   mat[4, 1] <- 0L
#'   
#'   print(mat)
#'   
#'   rowTabulates(mat)
#'   colTabulates(mat)
#'   
#'   rowTabulates(mat, values = 0)
#'   colTabulates(mat, values = 0)
#'
#' @keywords array iteration robust univar
#'
#' @name rowTabulates
#' @export
setGeneric("rowTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ...) standardGeneric("rowTabulates"),
           signature = "x"
)

.default_rowTabulates <- function(x, rows = NULL, cols = NULL, values = NULL){
  matrixStats::rowTabulates(x, rows = rows, cols = cols, values = values)
}

#' @rdname rowTabulates
setMethod("rowTabulates", signature = "matrix", .default_rowTabulates)

#' @rdname rowTabulates
setMethod("rowTabulates", signature = "numeric", .default_rowTabulates)

#' @rdname rowTabulates
setMethod("rowTabulates", signature = "array", .default_rowTabulates)




#' @rdname rowTabulates
#' @name colTabulates
#' @export
setGeneric("colTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ...) standardGeneric("colTabulates"),
           signature = "x"
)

.default_colTabulates <- function(x, rows = NULL, cols = NULL, values = NULL){
  matrixStats::colTabulates(x, rows = rows, cols = cols, values = values)
}

#' @rdname rowTabulates
setMethod("colTabulates", signature = "matrix", .default_colTabulates)

#' @rdname rowTabulates
setMethod("colTabulates", signature = "numeric", .default_colTabulates)

#' @rdname rowTabulates
setMethod("colTabulates", signature = "array", .default_colTabulates)



