#' Calculates the cumulative sum for each row (column) of a matrix-like object
#'
#' Calculates the cumulative sum for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowCumsums
#' @templateVar colName colCumsums
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template dimParameter
#'
#' @template returnMatrix_SameDimX
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCumsums}()} and
#'   \code{matrixStats::\link[matrixStats:rowCumsums]{colCumsums}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item \code{base::\link{cumsum}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCumsums
#' @export
setGeneric("rowCumsums", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCumsums"),
           signature = "x"
)

.default_rowCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::rowCumsums(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCumsums
setMethod("rowCumsums", signature = "matrix", .default_rowCumsums)

#' @rdname rowCumsums
setMethod("rowCumsums", signature = "numeric", .default_rowCumsums)

#' @rdname rowCumsums
setMethod("rowCumsums", signature = "array", .default_rowCumsums)




#' @rdname rowCumsums
#' @name colCumsums
#' @export
setGeneric("colCumsums", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCumsums"),
           signature = "x"
)

.default_colCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCumsums(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCumsums
setMethod("colCumsums", signature = "matrix", .default_colCumsums)

#' @rdname rowCumsums
setMethod("colCumsums", signature = "numeric", .default_colCumsums)

#' @rdname rowCumsums
setMethod("colCumsums", signature = "array", .default_colCumsums)



