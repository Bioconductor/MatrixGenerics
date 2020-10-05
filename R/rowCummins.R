#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowCummins
#' @templateVar colName colCummins
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
#' \item \code{matrixStats::\link[matrixStats:rowCumsums]{rowCummins}()} and
#'   \code{matrixStats::\link[matrixStats:rowCumsums]{colCummins}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For single minimum estimates, see \code{\link{rowMins}()}.
#' \item \code{base::\link{cummin}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCummins
#' @export
setGeneric("rowCummins", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCummins"),
           signature = "x"
)

.matrixStats_rowCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::rowCummins(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummins
setMethod("rowCummins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCummins)



#' @rdname rowCummins
#' @name colCummins
#' @export
setGeneric("colCummins", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCummins"),
           signature = "x"
)

.matrixStats_colCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCummins(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummins
setMethod("colCummins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCummins)

