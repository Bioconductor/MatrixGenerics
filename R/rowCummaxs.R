#' Calculates the cumulative maxima for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative maxima for each row (column) of a matrix-like
#' object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowCummaxs
#' @templateVar colName colCummaxs
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
#' \item \code{matrixStats::\link[matrixStats:rowCumsums]{rowCummaxs}()} and
#'   \code{matrixStats::\link[matrixStats:rowCumsums]{colCummaxs}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For single maximum estimates, see \code{\link{rowMaxs}()}.
#' \item \code{base::\link{cummax}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCummaxs
#' @export
setGeneric("rowCummaxs", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCummaxs"),
           signature = "x"
)

.matrixStats_rowCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::rowCummaxs(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummaxs
setMethod("rowCummaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCummaxs)



#' @rdname rowCummaxs
#' @name colCummaxs
#' @export
setGeneric("colCummaxs", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCummaxs"),
           signature = "x"
)

.matrixStats_colCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCummaxs(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @rdname rowCummaxs
setMethod("colCummaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCummaxs)

