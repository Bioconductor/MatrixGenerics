#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object
#'
#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @templateVar rowName rowRanges
#' @templateVar colName colRanges
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#' 
#' @return a \code{\link{numeric}} \code{Nx2} (\code{Kx2}) 
#'   \code{\link{matrix}}, where N (K) is the number of rows (columns) for
#'   which the ranges are calculated. 
#'
#' @note Unfortunately for the argument list of the \code{rowRanges()}
#'   generic function we cannot follow the scheme used for the other
#'   row/column matrix summarization generic functions. This is because
#'   we need to be compatible with the historic \code{rowRanges()} getter
#'   for \link[SummarizedExperiment]{RangedSummarizedExperiment} objects.
#'   See \code{?SummarizedExperiment::\link[SummarizedExperiment]{rowRanges}}.
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowRanges}()} and
#'   \code{matrixStats::\link[matrixStats:rowRanges]{colRanges}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For max estimates, see \code{\link{rowMaxs}()}.
#' \item For min estimates, see \code{\link{rowMins}()}.
#' \item \code{base::\link{range}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust
#'
#' @name rowRanges
#' @export
setGeneric("rowRanges", function(x, ...) standardGeneric("rowRanges"))

.matrixStats_rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::rowRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowRanges
setMethod("rowRanges", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowRanges)



#' @rdname rowRanges
#' @name colRanges
#' @export
setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colRanges"),
           signature = "x"
)

.matrixStats_colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...){
  matrixStats::colRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ...)
}

#' @rdname rowRanges
setMethod("colRanges", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colRanges)

