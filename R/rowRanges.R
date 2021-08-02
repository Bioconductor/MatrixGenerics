#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object
#'
#' Calculates the minimum and maximum for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowRanges
#'
#' @templateVar rowName rowRanges
#' @templateVar colName colRanges
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#' @template useNamesParameter
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
setGeneric("rowRanges", function(x, ...) standardGeneric("rowRanges"))

.matrixStats_rowRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowRanges
setMethod("rowRanges", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowRanges)

## Note that because the rowRanges() accessor for SummarizedExperiment
## objects (and other objects in Bioconductor) is implemented as a method
## for the MatrixGenerics::rowRanges() generic, the user-friendly fallback
## mechanism for rowRanges() could produce an error message like:
##
##   Error in MatrixGenerics:::.load_next_suggested_package_to_search(x) :
##     Failed to find a rowRanges() method for <SomeClass> objects.
##     However, the following package is likely to contain the missing
##     method but is not installed: sparseMatrixStats.
##     Please install it (with 'BiocManager::install("sparseMatrixStats")')
##     and try again.
##
## in the (admittedly rare) situations where the user tries to call the
## accessor on a SummarizedExperiment or RaggedExperiment object etc.. but
## doesn't have the SummarizedExperiment or RaggedExperiment package loaded.
## Not clear that this can even happen, but if it did, the error message
## would be quite misleading.
#' @export
#' @rdname rowRanges
## Default method with user-friendly fallback mechanism.
setMethod("rowRanges", "ANY", make_default_method_def("rowRanges"))



#' @export
#' @rdname rowRanges
setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colRanges"),
           signature = "x"
)

.matrixStats_colRanges <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colRanges(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowRanges
setMethod("colRanges", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colRanges)

#' @export
#' @rdname rowRanges
## Default method with user-friendly fallback mechanism.
setMethod("colRanges", "ANY", make_default_method_def("colRanges"))

