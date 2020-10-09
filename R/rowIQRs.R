#' Calculates the interquartile range for each row (column) of a matrix-like
#' object
#'
#' Calculates the interquartile range for each row (column) of a matrix-like
#' object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowIQRs
#'
#' @templateVar rowName rowIQRs
#' @templateVar colName colIQRs
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
#' \item \code{matrixStats::\link[matrixStats]{rowIQRs}()} and
#'   \code{matrixStats::\link[matrixStats:rowIQRs]{colIQRs}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For a non-robust analog, see \code{\link{rowSds}()}. For a more
#'  robust version see [rowMads()]
#' \item \code{stats::\link[stats]{IQR}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowIQRs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowIQRs"),
           signature = "x"
)

.matrixStats_rowIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::rowIQRs(x, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @export
#' @rdname rowIQRs
setMethod("rowIQRs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowIQRs)

#' @export
#' @rdname rowIQRs
## Default method with user-friendly fallback mechanism.
setMethod("rowIQRs", "ANY", make_default_method_def("rowIQRs"))



#' @export
#' @rdname rowIQRs
setGeneric("colIQRs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colIQRs"),
           signature = "x"
)

.matrixStats_colIQRs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...){
  matrixStats::colIQRs(x, rows = rows, cols = cols, na.rm = na.rm, ...)
}

#' @export
#' @rdname rowIQRs
setMethod("colIQRs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colIQRs)

#' @export
#' @rdname rowIQRs
## Default method with user-friendly fallback mechanism.
setMethod("colIQRs", "ANY", make_default_method_def("colIQRs"))

