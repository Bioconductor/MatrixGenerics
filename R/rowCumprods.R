#' Calculates the cumulative product for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative product for each row (column) of a matrix-like
#' object.
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowCumprods
#'
#' @templateVar rowName rowCumprods
#' @templateVar colName colCumprods
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
#' \item \code{matrixStats::\link[matrixStats:rowCumsums]{rowCumprods}()} and
#'   \code{matrixStats::\link[matrixStats:rowCumsums]{colCumprods}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#'  \item \code{base::\link{cumprod}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowCumprods", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCumprods"),
           signature = "x"
)

.matrixStats_rowCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::rowCumprods(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @export
#' @rdname rowCumprods
setMethod("rowCumprods", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCumprods)

#' @export
#' @rdname rowCumprods
## Default method with user-friendly fallback mechanism.
setMethod("rowCumprods", "ANY", make_default_method_def("rowCumprods"))



#' @export
#' @rdname rowCumprods
setGeneric("colCumprods", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCumprods"),
           signature = "x"
)

.matrixStats_colCumprods <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ...){
  matrixStats::colCumprods(x, rows = rows, cols = cols, dim. = dim., ...)
}

#' @export
#' @rdname rowCumprods
setMethod("colCumprods", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCumprods)

#' @export
#' @rdname rowCumprods
## Default method with user-friendly fallback mechanism.
setMethod("colCumprods", "ANY", make_default_method_def("colCumprods"))

