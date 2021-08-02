#' Calculates the cumulative sum for each row (column) of a matrix-like object
#'
#' Calculates the cumulative sum for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowCumsums
#'
#' @templateVar rowName rowCumsums
#' @templateVar colName colCumsums
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template dimParameter
#' @template useNamesParameter
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
setGeneric("rowCumsums", function(x, rows = NULL, cols = NULL,  ..., useNames = NA) standardGeneric("rowCumsums"),
           signature = "x"
)

.matrixStats_rowCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowCumsums(x, rows = rows, cols = cols, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowCumsums
setMethod("rowCumsums", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCumsums)

#' @export
#' @rdname rowCumsums
## Default method with user-friendly fallback mechanism.
setMethod("rowCumsums", "ANY", make_default_method_def("rowCumsums"))



#' @export
#' @rdname rowCumsums
setGeneric("colCumsums", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCumsums"),
           signature = "x"
)

.matrixStats_colCumsums <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::colCumsums(x, rows = rows, cols = cols, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowCumsums
setMethod("colCumsums", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCumsums)

#' @export
#' @rdname rowCumsums
## Default method with user-friendly fallback mechanism.
setMethod("colCumsums", "ANY", make_default_method_def("colCumsums"))

