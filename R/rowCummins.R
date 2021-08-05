#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative minima for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowCummins
#'
#' @templateVar rowName rowCummins
#' @templateVar colName colCummins
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
setGeneric("rowCummins", function(x, rows = NULL, cols = NULL, ..., useNames = NA) standardGeneric("rowCummins"),
           signature = "x"
)

.matrixStats_rowCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowCummins(x, rows = rows, cols = cols, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowCummins
setMethod("rowCummins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCummins)

#' @export
#' @rdname rowCummins
## Default method with user-friendly fallback mechanism.
setMethod("rowCummins", "ANY", make_default_method_def("rowCummins"))



#' @export
#' @rdname rowCummins
setGeneric("colCummins", function(x, rows = NULL, cols = NULL, ..., useNames = NA) standardGeneric("colCummins"),
           signature = "x"
)

.matrixStats_colCummins <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::colCummins(x, rows = rows, cols = cols, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowCummins
setMethod("colCummins", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCummins)

#' @export
#' @rdname rowCummins
## Default method with user-friendly fallback mechanism.
setMethod("colCummins", "ANY", make_default_method_def("colCummins"))

