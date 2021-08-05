#' Calculates the cumulative maxima for each row (column) of a matrix-like
#' object
#'
#' Calculates the cumulative maxima for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowCummaxs
#'
#' @templateVar rowName rowCummaxs
#' @templateVar colName colCummaxs
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
setGeneric("rowCummaxs", function(x, rows = NULL, cols = NULL, ..., useNames = NA) standardGeneric("rowCummaxs"),
           signature = "x"
)

.matrixStats_rowCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowCummaxs(x, rows = rows, cols = cols, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowCummaxs
setMethod("rowCummaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowCummaxs)

#' @export
#' @rdname rowCummaxs
## Default method with user-friendly fallback mechanism.
setMethod("rowCummaxs", "ANY", make_default_method_def("rowCummaxs"))



#' @export
#' @rdname rowCummaxs
setGeneric("colCummaxs", function(x, rows = NULL, cols = NULL, ..., useNames = NA) standardGeneric("colCummaxs"),
           signature = "x"
)

.matrixStats_colCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::colCummaxs(x, rows = rows, cols = cols, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowCummaxs
setMethod("colCummaxs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colCummaxs)

#' @export
#' @rdname rowCummaxs
## Default method with user-friendly fallback mechanism.
setMethod("colCummaxs", "ANY", make_default_method_def("colCummaxs"))

