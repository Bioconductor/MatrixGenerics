#' Check if any elements in a row (column) of a matrix-like object is missing
#'
#' Check if any elements in a row (column) of a matrix-like object is missing.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowAnyNAs
#'
#' @templateVar rowName rowAnyNAs
#' @templateVar colName colAnyNAs
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template useNamesParameter
#'
#' @template returnVectorLgl
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:anyMissing]{rowAnyNAs}()} and
#'   \code{matrixStats::\link[matrixStats:anyMissing]{colAnyNAs}()} which are
#'   used when the input is a \code{matrix} or \code{numeric} vector.
#' \item For checks if any element is equal to a value, see
#'   \code{\link{rowAnys}()}.
#' \item \code{base::\link{is.na}()} and \code{base::\link{any}()}.
#' }
#'
#' @template standardExamples
setGeneric("rowAnyNAs", function(x, rows = NULL, cols = NULL,  ..., useNames = TRUE) standardGeneric("rowAnyNAs"),
           signature = "x"
)

.matrixStats_rowAnyNAs <- function(x, rows = NULL, cols = NULL, ..., useNames = TRUE){
  matrixStats::rowAnyNAs(x, rows = rows, cols = cols, ..., useNames = useNames)
}

#' @export
#' @rdname rowAnyNAs
setMethod("rowAnyNAs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowAnyNAs)

#' @export
#' @rdname rowAnyNAs
## Default method with user-friendly fallback mechanism.
setMethod("rowAnyNAs", "ANY", make_default_method_def("rowAnyNAs"))



#' @export
#' @rdname rowAnyNAs
setGeneric("colAnyNAs", function(x, rows = NULL, cols = NULL, ..., useNames = TRUE) standardGeneric("colAnyNAs"),
           signature = "x"
)

.matrixStats_colAnyNAs <- function(x, rows = NULL, cols = NULL, ..., useNames = TRUE){
  matrixStats::colAnyNAs(x, rows = rows, cols = cols, ..., useNames = useNames)
}

#' @export
#' @rdname rowAnyNAs
setMethod("colAnyNAs", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colAnyNAs)

#' @export
#' @rdname rowAnyNAs
## Default method with user-friendly fallback mechanism.
setMethod("colAnyNAs", "ANY", make_default_method_def("colAnyNAs"))

