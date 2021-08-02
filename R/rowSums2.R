#' Calculates the sum for each row (column) of a matrix-like object
#'
#' Calculates the sum for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowSums2
#'
#' @templateVar rowName rowSums2
#' @templateVar colName colSums2
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template na_rmParameter
#' @template dimParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowSums2}()} and
#'   \code{matrixStats::\link[matrixStats:rowSums2]{colSums2}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item \code{base::\link{sum}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowSums2", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowSums2"),
           signature = "x"
)

.matrixStats_rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowSums2(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowSums2
setMethod("rowSums2", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowSums2)

#' @export
#' @rdname rowSums2
## Default method with user-friendly fallback mechanism.
setMethod("rowSums2", "ANY", make_default_method_def("rowSums2"))



#' @export
#' @rdname rowSums2
setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colSums2"),
           signature = "x"
)

.matrixStats_colSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ..., useNames = NA){
  matrixStats::colSums2(x, rows = rows, cols = cols, na.rm = na.rm, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowSums2
setMethod("colSums2", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colSums2)

#' @export
#' @rdname rowSums2
## Default method with user-friendly fallback mechanism.
setMethod("colSums2", "ANY", make_default_method_def("colSums2"))

