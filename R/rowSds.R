#' Calculates the standard deviation for each row (column) of a matrix-like
#' object
#'
#' Calculates the standard deviation for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowSds
#'
#' @templateVar rowName rowSds
#' @templateVar colName colSds
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means
#' @template dimParameter
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowSds}()} and
#'   \code{matrixStats::\link[matrixStats:rowSds]{colSds}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For variance estimates, see \code{\link{rowVars}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowSds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ..., useNames = NA) standardGeneric("rowSds"),
           signature = "x"
)

.matrixStats_rowSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::rowSds(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowSds
setMethod("rowSds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowSds)

#' @export
#' @rdname rowSds
## Default method with user-friendly fallback mechanism.
setMethod("rowSds", "ANY", make_default_method_def("rowSds"))



#' @export
#' @rdname rowSds
setGeneric("colSds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ..., useNames = NA) standardGeneric("colSds"),
           signature = "x"
)

.matrixStats_colSds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ..., useNames = NA){
  matrixStats::colSds(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ..., useNames = NA)
}

#' @export
#' @rdname rowSds
setMethod("colSds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colSds)

#' @export
#' @rdname rowSds
## Default method with user-friendly fallback mechanism.
setMethod("colSds", "ANY", make_default_method_def("colSds"))

