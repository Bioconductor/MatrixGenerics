#' Calculates the product for each row (column) of a matrix-like object
#'
#' Calculates the product for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowProds
#'
#' @templateVar rowName rowProds
#' @templateVar colName colProds
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @template na_rmParameter
#' @param method A character vector of length one that specifies the
#'   how the product is calculated. Note, that this is not a generic
#'   argument and not all implementation have to provide it.
#' @template useNamesParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowProds}()} and
#'   \code{matrixStats::\link[matrixStats:rowProds]{colProds}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For sums across rows (columns), see
#'   \code{\link{rowSums2}()} ([colSums2()])
#' \item \code{base::\link{prod}()}.
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust univar
setGeneric("rowProds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("rowProds"),
           signature = "x"
)

.matrixStats_rowProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ..., useNames = NA){
  matrixStats::rowProds(x, rows = rows, cols = cols, na.rm = na.rm, method = method, ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowProds
setMethod("rowProds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowProds)

#' @export
#' @rdname rowProds
## Default method with user-friendly fallback mechanism.
setMethod("rowProds", "ANY", make_default_method_def("rowProds"))



#' @export
#' @rdname rowProds
setGeneric("colProds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ..., useNames = NA) standardGeneric("colProds"),
           signature = "x"
)

.matrixStats_colProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ..., useNames = NA){
  matrixStats::colProds(x, rows = rows, cols = cols, na.rm = na.rm, method = method, ..., useNames = isTRUE(useNames))
}

#' @export
#' @rdname rowProds
setMethod("colProds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colProds)

#' @export
#' @rdname rowProds
## Default method with user-friendly fallback mechanism.
setMethod("colProds", "ANY", make_default_method_def("colProds"))

