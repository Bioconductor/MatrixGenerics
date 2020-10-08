#' Calculates the product for each row (column) of a matrix-like object
#'
#' Calculates the product for each row (column) of a matrix-like object.
#' 
#' @include MatrixGenerics-package.R
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
#'
#' @name rowProds
#' @export
setGeneric("rowProds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("rowProds"),
           signature = "x"
)

.matrixStats_rowProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...){
  matrixStats::rowProds(x, rows = rows, cols = cols, na.rm = na.rm, method = method, ...)
}

#' @rdname rowProds
setMethod("rowProds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowProds)



#' @rdname rowProds
#' @name colProds
#' @export
setGeneric("colProds", function(x, rows = NULL, cols = NULL, na.rm = FALSE, ...) standardGeneric("colProds"),
           signature = "x"
)

.matrixStats_colProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, method = c("direct", "expSumLog"), ...){
  matrixStats::colProds(x, rows = rows, cols = cols, na.rm = na.rm, method = method, ...)
}

#' @rdname rowProds
setMethod("colProds", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colProds)

