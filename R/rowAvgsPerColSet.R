#' Calculates for each row (column) a summary statistic for equally sized
#' subsets of columns (rows)
#' 
#' Calculates for each row (column) a summary statistic for equally sized
#' subsets of columns (rows).
#' 
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowAvgsPerColSet
#'
#' @templateVar rowName rowAvgsPerColSet
#' @templateVar colName colAvgsPerRowSet
#' 
#' @template matrixStatsLink
#' 
#' @param X An `NxM` matrix-like object.
#' @param W An optional numeric `NxM` matrix of weights.
#' @param rows,cols A \code{\link{vector}} indicating the subset (and/or 
#'   columns) to operate over. If \code{\link{NULL}}, no subsetting is
#'   done.
#' @param S An [integer] `KxJ` matrix that specifying the `J` subsets. Each 
#'   column hold `K` column (row) indices for the corresponding subset. The 
#'   range of values is \[1, M\] (\[1,N\]).
#' @param FUN A row-by-row (column-by-column) summary statistic function. It is 
#'   applied to to each column (row) subset of `X` that is specified by `S`.
#' @param ... Additional arguments passed to `FUN`.
#' @param na.rm (logical) Argument passed to `FUN()` as `na.rm = na.rm`. 
#'   If `NA` (default), then `na.rm = TRUE` is used if `X` or `S` holds missing values, 
#'   otherwise `na.rm = FALSE`.
#' @param tFUN If `TRUE`, `X` is transposed before it is passed to `FUN`.
#' 
#' 
#' @return Returns a numeric `JxN` (`MxJ`) matrix.
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats:rowAvgsPerColSet]{rowAvgsPerColSet}()}
#'   and \code{matrixStats::\link[matrixStats:rowAvgsPerColSet]{colAvgsPerRowSet}()}
#'   which are used when the input is a \code{matrix} or \code{numeric} vector.
#' }
#' 
#' @examples 
#'   mat <- matrix(rnorm(20), nrow = 5, ncol = 4)
#'   mat[2, 1] <- NA
#'   mat[3, 3] <- Inf
#'   mat[4, 1] <- 0
#'   
#'   print(mat)
#'   
#'   S <- matrix(1:ncol(mat), ncol = 2)
#'   print(S)
#'   
#'   rowAvgsPerColSet(mat, S = S, FUN = rowMeans)
#'   rowAvgsPerColSet(mat, S = S, FUN = rowVars)
#'
#' @keywords array iteration robust univar2
setGeneric("rowAvgsPerColSet", function(X, W = NULL, rows = NULL, S, FUN = rowMeans,  ..., na.rm = NA, tFUN = FALSE) standardGeneric("rowAvgsPerColSet"),
           signature = "X"
)

.matrixStats_rowAvgsPerColSet <- function(X, W = NULL, rows = NULL, S, FUN = rowMeans,  ..., na.rm = NA, tFUN = FALSE){
  matrixStats::rowAvgsPerColSet(X = X, W = W, rows = rows, S = S, FUN = FUN, ..., na.rm = na.rm, tFUN = tFUN)
}

#' @export
#' @rdname rowAvgsPerColSet
setMethod("rowAvgsPerColSet", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowAvgsPerColSet)

#' @export
#' @rdname rowAvgsPerColSet
## Default method with user-friendly fallback mechanism.
setMethod("rowAvgsPerColSet", "ANY", function (X, W = NULL, rows = NULL, S, FUN = rowMeans, ..., na.rm = NA, tFUN = FALSE){
  MatrixGenerics:::.load_next_suggested_package_to_search(X)
  callGeneric()
})



#' @export
#' @rdname rowAvgsPerColSet
setGeneric("colAvgsPerRowSet", function(X, W = NULL, cols = NULL, S, FUN = colMeans,  ..., na.rm = NA, tFUN = FALSE) standardGeneric("colAvgsPerRowSet"),
           signature = "X"
)

.matrixStats_colAvgsPerRowSet <- function(X, W = NULL, cols = NULL, S, FUN = colMeans,  ..., na.rm = NA, tFUN = FALSE){
  matrixStats::colAvgsPerRowSet(X = X, W = W, cols = cols, S = S, FUN = FUN, ..., na.rm = na.rm, tFUN = tFUN)
}

#' @export
#' @rdname rowAvgsPerColSet
setMethod("colAvgsPerRowSet", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colAvgsPerRowSet)

#' @export
#' @rdname rowAvgsPerColSet
## Default method with user-friendly fallback mechanism.
setMethod("colAvgsPerRowSet", "ANY", function (X, W = NULL, cols = NULL, S, FUN = colMeans, ..., na.rm = NA, tFUN = FALSE){
  MatrixGenerics:::.load_next_suggested_package_to_search(X)
  callGeneric()
})

