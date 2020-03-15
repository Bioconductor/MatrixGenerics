#' Calculates for each row (column) a summary statistic for equally sized subsets of columns (rows)
#' 
#' Calculates for each row (column) a summary statistic for equally sized subsets of columns (rows)
#' 
#' @templateVar rowName rowAvgsPerColSet
#' @templateVar colName colAvgsPerRowSet
#' 
#' @template matrixStatsLink
#' 
#' @param X An `NxM` matrix-like object.
#' @param W An optional numeric `NxM` matrix of weights.
#' @param rows,cols A \code{\link[base]{vector}} indicating the subset (and/or 
#'   columns) to operate over. If \code{\link[base]{NULL}}, no subsetting is done.
#' @param S An [integer] `KxJ` matrix that specifying the `J` subsets. Each colunn
#'   hold `K` column (row) indices for the corresponding subset. The range of values
#'   is \[1, M\] (\[1,N\]).
#' @param FUN A row-by-row (column-by-column) summary statistic function. It is 
#'   applied to to each column (row) subset of `X` that is specified by `S`.
#' @param ... Additional arguments passed to `FUN`.
#' @param tFUN If `TRUE`, `X` is transposed before it is passed to `FUN`.
#' 
#' 
#' @return Returns a numeric `JxN` (`MxJ`) matrix.
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowAvgsPerColSet}()} and
#'   \code{matrixStats::\link[matrixStats]{colAvgsPerRowSet}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
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
#'
#' @name rowAvgsPerColSet
#' @export
setGeneric("rowAvgsPerColSet", function(X, W = NULL, rows = NULL, S, FUN = rowMeans,  ..., tFUN = FALSE) standardGeneric("rowAvgsPerColSet"),
           signature = "X"
)

.default_rowAvgsPerColSet <- function(X, W = NULL, rows = NULL, S, FUN = rowMeans,  ..., tFUN = FALSE){
  matrixStats::rowAvgsPerColSet(X = X, W = W, rows = rows, S = S, FUN = FUN, ..., tFUN = tFUN)
}

#' @rdname rowAvgsPerColSet
setMethod("rowAvgsPerColSet", signature = "matrix", .default_rowAvgsPerColSet)

#' @rdname rowAvgsPerColSet
setMethod("rowAvgsPerColSet", signature = "numeric", .default_rowAvgsPerColSet)

#' @rdname rowAvgsPerColSet
setMethod("rowAvgsPerColSet", signature = "array", .default_rowAvgsPerColSet)




#' @rdname rowAvgsPerColSet
#' @name colAvgsPerRowSet
#' @export
setGeneric("colAvgsPerRowSet", function(X, W = NULL, cols = NULL, S, FUN = rowMeans,  ..., tFUN = FALSE) standardGeneric("colAvgsPerRowSet"),
           signature = "X"
)

.default_colAvgsPerRowSet <- function(X, W = NULL, cols = NULL, S, FUN = rowMeans,  ..., tFUN = FALSE){
  matrixStats::colAvgsPerRowSet(X = X, W = W, cols = cols, S = S, FUN = FUN, ..., tFUN = tFUN)
}

#' @rdname rowAvgsPerColSet
setMethod("colAvgsPerRowSet", signature = "matrix", .default_colAvgsPerRowSet)

#' @rdname rowAvgsPerColSet
setMethod("colAvgsPerRowSet", signature = "numeric", .default_colAvgsPerRowSet)

#' @rdname rowAvgsPerColSet
setMethod("colAvgsPerRowSet", signature = "array", .default_colAvgsPerRowSet)



