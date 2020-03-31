#' Calculates quantiles for each row (column) of a matrix-like object
#'
#' Calculates quantiles for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowQuantiles
#' @templateVar colName colQuantiles
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @param probs A numeric vector of J probabilities in \[0, 1\].
#' @template na_rmParameter
#' @param type An integer specifying the type of estimator. See 
#'   \code{stats::\link[stats]{quantile}()}. for more details. Note, that this
#'   is not a generic argument and not all implementation of this function have
#'   to provide it.
#' @param drop If `TRUE` a vector is returned if `J == 1`.
#'   Note, that this is not a generic argument and not all implementation of 
#'   this function have to provide it.
#' 
#' @template returnMatrix_JDim
#' 
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowQuantiles}()} and
#'   \code{matrixStats::\link[matrixStats]{colQuantiles}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item [stats::quantile]
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust
#'
#' @name rowQuantiles
#' @importFrom matrixStats colQuantiles rowQuantiles
#' @export
setGeneric("rowQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, ...) standardGeneric("rowQuantiles"),
           signature = "x"
)

.default_rowQuantiles <- function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), 
                                  na.rm = FALSE, type = 7L, ..., drop = TRUE) {
  matrixStats::rowQuantiles(x = x, rows = rows, cols = cols, probs = probs, na.rm = na.rm, type = type, ..., drop = drop)
}

#' @rdname rowQuantiles
setMethod("rowQuantiles", signature = "matrix", .default_rowQuantiles)

#' @rdname rowQuantiles
setMethod("rowQuantiles", signature = "numeric", .default_rowQuantiles)

#' @rdname rowQuantiles
setMethod("rowQuantiles", signature = "array", .default_rowQuantiles)




#' @rdname rowQuantiles
#' @name colQuantiles
#' @export
setGeneric("colQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, ...) standardGeneric("colQuantiles"),
           signature = "x"
)

.default_colQuantiles <- function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), 
                                  na.rm = FALSE, type = 7L, ..., drop = TRUE) {
  matrixStats::colQuantiles(x = x, rows = rows, cols = cols, probs = probs, na.rm = na.rm, type = type, ..., drop = drop)
}

#' @rdname rowQuantiles
setMethod("colQuantiles", signature = "matrix", .default_colQuantiles)

#' @rdname rowQuantiles
setMethod("colQuantiles", signature = "numeric", .default_colQuantiles)

#' @rdname rowQuantiles
setMethod("colQuantiles", signature = "array", .default_colQuantiles)



