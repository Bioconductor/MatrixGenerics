#' Calculates quantiles for each row (column) of a matrix-like object
#'
#' Calculates quantiles for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowQuantiles
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
#'   \code{stats::\link[stats]{quantile}()}. for more details.
#' @param drop If `TRUE` a vector is returned if `J == 1`.
#' @template useNamesParameter
#'
#' @template returnMatrix_JDim
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowQuantiles}()} and
#'   \code{matrixStats::\link[matrixStats:rowQuantiles]{colQuantiles}()} which
#'   are used when the input is a \code{matrix} or \code{numeric} vector.
#' \item [stats::quantile]
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust
setGeneric("rowQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L, ..., useNames = TRUE, drop = TRUE) standardGeneric("rowQuantiles"),
           signature = "x"
)

.matrixStats_rowQuantiles <- function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25),
                                  na.rm = FALSE, type = 7L, ..., useNames = TRUE, drop = TRUE) {
  matrixStats::rowQuantiles(x = x, rows = rows, cols = cols, probs = probs, na.rm = na.rm, type = type, ..., useNames = TRUE, drop = drop)
}

#' @export
#' @rdname rowQuantiles
setMethod("rowQuantiles", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowQuantiles)

#' @export
#' @rdname rowQuantiles
## Default method with user-friendly fallback mechanism.
setMethod("rowQuantiles", "ANY", make_default_method_def("rowQuantiles"))



#' @export
#' @rdname rowQuantiles
setGeneric("colQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L, ..., useNames = TRUE, drop = TRUE) standardGeneric("colQuantiles"),
           signature = "x"
)

.matrixStats_colQuantiles <- function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25),
                                  na.rm = FALSE, type = 7L, ..., useNames = TRUE, drop = TRUE) {
  matrixStats::colQuantiles(x = x, rows = rows, cols = cols, probs = probs, na.rm = na.rm, type = type, ..., useNames = TRUE, drop = drop)
}

#' @export
#' @rdname rowQuantiles
setMethod("colQuantiles", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colQuantiles)

#' @export
#' @rdname rowQuantiles
## Default method with user-friendly fallback mechanism.
setMethod("colQuantiles", "ANY", make_default_method_def("colQuantiles"))

