#' Calculates the mean for each row (column) of a matrix-like object
#'
#' Calculates the mean for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowMeans
#'
#' @templateVar rowName rowMeans
#' @templateVar colName colMeans
#'
#' @param x An NxK matrix-like object, a numeric data frame, or an array-like
#' object of two or more dimensions.
#' @template na_rmParameter
#' @param dims A single integer indicating which dimensions are regarded
#' as rows or columns to mean over. For \code{rowMeans}, the mean is over
#' dimensions \code{dims+1, ...}; for \code{colMeans} it is over
#' dimensions \code{1:dims}.
#' @param ... Additional arguments passed to specific methods.
#'
#' @details The S4 methods for \code{x} of type \code{\link{matrix}},
#' \code{\link{array}}, \code{\link{table}}, \code{\link{numeric}}, or
#' \code{\link{data.frame}} call \code{base::\link[base]{rowMeans}} /
#' \code{base::\link[base]{colMeans}}.
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{base::\link[base]{colMeans}} for the default
#'       \code{rowMeans} and \code{colMeans} methods.
#' \item \code{Matrix::\link[Matrix]{colMeans}} in the \pkg{Matrix} package
#'       for \code{rowMeans} and \code{colMeans} methods defined for
#'       CsparseMatrix derivatives (e.g. dgCMatrix objects).
#' }
#'
#' @template standardExamples
#'
#' @keywords array algebra arith iteration robust univar
## Calling setGeneric("rowMeans") brings the implicit generic defined in the
## methods package. This generic has arguments x, na.rm=FALSE, dims=1, ...
## so the colMeans() methods we define below use those exact same arguments.
setGeneric("rowMeans")

.base_rowMeans <- function(x, na.rm=FALSE, dims=1, ...)
{
    base::rowMeans(x, na.rm=na.rm, dims=dims, ...)
}

#' @export
#' @rdname rowMeans
setMethod("rowMeans", "matrix_OR_array_OR_table_OR_numeric", .base_rowMeans)

#' @export
#' @rdname rowMeans
setMethod("rowMeans", "data.frame", .base_rowMeans)

#' @export
#' @rdname rowMeans
## Default method with user-friendly fallback mechanism.
setMethod("rowMeans", "ANY", make_default_method_def("rowMeans"))


#' @export
#' @rdname rowMeans
## Calling setGeneric("colMeans") brings the implicit generic defined in the
## methods package. This generic has arguments x, na.rm=FALSE, dims=1, ...
## so the colMeans() methods we define below use those exact same arguments.
setGeneric("colMeans")

.base_colMeans <- function(x, na.rm=FALSE, dims=1, ...)
{
    base::colMeans(x, na.rm=na.rm, dims=dims, ...)
}

#' @export
#' @rdname rowMeans
setMethod("colMeans", "matrix_OR_array_OR_table_OR_numeric", .base_colMeans)

#' @export
#' @rdname rowMeans
setMethod("colMeans", "data.frame", .base_colMeans)

#' @export
#' @rdname rowMeans
## Default method with user-friendly fallback mechanism.
setMethod("colMeans", "ANY", make_default_method_def("colMeans"))

