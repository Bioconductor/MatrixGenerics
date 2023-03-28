#' Calculates the sum for each row (column) of a matrix-like object
#'
#' Calculates the sum for each row (column) of a matrix-like object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowSums
#'
#' @templateVar rowName rowSums
#' @templateVar colName colSums
#'
#' @param x An NxK matrix-like object, a numeric data frame, or an array-like
#' object of two or more dimensions.
#' @template na_rmParameter
#' @param dims A single integer indicating which dimensions are regarded
#' as rows or columns to sum over. For \code{rowSums}, the sum is over
#' dimensions \code{dims+1, ...}; for \code{colSums} it is over
#' dimensions \code{1:dims}.
#' @param ... Additional arguments passed to specific methods.
#'
#' @details The S4 methods for \code{x} of type \code{\link{matrix}},
#' \code{\link{array}}, \code{\link{table}}, \code{\link{numeric}}, or
#' \code{\link{data.frame}} call \code{base::\link[base]{rowSums}} /
#' \code{base::\link[base]{colSums}}.
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{base::\link[base]{colSums}} for the default
#'       \code{rowSums} and \code{colSums} methods.
#' \item \code{Matrix::\link[Matrix]{colSums}} in the \pkg{Matrix} package
#'       for \code{rowSums} and \code{colSums} methods defined for
#'       CsparseMatrix derivatives (e.g. dgCMatrix objects).
#' }
#'
#' @template standardExamples
#'
#' @keywords array algebra arith iteration robust univar
## Calling setGeneric("rowSums") brings the implicit generic defined in the
## methods package. This generic has arguments x, na.rm=FALSE, dims=1, ...
## so the colSums() methods we define below use those exact same arguments.
setGeneric("rowSums")

.base_rowSums <- function(x, na.rm=FALSE, dims=1, ...)
{
    base::rowSums(x, na.rm=na.rm, dims=dims, ...)
}

#' @export
#' @rdname rowSums
setMethod("rowSums", "matrix_OR_array_OR_table_OR_numeric", .base_rowSums)

#' @export
#' @rdname rowSums
setMethod("rowSums", "data.frame", .base_rowSums)

#' @export
#' @rdname rowSums
## Default method with user-friendly fallback mechanism.
setMethod("rowSums", "ANY", make_default_method_def("rowSums"))


#' @export
#' @rdname rowSums
## Calling setGeneric("colSums") brings the implicit generic defined in the
## methods package. This generic has arguments x, na.rm=FALSE, dims=1, ...
## so the colSums() methods we define below use those exact same arguments.
setGeneric("colSums")

.base_colSums <- function(x, na.rm=FALSE, dims=1, ...)
{
    base::colSums(x, na.rm=na.rm, dims=dims, ...)
}

#' @export
#' @rdname rowSums
setMethod("colSums", "matrix_OR_array_OR_table_OR_numeric", .base_colSums)

#' @export
#' @rdname rowSums
setMethod("colSums", "data.frame", .base_colSums)

#' @export
#' @rdname rowSums
## Default method with user-friendly fallback mechanism.
setMethod("colSums", "ANY", make_default_method_def("colSums"))

