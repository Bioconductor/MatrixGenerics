## IMPORTANT NOTE: We don't provide a user-friendly fallback mechanism for
## rowMeans() and colMeans(). Here is why:
## The method package defines **implicit generics** for the rowMeans()
## and colMeans() functions and this is what we get when we call
## setGeneric("rowMeans") and setGeneric("colMeans") below. These implicit
## generics introduce 2 surprises:
## 1. Their argument list (x, na.rm=FALSE, dims=1, ...) differs slightly from
##    base::rowMeans and base::colMeans. This means that we need to be careful
##    to use this same exact argument list in the rowMeans() and colMeans()
##    methods that we define below.
## 2. They define their own default method that basically calls base::rowMeans()
##    or base::colMeans(). Unfortunately, trying to override these default
##    methods with our own doesn't work. Even if selectMethod("rowMeans", "ANY")
##    and selectMethod("colMeans", "ANY") return our methods right after
##    loading MatrixGenerics in a fresh R session, this is no longer the case
##    if we then load the DelayedArray package. Then the default methods get
##    replaced with the original default methods defined by the implicit
##    generics. VERY NASTY! This is why our attempts at providing user-friendly
##    fallback mechanisms for rowMeans() and colMeans() are commented out below.

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
#' @details This man page documents the \code{rowMeans} and \code{colMeans}
#' \emph{S4 generic functions} defined in the \pkg{MatrixGenerics} package.
#' See \code{?base::\link[base]{colMeans}} for the default methods (defined
#' in the \pkg{base} package).
# #' The S4 methods for \code{x} of type \code{\link{matrix}},
# #' \code{\link{array}}, \code{\link{table}}, \code{\link{numeric}}, or
# #' \code{\link{data.frame}} call \code{base::\link[base]{rowMeans}} /
# #' \code{base::\link[base]{colMeans}}.
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
setGeneric("rowMeans")

# .base_rowMeans <- function(x, na.rm=FALSE, dims=1, ...)
# {
#     base::rowMeans(x, na.rm=na.rm, dims=dims, ...)
# }
#
# #' @export
# #' @rdname rowMeans
# setMethod("rowMeans", "matrix_OR_array_OR_table_OR_numeric", .base_rowMeans)
#
# #' @export
# #' @rdname rowMeans
# setMethod("rowMeans", "data.frame", .base_rowMeans)
#
# #' @export
# #' @rdname rowMeans
# ## Default method with user-friendly fallback mechanism.
# setMethod("rowMeans", "ANY", make_default_method_def("rowMeans"))


#' @export
#' @rdname rowMeans
setGeneric("colMeans")

# .base_colMeans <- function(x, na.rm=FALSE, dims=1, ...)
# {
#     base::colMeans(x, na.rm=na.rm, dims=dims, ...)
# }
#
# #' @export
# #' @rdname rowMeans
# setMethod("colMeans", "matrix_OR_array_OR_table_OR_numeric", .base_colMeans)
#
# #' @export
# #' @rdname rowMeans
# setMethod("colMeans", "data.frame", .base_colMeans)
#
# #' @export
# #' @rdname rowMeans
# ## Default method with user-friendly fallback mechanism.
# setMethod("colMeans", "ANY", make_default_method_def("colMeans"))

