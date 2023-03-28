## IMPORTANT NOTE: We don't provide a user-friendly fallback mechanism for
## rowSums() and colSums(). Here is why:
## The method package defines **implicit generics** for the rowSums()
## and colSums() functions and this is what we get when we call
## setGeneric("rowSums") and setGeneric("colSums") below. These implicit
## generics introduce 2 surprises:
## 1. Their argument list (x, na.rm=FALSE, dims=1, ...) differs slightly from
##    base::rowSums and base::colSums. This means that we need to be careful
##    to use this same exact argument list in the rowSums() and colSums()
##    methods that we define below.
## 2. They define their own default method that basically calls base::rowSums()
##    or base::colSums(). Unfortunately, trying to override these default
##    methods with our own doesn't work. Even if selectMethod("rowSums", "ANY")
##    and selectMethod("colSums", "ANY") return our methods right after
##    loading MatrixGenerics in a fresh R session, this is no longer the case
##    if we then load the DelayedArray package. Then the default methods get
##    replaced with the original default methods defined by the implicit
##    generics. VERY NASTY! This is why our attempts at providing user-friendly
##    fallback mechanisms for rowSums() and colSums() are commented out below.


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
#' @details This man page documents the \code{rowSums} and \code{colSums}
#' \emph{S4 generic functions} defined in the \pkg{MatrixGenerics} package.
#' See \code{?base::\link[base]{colSums}} for the default methods (defined
#' in the \pkg{base} package).
# #' The S4 methods for \code{x} of type \code{\link{matrix}},
# #' \code{\link{array}}, \code{\link{table}}, \code{\link{numeric}}, or
# #' \code{\link{data.frame}} call \code{base::\link[base]{rowSums}} /
# #' \code{base::\link[base]{colSums}}.
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
setGeneric("rowSums")

# .base_rowSums <- function(x, na.rm=FALSE, dims=1, ...)
# {
#     base::rowSums(x, na.rm=na.rm, dims=dims, ...)
# }
#
# #' @export
# #' @rdname rowSums
# setMethod("rowSums", "matrix_OR_array_OR_table_OR_numeric", .base_rowSums)
#
# #' @export
# #' @rdname rowSums
# setMethod("rowSums", "data.frame", .base_rowSums)
#
# #' @export
# #' @rdname rowSums
# ## Override default method of implicit generic with user-friendly fallback
# ## mechanism.
# setMethod("rowSums", "ANY", make_default_method_def("rowSums"))


#' @export
#' @rdname rowSums
setGeneric("colSums")

# .base_colSums <- function(x, na.rm=FALSE, dims=1, ...)
# {
#     base::colSums(x, na.rm=na.rm, dims=dims, ...)
# }
#
# #' @export
# #' @rdname rowSums
# setMethod("colSums", "matrix_OR_array_OR_table_OR_numeric", .base_colSums)
#
# #' @export
# #' @rdname rowSums
# setMethod("colSums", "data.frame", .base_colSums)
#
# #' @export
# #' @rdname rowSums
# ## Override default method of implicit generic with user-friendly fallback
# ## mechanism.
# setMethod("colSums", "ANY", make_default_method_def("colSums"))

