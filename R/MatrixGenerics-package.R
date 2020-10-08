#' The MatrixGenerics package
#'
#' The \pkg{MatrixGenerics} package defines S4 generic summary statistic
#' functions that operate on matrix-Like objects.
#'
# NOTE: Import a single function to quieten R CMD check NOTE:
#         Package in Depends field not imported from: ‘matrixStats’
#           These packages need to be imported from (in the NAMESPACE file)
#           for when this namespace is loaded but not attached.
#' @importFrom matrixStats allocArray
#' @import methods
#
#' @name MatrixGenerics-package
#' @exportClass matrix_OR_array_OR_table_OR_numeric
#' @aliases class:matrix_OR_array_OR_table_OR_numeric
#' @aliases matrix_OR_array_OR_table_OR_numeric-class
#' @aliases matrix_OR_array_OR_table_OR_numeric
# NOTE: Starting with R 4.0.0 a matrix is an array so no need to explicitly
# list "matrix" as a member of the union.
setClassUnion("matrix_OR_array_OR_table_OR_numeric",
    c("array", "table", "numeric")
)

