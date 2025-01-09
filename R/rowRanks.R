#' Calculates the rank of the elements for each row (column) of a matrix-like
#' object
#'
#' Calculates the rank of the elements for each row (column) of a matrix-like
#' object.
#'
#' @include MatrixGenerics-package.R
#'
#' @export
#' @name rowRanks
#'
#' @templateVar rowName rowRanks
#' @templateVar colName colRanks
#'
#' @template matrixStatsLink
#'
#' @template standardParameters
#' @param ties.method A character string specifying how ties are treated. Note
#'   that the default specifies fewer options than the original matrixStats
#'   package.
#' @template dimParameter
#' @param preserveShape If `TRUE` the output matrix has the same shape as the
#'   input x. Note, that this is not a generic argument and not all
#'   implementation of this function have to provide it.
#' @template useNamesParameter
#'
#' @return A \code{\link[base]{matrix}} of type \code{\link[base]{integer}} is
#' returned, unless \code{ties.method = "average"} when it is of type
#' \code{\link[base]{numeric}}.
#'
#' The \code{rowRanks()} function always returns an NxK
#' \code{\link[base]{matrix}}, where N (K) is the number of rows (columns)
#' whose ranks are calculated.
#'
#' The \code{colRanks()} function returns an NxK \code{\link[base]{matrix}}, if
#' \code{preserveShape = TRUE}, otherwise a KxN \code{\link[base]{matrix}}.
#'
#' Any \code{\link[base]{names}} of \code{x} are ignored and absent in the
#' result.
#'
#' @details
#'    The `matrixStats::rowRanks()` function can handle a lot of different
#'    values for the `ties.method` argument. Users of the generic function
#'    should however only rely on `max` and `average` because the other ones
#'    are not guaranteed to be implemented:
#'    \describe{
#'      \item{`max`}{for values with identical values the maximum rank is
#'        returned}
#'      \item{`average`}{for values with identical values the average of the
#'        ranks they cover is returned. Note, that in this case the return
#'        value is of type `numeric`.}
#'    }
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowRanks}()} and
#'   \code{matrixStats::\link[matrixStats:rowRanks]{colRanks}()} which are used
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item [base::rank]
#' }
#'
#' @template standardExamples
#'
#' @keywords array iteration robust
setGeneric("rowRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average"), ..., useNames = TRUE) standardGeneric("rowRanks"),
           signature = "x"
)

.matrixStats_rowRanks <-  function(x, rows = NULL, cols = NULL,
                               ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                               dim. = dim(x), ..., useNames = TRUE){
  matrixStats::rowRanks(x = x, rows = rows, cols = cols, ties.method = ties.method, dim. = dim., ..., useNames = useNames)
}

#' @export
#' @rdname rowRanks
setMethod("rowRanks", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_rowRanks)

#' @export
#' @rdname rowRanks
## Default method with user-friendly fallback mechanism.
setMethod("rowRanks", "ANY", make_default_method_def("rowRanks"))



#' @export
#' @rdname rowRanks
setGeneric("colRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average"), ..., useNames = TRUE) standardGeneric("colRanks"),
           signature = "x"
)

.matrixStats_colRanks <-  function(x, rows = NULL, cols = NULL,
                               ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                               dim. = dim(x), preserveShape = FALSE, ..., useNames = TRUE){
  matrixStats::colRanks(x = x, rows = rows, cols = cols, ties.method = ties.method, dim. = dim., preserveShape = preserveShape, ..., useNames = useNames)
}

#' @export
#' @rdname rowRanks
setMethod("colRanks", "matrix_OR_array_OR_table_OR_numeric", .matrixStats_colRanks)

#' @export
#' @rdname rowRanks
## Default method with user-friendly fallback mechanism.
setMethod("colRanks", "ANY", make_default_method_def("colRanks"))

