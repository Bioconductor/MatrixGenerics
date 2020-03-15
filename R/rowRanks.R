#' Calculates the rank of the elements for each row (column) of a matrix-like object
#'
#' Calculates the rank of the elements for each row (column) of a matrix-like object
#' 
#' @templateVar rowName rowRanks
#' @templateVar colName colRanks
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @param ties.method A character string specifying how ties are treated. Note that the default specifies 
#'   fewer options than the original matrixStats package.
#' @template dimParameter
#' @param preserveShape If `TRUE` the output matrix has the same shape as the input x.
#'   Note, that this is not a generic srgument and not all implementation of this function have to
#'   provide it.
#' 
#' @return a matrix of type \code{\link[base]{integer}} is returned unlesss `ties.method = "average"`. It
#'  has dimesions` \code{NxJ} (\code{KxJ}) \code{\link{matrix}}, where
#'  N (K) is the number of rows (columns) of the input x.
#'  
#' @details 
#'    The `matrixStats::rowRanks()` function can handle a lot of different values for the `ties.method`
#'    argument. Users of the generic function should however only rely on those two because the other
#'    ones are not guaranteed to be implemented:
#'    \describe{
#'      \item{`max`}{for values with identical values the maximum rank is returned}
#'      \item{`average`}{for values with identical values the average of the ranks they cover
#'      is returned. Note, that in this case the return value is of type `numeric`.}
#'    }
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowRanks}()} and
#'   \code{matrixStats::\link[matrixStats]{colRanks}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item [base::rank]
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust
#'
#' @name rowRanks
#' @export
setGeneric("rowRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average"), ...) standardGeneric("rowRanks"),
           signature = "x"
)

.default_rowRanks <-  function(x, rows = NULL, cols = NULL, 
                               ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                               dim. = dim(x)){
  matrixStats::rowRanks(x = x, rows = rows, cols = cols, ties.method = ties.method, dim. = dim.)
}

#' @rdname rowRanks
setMethod("rowRanks", signature = "matrix", .default_rowRanks)

#' @rdname rowRanks
setMethod("rowRanks", signature = "numeric", .default_rowRanks)

#' @rdname rowRanks
setMethod("rowRanks", signature = "array", .default_rowRanks)




#' @rdname rowRanks
#' @name colRanks
#' @export
setGeneric("colRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average"), ...) standardGeneric("colRanks"),
           signature = "x"
)

.default_colRanks <-  function(x, rows = NULL, cols = NULL, 
                               ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                               dim. = dim(x), preserveShape = FALSE){
  matrixStats::colRanks(x = x, rows = rows, cols = cols, ties.method = ties.method, dim. = dim., preserveShape = preserveShape)
}

#' @rdname rowRanks
setMethod("colRanks", signature = "matrix", .default_colRanks)

#' @rdname rowRanks
setMethod("colRanks", signature = "numeric", .default_colRanks)

#' @rdname rowRanks
setMethod("colRanks", signature = "array", .default_colRanks)



