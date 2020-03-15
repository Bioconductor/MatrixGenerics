#' Calculates the product for each row (column) of a matrix-like object
#'
#' Calculates the product for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowProds
#' @templateVar colName colProds
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @param method A character vector of length one that specifies the 
#'   how the product is calculated. Note, that this is not a generic 
#'   argument and not all implementation have to provide it.
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowProds}()} and
#'   \code{matrixStats::\link[matrixStats]{colProds}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For sums across rows (columns), see \code{\link{rowSums2}()} ([colSums2()])
#' \item [prod()]
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowProds
#' @export
setGeneric("rowProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("rowProds"),
           signature = "x"
)

.default_rowProds <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, method = c("direct", "expSumLog")){
  matrixStats::rowProds(x, rows = rows, cols = cols, na.rm=na.rm, method = method)
}

#' @rdname rowProds
setMethod("rowProds", signature = "matrix", .default_rowProds)

#' @rdname rowProds
setMethod("rowProds", signature = "numeric", .default_rowProds)

#' @rdname rowProds
setMethod("rowProds", signature = "array", .default_rowProds)




#' @rdname rowProds
#' @name colProds
#' @export
setGeneric("colProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...) standardGeneric("colProds"),
           signature = "x"
)

.default_colProds <- function(x, rows = NULL, cols = NULL, na.rm=FALSE, method = c("direct", "expSumLog")){
  matrixStats::colProds(x, rows = rows, cols = cols, na.rm=na.rm, method = method)
}

#' @rdname rowProds
setMethod("colProds", signature = "matrix", .default_colProds)

#' @rdname rowProds
setMethod("colProds", signature = "numeric", .default_colProds)

#' @rdname rowProds
setMethod("colProds", signature = "array", .default_colProds)



