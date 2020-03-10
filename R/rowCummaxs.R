#' Calculates the cumulative maxima for each row (column) of a matrix-like object
#'
#' Calculates the cumulative maxima for each row (column) of a matrix-like object
#' 
#' @templateVar rowName rowCummaxs
#' @templateVar colName colCummaxs
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template dimParameter
#'
#' @template returnMatrix_SameDimX
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowCummaxs}()} and
#'   \code{matrixStats::\link[matrixStats]{colCummaxs}()} which are used when
#'   the input is a \code{matrix} or \code{numeric} vector.
#' \item For single maximum estimates, see \code{\link{rowMaxs}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowCummaxs
#' @export
setGeneric("rowCummaxs", function(x, rows = NULL, cols = NULL,  ...) standardGeneric("rowCummaxs"),
           signature = "x"
)

.default_rowCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x)){
  matrixStats::rowCummaxs(x, rows = rows, cols = cols, dim. = dim.)
}

#' @rdname rowCummaxs
setMethod("rowCummaxs", signature = "matrix", .default_rowCummaxs)

#' @rdname rowCummaxs
setMethod("rowCummaxs", signature = "numeric", .default_rowCummaxs)

#' @rdname rowCummaxs
setMethod("rowCummaxs", signature = "array", .default_rowCummaxs)




#' @rdname rowCummaxs
#' @name colCummaxs
#' @export
setGeneric("colCummaxs", function(x, rows = NULL, cols = NULL, ...) standardGeneric("colCummaxs"),
           signature = "x"
)

.default_colCummaxs <- function(x, rows = NULL, cols = NULL, dim. = dim(x)){
  matrixStats::colCummaxs(x, rows = rows, cols = cols, dim. = dim.)
}

#' @rdname rowCummaxs
setMethod("colCummaxs", signature = "matrix", .default_colCummaxs)

#' @rdname rowCummaxs
setMethod("colCummaxs", signature = "numeric", .default_colCummaxs)

#' @rdname rowCummaxs
setMethod("colCummaxs", signature = "array", .default_colCummaxs)



