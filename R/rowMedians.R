#' Calculates the median for each row (column) of a matrix-like object
#'
#' Calculates the median for each row (column) of a matrix-like object.
#'
#' @usage
#' rowMedians(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...)
#'
#' colMedians(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...)
#' @param x An NxK matrix-like object.
#'
#' @param rows,cols A \code{\link[base]{vector}} indicating subset of rows
#' (and/or columns) to operate over. If \code{\link[base]{NULL}}, no subsetting
#' is done.
#'
#' @param na.rm If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s
#' are excluded first, otherwise not.
#'
#' @param dim. An \code{\link[base]{integer}} \code{\link[base]{vector}} of
#' length two specifying the dimension of \code{x}, also when not a
#' \code{\link[base]{matrix}}.
#'
#' @param ... Additional arguments passed to specific methods.
#'
#' @return Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' length N (K).
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowMedians}()} and
#'   \code{matrixStats::\link[matrixStats]{colMedians}()} for the default
#'   methods.
#' \item See \code{\link{rowWeightedMedians}()} and \code{colWeightedMedians()}
#'   for weighted medians.
# #' \item For mean estimates, see \code{\link{rowMeans2}()} and
# #'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#'
#' @keywords array iteration robust univar
#'
#' @name rowMedians
setGeneric(
  "rowMedians",
  function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
           ...) standardGeneric("rowMedians"),
  signature = "x",
  useAsDefault = function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                          dim. = dim(x), ...) {
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
      stop("'matrixStats' package required for matrix operations",
           call. = FALSE)
    }
    matrixStats::rowMedians(x, rows = rows, cols = cols, na.rm = na.rm,
                            dim. = dim., ...)
  })
setGenericImplicit("rowMedians")

#' @rdname rowMedians
#' @name colMedians
setGeneric(
  "colMedians",
  function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
           ...) standardGeneric("colMedians"),
  signature = "x",
  useAsDefault = function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                          dim. = dim(x), ...) {
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
      stop("'matrixStats' package required for matrix operations",
           call. = FALSE)
    }
    matrixStats::colMedians(x, rows = rows, cols = cols, na.rm = na.rm,
                            dim. = dim., ...)
  })
setGenericImplicit("colMedians")
