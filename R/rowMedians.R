#' Calculates the median for each row (column) of a matrix-like object
#'
#' Calculates the median for each row (column) of a matrix-like object.
#'
#' @param x An NxK matrix-like object.
#' @param na.rm If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s
#' are excluded first, otherwise not.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' length N (K).
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowMedians}()} and
#'   \code{matrixStats::\link[matrixStats]{colMedians}()} which are used for
#'   \code{matrix} input.
#' \item See \code{\link{rowWeightedMedians}()} and \code{colWeightedMedians()}
#'   for weighted medians.
# #' \item For mean estimates, see \code{\link{rowMeans2}()} and
# #'   \code{\link[base:colSums]{rowMeans}()}.
#' }
#'
#' @keywords array iteration robust univar
#'
#' @name rowMedians
setGeneric("rowMedians", function(x, na.rm=FALSE, ...) standardGeneric("rowMedians"),
           signature = "x",
)

setMethod("rowMedians", "ANY", function(x, na.rm = FALSE, ...) {
  stop("colVars() is not defined for x of class: ", paste0(class(x), collapse = ", "))  
})

setMethod("rowMedians", signature = "matrix", function(x, na.rm=FALSE, ...){
  if(requireNamespace("matrixStats", quietly = TRUE)){
    matrixStats::rowMedians(x, na.rm=na.rm, ...)
  }else{
    stop("rowMedians() for x of class matrix, requires the 'matrixStats' package.\n",
         "You can install it with 'install.packages(\"matrixStats\")'.")
  }
})




#' @rdname rowMedians
#' @name colMedians
setGeneric("colMedians", function(x, na.rm=FALSE, ...) standardGeneric("colMedians"),
           signature = "x",
)

setMethod("colMedians", "ANY", function(x, na.rm = FALSE, ...) {
  stop("colVars() is not defined for x of class: ", paste0(class(x), collapse = ", "))  
})

setMethod("colMedians", signature = "matrix", function(x, na.rm=FALSE, ...){
  if(requireNamespace("matrixStats", quietly = TRUE)){
    matrixStats::colMedians(x, na.rm=na.rm, ...)
  }else{
    stop("colMedians() for x of class matrix, requires the 'matrixStats' package.\n",
         "You can install it with 'install.packages(\"matrixStats\")'.")
  }
})


