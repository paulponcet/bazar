#' @title
#' Almost unique elements
#'
#' @description
#' The function \code{almost_unique} extracts elements of a vector \code{x} 
#' that are unique up to a tolerance factor.
#'
#' @param x
#' numeric. The vector of numeric values at stake.
#'
#' @param tolerance
#' numeric. Relative differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @param ...
#' Additional arguments to be passed to the function 
#' \code{\link[base]{duplicated}}, which is used internally by 
#' \code{almost_unique}. 
#'
#' @return
#' A vector of the same type as \code{x}.
#' 
#' @seealso 
#' \code{\link[base]{unique}}, 
#' \code{\link[base]{duplicated}}.
#' 
#' @export
#' 
#' @examples 
#' almost_unique(c(1, 1.01), tol = 0.1)
#' almost_unique(c(1, 1.01), tol = 0.01)
#' 
#' almost_unique(c(1, 2, 3), tol = 10)
#' almost_unique(c(1, 2, 3), tol = 5)
#' almost_unique(c(1, 2, 3), tol = 1)
#' 
almost_unique <- 
function(x, 
         ...)
{
  UseMethod("almost_unique")
}


#' @export
#' @rdname almost_unique
#' 
almost.unique <- 
function(x, 
         ...)
{
  .Deprecated("almost_unique")
  almost_unique(x, ...)
}


#' @export
#' @rdname almost_unique
#' 
almost_unique.default <- 
function(x, 
         tolerance = sqrt(.Machine$double.eps),
         ...)
{
  y <- round(x/tolerance, 0)
  d <- duplicated(y, ...)
  x[!d]
}
