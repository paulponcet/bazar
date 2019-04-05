#' @title
#' Test if values of a vector are almost zero
#'
#' @description
#' The function \code{almost_zero} tests if values of
#' the numeric vector \code{x} are equal to zero up 
#' to a tolerance.
#'
#' @param x
#' numeric. The vector of numeric values at stake.
#'
#' @param tolerance
#' numeric. Differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#'
#' @return
#' A logical vector of the same length as \code{x}.
#'
#' @seealso \code{\link[base]{all.equal}}.
#'
#' @export
#'
#' @examples
#' almost_zero(c(0, 10^(-7), 10^(-8)))
#'
almost_zero <-
function(x,
         tolerance = sqrt(.Machine$double.eps))
{
  almost_equal(x, 0, tolerance)
}


#' @export
#' @rdname almost_zero
#' 
almost.zero <- 
function(x,
         tolerance = sqrt(.Machine$double.eps))
{
  .Deprecated("almost_zero")
  almost_zero(x, tolerance)
}
