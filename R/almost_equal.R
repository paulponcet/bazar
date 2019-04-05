#' @title 
#' Test (almost) equality of numeric values
#' 
#' @description 
#' The function \code{almost_equal} tests if two numeric vectors 
#' have equal values up to a tolerance. 
#' 
#' @param x
#' numeric vector. 
#' 
#' @param y
#' numeric vector of the same length as \code{x}. 
#' 
#' @param tolerance 
#' numeric. Differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @return
#' A logical vector of the same length as \code{x} and \code{y}. 
#' 
#' @author 
#' Tommy on StackOverflow, see \url{http://stackoverflow.com/a/7667703}. 
#' 
#' @export
#' 
#' @examples 
#' almost_equal(x = 1:3, 
#'              y = 1:3 + c(10^(-6), 10^(-7), 10^(-8)))
#' 
almost_equal <- 
function(x, 
         y, 
         tolerance = sqrt(.Machine$double.eps))
{
  # Catch two infinities of the same sign
  b1 <- (x == y)
  
  diff <- abs(x - y)
  mag <- pmax(abs(x), abs(y))
  b2 <- ifelse(mag > tolerance, diff/mag <= tolerance, diff <= tolerance)
  
  b1 | (!is.infinite(x) & !is.infinite(y) & b2)
}


#' @export
#' @rdname almost_equal
#' 
almost.equal <- 
function(x,
         y, 
         tolerance = sqrt(.Machine$double.eps))
{
  .Deprecated("almost_equal")
  almost_equal(x, y, tolerance)
}
