#' @title 
#' Test if the values of a vector are whole numbers 
#' 
#' @description 
#' The function \code{is.wholenumber} tests if values of 
#' the numeric vector \code{x} are all whole numbers (up 
#' to a tolerance).
#' 
#' The function \code{as.wholenumber} is a synonym for 
#' \code{\link[base]{as.integer}}. 
#' 
#' @param x
#' a vector to be tested. 
#' 
#' @param tolerance 
#' numeric. Differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @param ...
#' Additional arguments passed to or from other methods. 
#' 
#' @return 
#' A logical, \code{TRUE} if all values of \code{x} 
#' are whole numbers. 
#' 
#' @export
#' 
#' @examples 
#' x = c(1L, 10L)
#' is.integer(x)
#' is.wholenumber(x)
#' 
#' x = c(1, 10)
#' is.integer(x)
#' is.wholenumber(x) # here is the difference with 'is.integer'
#' 
#' is.wholenumber(1+10^(-7))
#' is.wholenumber(1+10^(-8))
#' 
is.wholenumber <- 
function(x, 
         tolerance = sqrt(.Machine$double.eps))
{
  is.numeric(x) && all(abs(x - round(x)) < tolerance)
}


#' @export
#' @rdname is.wholenumber
#'
as.wholenumber <- 
function(x, 
         ...)
{
  as.integer(x, ...)
}
