#' @title 
#' Test if the values of a vector are whole numbers 
#' 
#' @description 
#' The function \code{is_wholenumber} tests if values of 
#' the numeric vector \code{x} are all whole numbers (up 
#' to a tolerance).
#' 
#' The function \code{as_wholenumber} is a synonym for 
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
#' are (finite) whole numbers. 
#' If \code{x} contains \code{NA} or \code{NaN}, then \code{NA} is returned. 
#' 
#' @export
#' 
#' @examples 
#' x = c(1L, 10L)
#' is.integer(x)
#' is_wholenumber(x)
#' 
#' x = c(1, 10)
#' is.integer(x)
#' is_wholenumber(x) # here is the difference with 'is.integer'
#' 
#' is_wholenumber(1+10^(-7))
#' is_wholenumber(1+10^(-8))
#' 
is_wholenumber <- 
function(x, 
         tolerance = sqrt(.Machine$double.eps))
{
  is.numeric(x) && 
    #all(is.finite(x)) && # not sure...
    #all(!is.na(x)) &&    # not sure either...
    all(abs(x - round(x)) < tolerance, na.rm = TRUE)
}


#' @export
#' @rdname is_wholenumber
#' 
is.wholenumber <- 
function(x,
         tolerance = sqrt(.Machine$double.eps))
{
  .Deprecated("is_wholenumber")
  is_wholenumber(x, tolerance)
}


#' @export
#' @rdname is_wholenumber
#'
as_wholenumber <- 
function(x, 
         ...)
{
  as.integer(x, ...)
}


#' @export
#' @rdname is_wholenumber
#' 
as.wholenumber <- 
function(x,
         ...)
{
  .Deprecated("as_wholenumber")
  as_wholenumber(x, ...)
}
