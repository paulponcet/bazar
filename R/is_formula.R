#' @title 
#' Test if an object is a formula
#' 
#' @description 
#' The function \code{is_formula} tests if the object 
#' \code{x} is a formula. 
#' 
#' @param x
#' An object. 
#' 
#' @return 
#' A logical, \code{TRUE} if \code{x} is a formula. 
#' 
#' @export
#' 
#' @examples 
#' is_formula("this is a formula")
#' is_formula(f <- formula("y ~ x"))
#' is_formula(update(f, ~ . -1))
#' 
is_formula <-
function(x)
{
  typeof(x) == "language" && 
    inherits(x, "formula") && 
    (length(x) %in% c(2L, 3L))
}


#' @export
#' @rdname is_formula
#' 
is.formula <- 
function(x)
{
  .Deprecated("is_formula")
  is_formula(x)
}
