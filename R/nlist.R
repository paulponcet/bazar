#' @title 
#' Named lists
#' 
#' @description 
#' Functions to construct, coerce and check for named lists. 
#' 
#' @param ...
#' Named objects. 
#' 
#' @param x
#' Object to be coerced or tested. 
#' 
#' @return 
#' A named list. 
#'  
#' @importFrom tibble lst
#' @export
#' 
#' @examples 
#' x <- nlist(x = 2, y = c("a", "b"))
#' is_nlist(x)
#'
nlist <- 
function(...)
{
  x <- tibble::lst(...)
  if (is_empty(x)) {
    names(x) <- character(0)
  }
  x
}


#' @export
#' @rdname nlist
#' 
as_nlist <-
function(x, 
         ...)
{
  y <- as.list(x, ...)
  names(y) <- names(x)
  if (is_empty(y)) return(nlist())
  if (!is_nlist(y)) stop("cannot convert 'x' into a named list", call. = FALSE)
  y
}


#' @export
#' @rdname nlist
#' 
is_nlist <- 
function(x)
{
  is.list(x) && 
    !is.null(names(x)) && 
    "" %nin% names(x)
}


#' @export
#' @rdname nlist
#' 
as.nlist <- 
function(x, 
         ...)
{
  .Deprecated("as_nlist")
  as_nlist(x, ...)
}


#' @export
#' @rdname nlist
#' 
is.nlist <- 
function(x)
{
  .Deprecated("is_nlist")
  is_nlist(x)
}
