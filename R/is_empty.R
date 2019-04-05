#' @title 
#' Test emptyness
#' 
#' @description 
#' These methods test if an object \code{x} is empty. 
#' 
#' @param x
#' An object to be tested. 
#' 
#' @return 
#' \code{TRUE} if \code{x} is empty, \code{FALSE} otherwise. 
#' 
#' @seealso 
#' \code{\link[bazar]{as_empty}} in this package. 
#' 
#' @export
#' 
#' @examples 
#' is_empty(4)
#' is_empty(c())
#' is_empty(new.env())
#' is_empty(character(0))
#' is_empty(list())
#' is_empty(integer(0))
#' is_empty(data.frame())
#' 
is_empty <- 
function(x)
{
  UseMethod("is_empty")
}


#' @export
#' @rdname is_empty
#' 
is.empty <- 
function(x)
{
  .Deprecated("is_empty")
  is_empty(x)
}


#' @export
#' @rdname is_empty
#' 
is_empty.default <- 
function(x)
{
  length(x)==0L
}


#' @export
#' @rdname is_empty
#' 
is_empty.data.frame <- 
function(x)
{
  nrow(x) == 0L #|| ncol(x) == 0L
}
