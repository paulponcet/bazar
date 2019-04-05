#' @title 
#' Convert to an empty object
#' 
#' @description 
#' Convert \code{x} to an empty object. 
#' 
#' @param x
#' An object. 
#' 
#' @param ...
#' Additional parameterS. 
#' 
#' @return 
#' An empty object
#' 
#' @seealso 
#' \code{\link[bazar]{is_empty}} in this package. 
#' 
#' @export
#' 
#' @examples 
#' x <- c("a", "b", "c")
#' as_empty(x)
#' class(as_empty(x)) # still a character
#' 
#' x <- factor(LETTERS)
#' as_empty(x)        # levels are kept
#' class(as_empty(x)) # still a factor
#' 
#' x <- data.frame(x = 1:3, y = 2:4)
#' as_empty(x)
#' 
as_empty <-
function(x, 
         ...)
{
  UseMethod("as_empty")
}


#' @export
#' @rdname as_empty
#' 
as.empty <- 
function(x, 
         ...)
{
  .Deprecated("as_empty")
  as_empty(x, ...)
}


#' @export
#' @rdname as_empty
#'
as_empty.default <-
function(x, 
         ...)
{
  x[0L]
}


#' @export
#' @rdname as_empty
#'
as_empty.data.frame <-
function(x, 
         ...)
{
  x[0L, ]
}
