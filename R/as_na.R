#' @title 
#' Transform values to NA
#' 
#' @description 
#' These methods transform values to \code{\link[base]{NA}} 
#' for different classes of objects. 
#' 
#' @param x
#' The object at stake. 
#' 
#' @param ...
#' Additional arguments (unused).
#' 
#' @return 
#' An object of the same class as \code{x}; 
#' the attributes of \code{x} are passed unchanged to the result. 
#' 
#' @export
#' 
#' @examples 
#' x <- c("a", "b", "c")
#' as_na(x)
#' class(as_na(x)) # still a character
#' 
#' x <- factor(LETTERS)
#' as_na(x)        # levels are kept
#' class(as_na(x)) # still a factor
#' 
#' x <- data.frame(x = 1:3, y = 2:4)
#' as_na(x)
#' dim(as_na(x))
#' 
#' x <- matrix(1:6, 2, 3)
#' attr(x, "today") <- Sys.Date()
#' as_na(x)        # attributes are kept
#'
as_na <-
function(x, 
         ...)
{
  UseMethod("as_na")
}


#' @export
#' @rdname as_na
#' 
as.na <- 
function(x, 
         ...)
{
  .Deprecated("as_na")
  as_na(x, ...)
}


#' @export
#' @rdname as_na
#'
as_na.default <-
function(x, 
         ...)
{
  w <- seq_along(x)
  x[w] <- rep(NA, length(w))
  x
}


#' @export
#' @rdname as_na
#'
as_na.data.frame <-
function(x, 
         ...)
{
  y <- as.data.frame(lapply(x, FUN = as_na, ...), 
                     stringsAsFactors = FALSE)
  attributes(y) <- attributes(x)
  y
}


#' @export
#' @rdname as_na
#'
as_na.list <-
function(x, 
         ...)
{
  y <- lapply(x, FUN = as_na, ...)
  attributes(y) <- attributes(x)
  y
}
