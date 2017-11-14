#' @title 
#' Convert object to function
#' 
#' @description 
#' \code{as.fun} is a generic function that does the same as \code{as.function} 
#' from package \pkg{base}, with the additional feature that 
#' \code{as.fun.character} converts a string into the function it names. 
#' 
#' @param x
#' The object to convert.
#' 
#' @param envir
#' Environment in which the function should be defined. 
#' 
#' @param ...
#' Additional arguments, not used currently. 
#' 
#' @return 
#' The desired function. 
#' 
#' @author 
#' Adapted from MrFlick, 
#' see \url{https://stackoverflow.com/a/38984214} on StackOverflow. 
#' 
#' @export
#'
#' @examples 
#' as.fun(mean)
#' as.fun("mean")
#' as.fun("edit")
#' as.fun("stats::predict")
#' 
#' f <- as.fun(1)
#' f(2)   # 1
#' f("a") # 1
#' 
#' f <- as.fun(FALSE)
#' f(2)   # FALSE
#' f("a") # FALSE
#' 
#' f <- as.fun(data.frame(x = 1:2, y = 2:3))
#' f("x") # 'x' column
#' f("y") # 'y' column
#' 
as.fun <- 
function(x, 
         ...)
{
  UseMethod("as.fun")  
}


#' @export
#' @rdname as.fun
#' 
as.fun.default <- 
function(x, 
         envir = parent.frame(),
         ...)
{
  as.function.default(x, envir = envir, ...)
}


#' @export
#' @rdname as.fun
#' 
as.fun.character <-
function(x,
         ...)
{
  if (grepl("::", x)) {
    ns <- strsplit(x, "::")[[1L]]
    x <- ns[2L]
    w <- 1L
  } else {
    ns <- unlist(sessionPackages(), use.names = FALSE)
    w <- which(vapply(ns, 
                      FUN = function(n) { x %in% getNamespaceExports(n) }, 
                      FUN.VALUE = logical(1L)))
    if (length(w) > 1L) 
      stop(paste0("several packages export '", x, "', please use ::"))
    if (length(w) == 0L) 
      stop(paste0("'", x, "' is not exported by the packages currently loaded"))
  }
  structure(getExportedValue(ns[w], x), fun = x, package = ns[w])
}


#' @export
#' @rdname as.fun
#' 
as.fun.name <- 
function(x, 
         ...)
{
  as.fun(as.character(x), ...)
}


#' @export
#' @rdname as.fun
#' 
as.fun.numeric <-
function(x,
         ...)
{
  function(...) { x }
}


#' @export
#' @rdname as.fun
#' 
as.fun.logical <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.factor <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.complex <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.data.frame <- 
function(x, 
         ...)
{
  function(n = names(x))
  {
    n <- match.arg(n, names(x))
    x[[n]]
  }
}
