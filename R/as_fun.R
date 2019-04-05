#' @title 
#' Convert object to function
#' 
#' @description 
#' \code{as_fun} is a generic function that does the same as \code{as.function} 
#' from package \pkg{base}, with the additional feature that 
#' \code{as_fun.character} converts a string into the function it names. 
#' 
#' @param x
#' The object to convert.
#' 
#' @param envir
#' Environment in which the function should be defined. 
#' 
#' @param ...
#' Additional arguments (currently not used). 
#' 
#' @return 
#' The desired function. 
#' 
#' @author 
#' \code{as_fun.character} is adapted from MrFlick, 
#' see \url{https://stackoverflow.com/a/38984214} on StackOverflow. 
#' 
#' @export
#' 
#' @seealso \code{\link[rlang]{as_function}} in package \pkg{rlang}. 
#' 
#' @examples 
#' as_fun(mean)
#' as_fun("mean")
#' as_fun("edit")
#' as_fun("stats::predict")
#' 
#' ## Uses 'rlang::as_function()' for formulas under the hood: 
#' f <- as_fun(~ . + 1)
#' f(10) # 11
#' 
#' ## the constant function '1'
#' f <- as_fun(1)
#' f(2)   # 1
#' f("a") # 1
#' 
#' ## the constant function 'FALSE'
#' f <- as_fun(FALSE)
#' f(2)   # FALSE
#' f("a") # FALSE
#' 
#' f <- as_fun(data.frame(x = 1:2, y = 2:3))
#' f("x") # 'x' column
#' f("y") # 'y' column
#' 
as_fun <- 
function(x, 
         ...)
{
  UseMethod("as_fun")  
}


#' @export
#' @rdname as_fun
#' 
as.fun <- 
function(x, 
         ...)
{
  .Deprecated("as_fun")
  as_fun(x, ...)
}


#' @export
#' @rdname as_fun
#' 
as_fun.default <- 
function(x, 
         envir = parent.frame(),
         ...)
{
  if (is.null(envir)) envir <- baseenv()
  as.function.default(x, envir = envir, ...)
}


#' @export
#' @rdname as_fun
#' 
as_fun.character <-
function(x,
         ...)
{
  if (grepl(":::", x)) {
    ns <- strsplit(x, ":::")[[1L]]
    x <- ns[2L]
    w <- 1L
  } else if (grepl("::", x)) {
    ns <- strsplit(x, "::")[[1L]]
    x <- ns[2L]
    w <- 1L
  } else {
    ns <- .packages(all.available = FALSE) #unlist(sessionPackages(), use.names = FALSE)
    w <- which(vapply(ns, 
                      FUN = function(n) { x %in% getNamespaceExports(n) }, 
                      FUN.VALUE = logical(1L)))
    if (length(w) > 1L) 
      stop(paste0("several packages export '", x, "', please use ::"), call. = FALSE)
    if (length(w) == 0L) 
      stop(paste0("'", x, "' is not exported by the packages currently loaded"), 
           call. = FALSE)
  }
  f <- getExportedValue(ns[w], x)
  #formals(f) = rlist::list.merge(formals(f), nlist(...))
  structure(f, fun = x, package = ns[w])
}


#' @export
#' @rdname as_fun
#' 
as_fun.formula <- 
function(x, 
         ...)
{
  rlang::as_function(x, ...)
}


#' @export
#' @rdname as_fun
#' 
as_fun.name <- 
function(x, 
         ...)
{
  as_fun(as.character(x), ...)
}


#' @export
#' @rdname as_fun
#' 
as_fun.call <- 
function(x, 
         ...)
{
  as_fun(as.character(x[[1L]]), ...)
}


#' @export
#' @rdname as_fun
#' 
as_fun.numeric <-
function(x,
         ...)
{
  function(...) { x }
}


#' @export
#' @rdname as_fun
#' 
as_fun.logical <- as_fun.numeric


#' @export
#' @rdname as_fun
#' 
as_fun.factor <- as_fun.numeric


#' @export
#' @rdname as_fun
#' 
as_fun.complex <- as_fun.numeric


#' @export
#' @rdname as_fun
#' 
as_fun.data.frame <- 
function(x, 
         ...)
{
  f <- function(n)
  {
    n <- match.arg(n, names(x))
    x[[n]]
  }
  formals(f) <- list(n = names(x))
  f
}


#' @importFrom stats predict 
#' @export
#' @rdname as_fun
#' 
as_fun.lm <- 
function(x, 
         ...)
{
  ## Name of the X variables 
  ..x <- x
  rm(x)
  ..n <- stats::formula(..x)
  ..n <- all.vars(..n)[-1]
  if ("..x" %in% ..n) {
    stop("the model's formula contains a variable called '..x', 
         'as_fun()' does not work in this specific case")
  }
  if ("..n" %in% ..n) { 
    stop("the model's formula contains a variable called '..n', 
         'as_fun()' does not work in this specific case")
  }
  
  ## Creation of the function to be returned, with no arguments yet
  f <- function()
  {
    df <- as.data.frame(as.list(environment()))
    names(df) <- ..n
    p <- stats::predict(..x, newdata = df, type = default_type(..x), ...)
    if (is.list(p)) {
      if (!is.null(p$fit)) {
        y <- p$fit
      } else if (!is.null(p$pred)) {
        y <- p$pred
      } else {
        stop("cannot find predicted values")
      }
    } else {
      y <- p
    }
    unname(y)
  }

  ## 'l' is the list used to name the arguments of the function 'f()'
  l <- replicate(length(..n), substitute())
  names(l) <- ..n
  formals(f) <- l
  f
}


#' @export
#' @rdname as_fun
#' 
as_fun.rpart <- as_fun.lm
# TODO: essayer de voir si le Y du rpart est un factor... 


#' @importFrom stats as.stepfun
#' @export
#' 
as_fun.isoreg <- 
function(x, 
         ...)
{
  stats::as.stepfun(x, ...)
}


default_type <- 
function(obj)
{
  UseMethod("default_type")
}

default_type.default <- 
function(obj)
{
  "response"
}

default_type.rpart <- 
function(obj)
{
  "vector"
}

default_type.train <- 
function(obj)
{
  "raw"
}
