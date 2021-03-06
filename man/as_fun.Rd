% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_fun.R
\name{as_fun}
\alias{as_fun}
\alias{as.fun}
\alias{as_fun.default}
\alias{as_fun.character}
\alias{as_fun.formula}
\alias{as_fun.name}
\alias{as_fun.call}
\alias{as_fun.numeric}
\alias{as_fun.logical}
\alias{as_fun.factor}
\alias{as_fun.complex}
\alias{as_fun.data.frame}
\alias{as_fun.lm}
\alias{as_fun.rpart}
\title{Convert object to function}
\usage{
as_fun(x, ...)

as.fun(x, ...)

\method{as_fun}{default}(x, envir = parent.frame(), ...)

\method{as_fun}{character}(x, ...)

\method{as_fun}{formula}(x, ...)

\method{as_fun}{name}(x, ...)

\method{as_fun}{call}(x, ...)

\method{as_fun}{numeric}(x, ...)

\method{as_fun}{logical}(x, ...)

\method{as_fun}{factor}(x, ...)

\method{as_fun}{complex}(x, ...)

\method{as_fun}{data.frame}(x, ...)

\method{as_fun}{lm}(x, ...)

\method{as_fun}{rpart}(x, ...)
}
\arguments{
\item{x}{The object to convert.}

\item{...}{Additional arguments (currently not used).}

\item{envir}{Environment in which the function should be defined.}
}
\value{
The desired function.
}
\description{
\code{as_fun} is a generic function that does the same as \code{as.function} 
from package \pkg{base}, with the additional feature that 
\code{as_fun.character} converts a string into the function it names.
}
\examples{
as_fun(mean)
as_fun("mean")
as_fun("edit")
as_fun("stats::predict")

## Uses 'rlang::as_function()' for formulas under the hood: 
f <- as_fun(~ . + 1)
f(10) # 11

## the constant function '1'
f <- as_fun(1)
f(2)   # 1
f("a") # 1

## the constant function 'FALSE'
f <- as_fun(FALSE)
f(2)   # FALSE
f("a") # FALSE

f <- as_fun(data.frame(x = 1:2, y = 2:3))
f("x") # 'x' column
f("y") # 'y' column

}
\seealso{
\code{\link[rlang]{as_function}} in package \pkg{rlang}.
}
\author{
\code{as_fun.character} is adapted from MrFlick, 
see \url{https://stackoverflow.com/a/38984214} on StackOverflow.
}
