% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternary_operator.R
\name{\%?\%}
\alias{\%?\%}
\alias{\%:\%}
\title{If-Then-Else ternary operator}
\usage{
condition \%?\% true

lhs \%:\% false
}
\arguments{
\item{condition}{logical. A vector.}

\item{true, false}{Values to use for \code{TRUE} and \code{FALSE} values of \code{condition}. 
They must be either the same length as \code{condition}, or length \code{1}.}

\item{lhs}{Left-hand side of \code{\%:\%}, which should come from the result of a 
\code{\%?\%} call.}
}
\value{
If \code{length(x) > 1}, then \code{\link{ifelse}} is used.
}
\description{
This is a C like ternary operator, the syntax being 
\code{condition \%?\% true \%:\% false}.
}
\examples{
(capitalize <- sample(c(TRUE, FALSE), 1))
capitalize \%?\% LETTERS[1:3] \%:\% letters[1:2]

# Does not work
\dontrun{
capitalize \%?\% 1*1:3 \%:\% 1:2
}

# Does work
capitalize \%?\% {1*1:3} \%:\% 1:2

# Does work too
capitalize \%?\% (1*1:3) \%:\% 1:2

# Vectorized version also works
c(capitalize,!capitalize) \%?\% "A" \%:\% c("b","c")

# Chaining operators is permitted 
FALSE \%?\% "a" \%:\% 
  (FALSE \%?\% "b") \%:\% 
  (capitalize \%?\% "C") \%:\% "c" 

}
\author{
Richie Cotton, see \url{https://stackoverflow.com/a/8791496/3902976}; 
Paul Poncet for the small modifications introduced.
}
