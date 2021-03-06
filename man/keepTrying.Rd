% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/keepTrying.R
\name{keepTrying}
\alias{keepTrying}
\title{Keep trying to assign the result of a call to a specified object}
\usage{
keepTrying(target, call, maxTries = 5, timeout = 5, noTermination = F,
  silent = F)
}
\arguments{
\item{target}{(character) The name under which the result of call should
be stored}

\item{call}{(character) The call as a character string}

\item{maxTries}{Try this number of times at maximum}

\item{timeout}{The timeout between tries}

\item{noTermination}{If False (default False) the try-error will be saved in
the object of name 'target'. Otherwise the function throws an error via
stop() which will interrupt a function that keepTrying is running inside of.}
}
\description{
This function tries a certain number of time (maxTries) to assign the result
of 'call' to an object called 'target' in the parent frame. Useful for
calls that may fail sometimes, e.g. downloading.
}
\examples{
> keepTrying("theAnswer", "2 + 40")
> theAnswer
42

> keepTrying("thisFails", "2 * 'abc'", maxTries = 2, noTermination = T)
Error in 2 * "abc" : non-numeric argument to binary operator
Trying again in 5 sec.
Error in 2 * "abc" : non-numeric argument to binary operator
> thisFails
[1] "Error in 2 * \\"abc\\" : non-numeric argument to binary operator\\n"
attr(,"class")
[1] "try-error"
attr(,"condition")
<simpleError in 2 * "abc": non-numeric argument to binary operator>
}

