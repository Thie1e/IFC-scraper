#' Keep trying to assign the result of a call to a specified object
#'
#' This function tries a certain number of time (maxTries) to assign the result
#' of 'call' to an object called 'target' in the parent frame. Useful for
#' calls that may fail sometimes, e.g. downloading.
#' @param target (character) The name under which the result of call should
#' be stored
#' @param call (character) The call as a character string
#' @param maxTries Try this number of times at maximum
#' @param timeout The timeout between tries
#' @param noTermination If False (default False) the try-error will be saved in
#' the object of name 'target'. Otherwise the function throws an error via
#' stop() which will interrupt a function that keepTrying is running inside of.
#' @examples
#' > keepTrying("theAnswer", "2 + 40")
#' > theAnswer
#' 42
#'
#' > keepTrying("thisFails", "2 * 'abc'", maxTries = 2, noTermination = T)
#'Error in 2 * "abc" : non-numeric argument to binary operator
#'Trying again in 5 sec.
#'Error in 2 * "abc" : non-numeric argument to binary operator
#'> thisFails
#'[1] "Error in 2 * \"abc\" : non-numeric argument to binary operator\n"
#'attr(,"class")
#'[1] "try-error"
#'attr(,"condition")
#'<simpleError in 2 * "abc": non-numeric argument to binary operator>
#'


keepTrying <- function(target, call, maxTries = 5, timeout = 5,
                       noTermination = F, silent = F) {
    stopifnot(is.character(target) & is.character(call))
    result <- NA
    class(result) <- "try-error"
    nTries <- 0
    while ("try-error" %in% class(result)) {
        result <- try(eval(parse(text = call)), silent = silent)
        if ("try-error" %in% class(result)) {
            nTries <- nTries + 1
            if (nTries >= maxTries & noTermination) {
                break
            } else if (nTries >= maxTries & !noTermination) {
                stop ("maxTries reached")
            }
        }
        if ("try-error" %in% class(result)) {
            if (silent == F) message(paste("Trying again in", timeout, "sec."))
            Sys.sleep(timeout)
        }
    }
    assign(x = target,
           value = result,
           envir = parent.frame())
}

