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

# keepTrying("x", "rnorm(10)")
#
#
#
# keepTrying <- function(call, maxTries = 5, timeout = 5,
#                        noTermination = F, silent = F) {
#     stopifnot(is.character(call))
#     result <- NA
#     class(result) <- "try-error"
#     nTries <- 0
#     while ("try-error" %in% class(result)) {
#         result <- try(eval(parse(text = call), envir = globalenv()), silent = silent)
#         if ("try-error" %in% class(result)) {
#             nTries <- nTries + 1
#             if (nTries >= maxTries & noTermination) {
#                 result <- NULL
#                 break
#             } else if (nTries >= maxTries & !noTermination) {
#                 stop ("maxTries reached")
#             }
#         }
#         if ("try-error" %in% class(result)) {
#             if (silent == F) message(paste("Trying again in", timeout, "sec."))
#             Sys.sleep(timeout)
#         }
#     }
#     # return(result)
# }
#
# keepTrying("5+2")
