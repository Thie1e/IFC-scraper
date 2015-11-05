#' Convert from UTF-8 and remove line break indicators
#'
#' On windows (my personal experience with Windows 10) there are
#' enconding problems which lead
#' to characters like Â or â€™. Converts encoding from UTF-8 and also removes
#' line break indicators.
#' There were no problems on Ubuntu, but the function doesn't do any harm on Linux.
#' @param string (character vector) The character string to be converted and cleaned
#' @return (character vector) The 'cleaned' and converted string


cleanEnc <- function(string) {
    stringClean <- iconv(string, from = "UTF-8")
    stringClean <- gsub(x = stringClean, pattern = "\\n+", replacement = " ")
    return(stringClean)
}
