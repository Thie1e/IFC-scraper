#' Get the names of the tabs at the bottom of a project's page
#'
#' This function returns the names of the tabs (if any) that may be at the
#' bottom of a project's page.
#' @param projectHTML Usually the output of readr::read_html() applied to the
#' URL of a single project
#' @return (character vector) The names of the tabs at the bottom of the page


getTabNames <- function(projectHTML) {
    # Find tab that contains cost
    # Get names of all tabs
    require(rvest)
    require(stringr)
    latestTab <- c()
    tabNames <- c()
    tabNr <- 1
    # Stop loop if latestTab is character(0)
    while (!identical(latestTab, character(0))) {
        tabToTry <- paste0('#tab', tabNr)
        latestTab <- projectHTML %>%
            html_nodes(tabToTry) %>%
            html_text(trim = T) %>%
            cleanEnc() %>% # Doesn't always work here, why?
            str_trim()
        tabNames <- c(tabNames, latestTab)
        tabNr <- tabNr + 1
    }
    return(tabNames)
}
