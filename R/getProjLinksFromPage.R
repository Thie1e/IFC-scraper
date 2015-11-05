#' Get all URLs of projects from a database page
#'
#' This function takes a URL of an 'overview page' of the IFC's database
#' (like http://ifcextapps.ifc.org/ifcext/spiwebsite1.nsf/frmShowView?openform&view=CRUDate&start=1&count=100&page=1)
#' as input and returns the URLs of all projects on that site.
#' @param URL (character) The URL of an overview page of the IFC's database
#' @return (character vector) All found project links


getProjLinksFromPage <- function(URL) {
    require(XML)
    doc <- htmlParse(URL)
    links <- xpathSApply(doc, "//a/@href")
    links <- links[grep(x = tolower(links), pattern = "opendocument")]
    links <- paste0("http://ifcextapps.ifc.org", links)
    free(doc)
    return(links)
}
