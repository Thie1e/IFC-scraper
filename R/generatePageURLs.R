#' Generate URLs of the database overview pages
#'
#' This function generates a speciefied number of database pages for the
#' project databse of the IFC. First page here
#' http://ifcextapps.ifc.org/ifcext/spiwebsite1.nsf/frmShowView?openform&view=CRUDate&start=1&count=100&page=1
#' The returned pages start at page one and end at page 'pages'.
#' @param pages The number of page URLs to return
#' @return (character vector) The desired number of URLs

generatePageURLs <- function(pages) {
    databasePageURLs <- rep(NA, times = pages)
    for (p in 1:pages) {
        databasePageURLs[p] <- paste0("http://ifcextapps.ifc.org/ifcext/spiwebsite1.nsf/frmshowview?openform&view=CRUDate&",
                                      "start=", 100 * p - 99,
                                      "&count=100&page=", p,
                                      "&doccount=6770")
    }
    return(databasePageURLs)
}
