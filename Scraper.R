library(rvest)
library(stringr)
library(XML)
library(countrycode)

setwd("/home/khl4v/Seafile/Crypt2/R/InternationalFinanceCorp_Scraper/")
setwd("E:\\Seafile\\Crypt2\\R\\InternationalFinanceCorp_Scraper\\")


# Generate database pages --------------------------------------------------
source("functions.R")
source("keepTrying.R")
# Look up manually at
# http://ifcextapps.ifc.org/ifcext/spiwebsite1.nsf/frmShowView?openform&view=CRUDate&start=1&count=100&page=1
pages <- 68
databasePageURLs <- generatePageURLs(pages)
projectList <- list()
nProjects <- 100 * pages # approximate number of projects to load
nLoaded <- 1
# Laufzeit für eine Seite mit timeout zwischen 2 und 5: ca. 6,5 Minuten
# Theoretische, komplette Laufzeit ohne Pausen: 45-60 Minuten
for (page in 1:(length(databasePageURLs))) {
    # To limit stress to the site
    Sys.sleep(runif(1, min = 2, max = 5))
    cat(paste0("Getting project links from database page ", page, "..."))
    projLinks <- getProjLinksFromPage(databasePageURLs[page])
    cat(" done. \n")
    # TEST
    # projLinks <- head(projLinks, 5)
    for (pnr in seq_along(projLinks)) {
        # To limit stress to the site
        Sys.sleep(runif(1, min = 2, max = 5))
        cat(paste0(round(nLoaded / nProjects, 4) * 100, "% ",
                   Sys.time(), " Downloading and reading... "))

        # projectHTML <- read_html(projLinks[pnr], encoding = "Latin-1")
        keepTrying("projectHTML",
                   "read_html(projLinks[pnr], encoding = 'Latin-1')",
                   maxTries = 10, timeout = 10, noTermination = T, silent = F)

        if ("try-error" %in% class(projectHTML)) { # = download error
            errMessage <- attributes(projectHTML)$condition$message
            projInfo <- rep(list(errMessage), 12)
            names(projInfo) <- names(projectList[[1]])
            projInfo$URL <- projLinks[pnr]
        } else {
            tabNames <- getTabNames(projectHTML)
            projInfo <- getProjectInfo(projectHTML, tabNames)
            projInfo$URL <- projLinks[pnr]
        }

        projectList[[length(projectList) + 1]] <- projInfo

        nLoaded <- nLoaded + 1
        cat(paste0(" project ", projInfo$projectNr, " done. \n"))
    }
    # Safety measure: Store list after finishing a page
    save(projectList, file = "projectList_backup.RData")
}

result <- lapply(projectList, unlist)
result <- do.call(rbind, result) # We don't want warnings here!
result <- data.frame(result, stringsAsFactors = F)
result$projectNr <- as.numeric(result$projectNr) # Coerces 'Not found' to NA
result$projectNr[is.na(result$projectNr)] <- "error"
result <- result[order(result$projectNr, decreasing = F), ]
View(result)
write.table(result, file = "IFC.csv", row.names = F)
save(result, file = "result.RData")

# Merge by project Nr
resultMerged <- result
NA_strings <- c("Not found", "No cost tab", "'Approved' not among previous events",
                "'Signed' not among previous events", " ")
resultMerged <- apply(resultMerged, 2, function(x) {
    temp <- x
    temp[temp %in% NA_strings] <- NA
    temp <- str_trim(temp)
    return(temp)
})
resultMerged <- data.frame(resultMerged, stringsAsFactors = F)
resultMerged$projectNr <- as.numeric(resultMerged$projectNr)
resultMerged$projectNr[is.na(resultMerged$projectNr)] <- "error"
# Put both URLs (if possible) into URL column
UrlPerProject <- sapply(resultMerged$projectNr,
                        function(x) resultMerged[resultMerged$projectNr == x, "URL"])
UrlPerProject <- sapply(UrlPerProject, function(x) paste(sort(x), collapse = " "))
UrlPerProject <- data.frame(projectNr = as.character(names(UrlPerProject)),
                            URL = UrlPerProject, stringsAsFactors = F)
resultMerged$URL <- NULL
resultMerged$URL <- sapply(resultMerged$projectNr, function(x) {
    insert <- UrlPerProject[UrlPerProject$projectNr == x, "URL"]
    insert <- sort(unique(insert))
    insert <- paste(insert, collapse = " ")
    return(insert)
})

# Check if there are no conflicting values for a project
projectNumbers <- unique(resultMerged$projectNr)
projectNumbers <- projectNumbers[projectNumbers != "error"]
projWithConflicts <- c()
for (i in seq_along(projectNumbers)) {
    tempdat <- resultMerged[resultMerged$projectNr == projectNumbers[i], ]
    if (nrow(tempdat) > 1) {
        if (nrow(tempdat) > 2) {
            print(paste("Project", projectNumbers[i], "has more than 2 pages"))
        }
        for (col in 1:ncol(tempdat)){
            tempcol <- na.omit(tempdat[, col])
            if (length(tempcol) > 1) {
                if (!identical(tempcol[1], tempcol[2])) {
                    print(paste("Project", projectNumbers[i], "Conflict of values"))
                    projWithConflicts <- c(projWithConflicts, projectNumbers[i])
                }
            }
        }
    }
}
# No output = OK
# There are actually projects that have more than 2 pages
projWithConflicts <- as.numeric(projWithConflicts)
# These have to be checked manually in Excel, some text fields are differing

resultMerged <- aggregate(x = resultMerged,
          by = list(resultMerged$projectNr),
          FUN = function(x) na.omit(x)[1])[,-1]

write.table(resultMerged, file = "IFC_merged.csv", row.names = F)

resultMerged_sample <- draw(resultMerged, n = 100)
write.table(resultMerged_sample, file = "IFC_merged_sample.csv", row.names = F, sep = ";")

