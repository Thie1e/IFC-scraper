library(rvest)
library(stringr)
library(XML)
library(countrycode)

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

IFCprojects <- lapply(projectList, unlist)
IFCprojects <- do.call(rbind, IFCprojects) # We don't want warnings here!
IFCprojects <- data.frame(IFCprojects, stringsAsFactors = F)
IFCprojects$projectNr <- as.numeric(IFCprojects$projectNr) # Coerces 'Not found' to NA
IFCprojects$projectNr[is.na(IFCprojects$projectNr)] <- "error"
IFCprojects <- IFCprojects[order(IFCprojects$projectNr, decreasing = F), ]
View(IFCprojects)
write.table(IFCprojects, file = "IFC.csv", row.names = F)
save(IFCprojects, file = "IFCprojects.RData")

# Merge by project Nr
IFCprojectsMerged <- IFCprojects
NA_strings <- c("Not found", "No cost tab", "'Approved' not among previous events",
                "'Signed' not among previous events", " ")
IFCprojectsMerged <- apply(IFCprojectsMerged, 2, function(x) {
    temp <- x
    temp[temp %in% NA_strings] <- NA
    temp <- str_trim(temp)
    return(temp)
})
IFCprojectsMerged <- data.frame(IFCprojectsMerged, stringsAsFactors = F)
IFCprojectsMerged$projectNr <- as.numeric(IFCprojectsMerged$projectNr)
IFCprojectsMerged$projectNr[is.na(IFCprojectsMerged$projectNr)] <- "error"
# Put both URLs (if possible) into URL column
UrlPerProject <- sapply(IFCprojectsMerged$projectNr,
                        function(x) IFCprojectsMerged[IFCprojectsMerged$projectNr == x, "URL"])
UrlPerProject <- sapply(UrlPerProject, function(x) paste(sort(x), collapse = " "))
UrlPerProject <- data.frame(projectNr = as.character(names(UrlPerProject)),
                            URL = UrlPerProject, stringsAsFactors = F)
IFCprojectsMerged$URL <- NULL
IFCprojectsMerged$URL <- sapply(IFCprojectsMerged$projectNr, function(x) {
    insert <- UrlPerProject[UrlPerProject$projectNr == x, "URL"]
    insert <- sort(unique(insert))
    insert <- paste(insert, collapse = " ")
    return(insert)
})

# Check if there are no conflicting values for a project
projectNumbers <- unique(IFCprojectsMerged$projectNr)
projectNumbers <- projectNumbers[projectNumbers != "error"]
projWithConflicts <- c()
for (i in seq_along(projectNumbers)) {
    tempdat <- IFCprojectsMerged[IFCprojectsMerged$projectNr == projectNumbers[i], ]
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

IFCprojectsMerged <- aggregate(x = IFCprojectsMerged,
          by = list(IFCprojectsMerged$projectNr),
          FUN = function(x) na.omit(x)[1])[,-1]

save(IFCprojectsMerged, file = "IFCprojectsMerged.RData")
write.table(IFCprojectsMerged, file = "IFC_merged.csv", row.names = F)
