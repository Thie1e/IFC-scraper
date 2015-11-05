#' Get the project information from a project's HTML
#'
#' This is the main function of the IFCscraper package. It returns project
#' information given a project's HTML (from read_html) and the tabNames of the
#' project's page.
#' @param projectHTML Usually the result of rvest::read_html()
#' @param tabNames (character) Usually the result of getTabNames()
#' @return A list with the pieces of information about the project as
#' its elements


getProjectInfo <- function(projectHTML, tabNames) {
    require(rvest)
    require(stringr)
    require(countrycode)
    originalLocale <- Sys.getlocale(category = "LC_TIME")
    # For date conversion irrespective of OS language
    Sys.setlocale("LC_TIME", "C")

    # Get project NR
    projectNr <- projectHTML %>%
        html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'dataCell', ' ' ))]") %>%
        html_text(trim = T) %>%
        head(n = 1) %>%
        str_trim()
    projectHeading <- projectHTML %>%
        html_nodes("td") %>%
        html_text() %>%
        str_trim()

    # Get Region
    regionPosition <- grep(x = tolower(projectHeading), pattern = "^region.?$")
    if (length(regionPosition) > 1) {
        warning(paste("Multiple occurences of region found"))
    }
    if (length(regionPosition) == 1) {
        projectRegion <- projectHeading[regionPosition + 1]
    } else projectRegion <- "Not found"

    # Get 'previous events'
    previousEventsPosition <- grep(x = tolower(projectHeading), pattern = "^previous")
    if (length(previousEventsPosition) == 1) {
        previousEvents <- projectHeading[previousEventsPosition + 1]
        previousEvents <- cleanEnc(previousEvents)
    } else previousEvents <- "Not found"

    # Store Invested, Signed, Approved
    if (previousEvents != "Not found") {
        eventsSplit <- unlist(strsplit(previousEvents, " "))
        apprPos <- grep(tolower(eventsSplit), pattern = "approved:")
        if (length(apprPos) == 1) {
            Approved <- paste(eventsSplit[apprPos + 1:3], collapse = " ")
            Approved <- gsub(Approved, pattern = "[ |,]+", replacement = "-")
            Approved <- as.Date(Approved, format = "%B-%d-%Y")
            Approved <- as.character(Approved) # To prevent coercion to num
        } else {
            Approved <- "'Approved' not among previous events"
        }
        signedPos <- grep(tolower(eventsSplit), pattern = "signed:")
        if (length(signedPos) == 1) {
            Signed <- paste(eventsSplit[signedPos + 1:3], collapse = " ")
            Signed <- gsub(Signed, pattern = "[ |,]+", replacement = "-")
            Signed <- as.Date(Signed, format = "%B-%d-%Y")
            Signed <- as.character(Signed) # To prevent coercion to num
        } else {
            Signed <- "'Signed' not among previous events"
        }
    } else {
        Approved = Signed = "Not found"
    }

    # Extract content of the cost tab
    costTabNr <- grep(tabNames, pattern = '[C|c]ost')
    # If length == 0 no cost tab was found
    if (length(costTabNr) == 0) {
        info <- data.frame(matrix(rep("No cost tab", times = 4), nrow = 1),
                           stringsAsFactors = F)
        colnames(info) <- c("Risk Management", "Guarantee", "Loan", "Equity")
        sponsor = "No cost tab"
        totalCost = "No cost tab"
    } else {
        xp <- paste0("//*[(@id = 'divtab", costTabNr, "')]//td")
        tabContent <- projectHTML %>%
            html_nodes(xpath = xp) %>%
            html_text()

        # Store content of "general" cost info field
        fieldname <- "total project cost and amount and nature of ifc's investment"
        fieldposition <- grep(x = tolower(tabContent), pattern = paste0("^", fieldname))
        if (length(fieldposition) > 1) {
            warning(paste("Multiple occurences of", fieldname, "found"))
        }
        if (length(fieldposition) == 1) {
            totalCost <- tabContent[fieldposition + 1]
            totalCost <- cleanEnc(totalCost)
        } else totalCost <- "Not found"

        # Store content of sponsor field
        fieldname <- "project sponsor"
        fieldposition <- grep(x = tolower(tabContent), pattern = paste0("^", fieldname))
        if (length(fieldposition) > 1) {
            warning(paste("Multiple occurences of", fieldname, "found"))
        }
        if (length(fieldposition) == 1) {
            sponsor <- tabContent[fieldposition + 1]
            sponsor <- cleanEnc(sponsor)
        } else sponsor <- "Not found"

        # store table of project infos (not on every page)
        info <- data.frame(matrix(NA, nrow = 1, ncol = 4),
                           stringsAsFactors = F)
        colnames(info) <- c("Risk Management", "Guarantee", "Loan", "Equity")
        info[1, ] <- sapply(colnames(info), function(x) {
            pat <- paste0("^", x)
            categoryIndex <- grep(tabContent, pattern = pat)
            if (length(categoryIndex) >= 1) {
                cellValue <- tabContent[categoryIndex + 1]
                cellValue <- cleanEnc(cellValue)
                return(cellValue)
            } else return("Not found")
        })
    }

    # Extract content of the contact tab
    contactTabNr <- grep(tabNames, pattern = '[C|c]ontacts')
    # If length == 0 no cost tab was found
    if (length(contactTabNr) == 0) {
        enterpriseBase <- "Not found"
    } else {
        xp <- paste0("//*[(@id = 'divtab", contactTabNr, "')]//td")
        tabContent <- projectHTML %>%
            html_nodes(xpath = xp) %>%
            html_text(trim = T) %>%
            cleanEnc()
        # Store content of "for inquiries..." field
        fieldname <- "for inquiries about the project, contact:"
        fieldposition <- which(tolower(tabContent) == fieldname)
        if (length(fieldposition) >= 1) {
            enterpriseBase <- tabContent[fieldposition + 1]
            enterpriseBase <- sapply(countrycode_data$country.name, function(x) {
                # There should be a space or end of line after the country name
                # otherwise e.g. Dominica is found if country = Dominican Rep.
                pat <- paste0(tolower(x), "[ |$|,|\\.|:]")
                countryFound <- grep(tolower(enterpriseBase), pattern = pat)
            })
            enterpriseBase <- unlist(enterpriseBase)
            enterpriseBase <- names(enterpriseBase)
            if (length(enterpriseBase) > 1) {
                enterpriseBase <- paste(enterpriseBase, collapse = " and/or ")
            }
            if (is.null(enterpriseBase)) {
                enterpriseBase <- "Not found or country name unknown"
            }
        } else enterpriseBase <- "Not found"
    }

    Sys.setlocale("LC_TIME", originalLocale)
    return(list(projectNr = projectNr,
                projectRegion = projectRegion,
                sponsor = sponsor,
                Approved = Approved,
                Signed = Signed,
                enterpriseBase = enterpriseBase,
                totalProjectCost = totalCost,
                riskManagement = info$`Risk Management`,
                guarantee = info$Guarantee,
                loan = info$Loan,
                equity = info$Equity
    ))
}
