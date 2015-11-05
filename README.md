### International Finance Corporation Scraper
A scraper for fetching project information from [the IFC's database.](http://ifcextapps.ifc.org/ifcext/spiwebsite1.nsf/frmShowView?openform&amp;view=CRUDate&amp;start=1&amp;count=100&amp;page=1)

There's a blog post describing the scraper at http://thie1e.github.io/ScrapingIFC/.

An example application (scraping the complete database) is in Scraper.R. The resulting data is already included in the package and can be accessed via `IFCprojects` and `IFCprojectsMerged` after loading the package.

**Work in progress. Still fails to scrape certain pieces of information.**

### Installation
    library(devtools)
    install_github('thie1e/IFC-scraper')
