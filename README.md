# Visualization tools for SEER-Medicare publications data

This is a respository with our R package for producing the SEER Medicare report available on our website.  It contains the scraped data used to prepare the report, and the code used to generate the figures and tables.  See http://www.outins.com/seer-medicare for the results.

## Quick start

The package includes a CDC WONDER request template at `inst/extdata/cdc_wonder_request.xml`.
Use it with `scrape_all_data()` and then generate the Word/PDF report:

```r
library(seermedicare)

xml_path <- system.file("extdata", "cdc_wonder_request.xml", package = "seermedicare", mustWork = TRUE)
seer_pub_data <- scrape_all_data(cdc_wonder_request_xml = xml_path, cdc_year = "2022")

generate_report(seer_pub_data, outputPath = "output", pdf = TRUE)
```
