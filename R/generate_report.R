#' Generate Full Report: Overview of All SEER-Medicare Publications
#'
#' @param seer_pub_data Result of running scrape_all_data.  Should have class "scrape_all_data"
#' @param outputPath Path to directory where report will be generated. Default is working directory
#' @param html Logical value - if TRUE, will generate html file; if FALSE, will generate Microsoft Word file
#'
#' @return doc Object of class \code{\link{docx}} or \code{\link{bsdoc}} containing SEER-Medicare report
#' @import magrittr ReporteRs ReporteRsjars rJava
#' @export
#'
#' @examples \dontrun{
#' generate_report()
#' }
generate_report <- function(seer_pub_data, outputPath = ".", html = FALSE){

  if(class(seer_pub_data) != "seermedicare") {
    stop("seer_pub_data argument must be of class seermedicare")
  }

  metadata <- seer_pub_data$metadata
  publications <- seer_pub_data$publications
  sites <- seer_pub_data$site_summary

  # SET LIST FORMATS AND OPTIONS
  mylist <- list(
    ol.left = seq(from = 0.5, by = 0.5, length.out = 9),
    ol.hanging = rep(0.4, 9),
    ol.format = rep("decimal", 9),
    ol.pattern = c("%1.", "%2.", "%3.", "%4.", "%5.", "%6.", "%7.", "%8.", "%9."),
    ul.left = seq(from = 0, by = 0.5, length.out = 9),
    ul.hanging = rep(0.4, 9),
    ul.format = c("disc", rep(c("circle", "disc"), 4))
    )

  options("ReporteRs-list-definition" = mylist, "ReporteRs-default-font" = "Times New Roman")

  if (html) {
    doc = bsdoc(title = "SEERMedicare")
  } else {
    doc = docx(title = "SEERMedicare.docx", template = system.file("extdata", "ReportTemplate.docx", package = "seermedicare", mustWork = T)) %>%
      map_title(stylenames = c("Heading1", "Heading2", "Title", "Subtitle"))
  }

  all_sites <- get_data("sites", publications)$values %>% levels

  message("Generating Title")

  doc = doc %>%
    addTitle("Overview of All SEER-Medicare Publications", 3) %>%
    addTitle("Outcomes Insights, Inc.", 4) %>% nline %>%
    addTitle("Mark D. Danese, MHS, PhD", 4) %>%
    addTitle("Marc Halperin", 4) %>% nline %>%
    addTitle(format(Sys.Date(), "%B %d, %Y"), 4)

  if (!html) { doc = addPageBreak(doc) }

  # Overview
  message("Generating Overview")

  doc = doc %>%
    addTitle("Overview") %>%
    addParagraph("Information was extracted from the National Cancer Institute (NCI) online reference database for \"SEER-Medicare\" publications", par.properties = level(1)) %>%
    addParagraph(set_of_paragraphs(
      pot("http://healthcaredelivery.cancer.gov/seermedicare/overview/pubsearch.html",
          hyperlink = metadata$nci_url,
          format = textProperties(underlined = T)),
      paste0(format(metadata$access_date, "Data accessed %B %d, %Y"), .underrepresented(seer_pub_data) ),
      sprintf("%i citations retrieved", nrow(publications)),
      "NCI classified all publications according to the following:"), par.properties = level(2)) %>%
    addParagraph(sprintf("%d tumor types (e.g., colorectal, leukemia, etc.)", length(all_sites)), par.properties = level(3)) %>%
    addParagraph(set_of_paragraphs(
      "Summary measures were calculated by year, tumor site, journal, and author",
      "Exploratory analyses were conducted to assess publication frequency relative to cancer incidence, prevalence, and deaths",
      sprintf("All analyses were conducted using %s", R.Version()$version.string),
      sprintf("All data were extracted from internet-based sources on %s using the rvest package (version %s)", format(metadata$access_date, "%B %d, %Y"), metadata$rvestvers),
      sprintf("Figures were created using the ggplot2 package (version %s)", utils::packageVersion("ggplot2")),
      sprintf("This report was generated using the ReporteRs package (version %s)", utils::packageVersion("ReporteRs"))), par.properties = level(1))


  # Seer-Medicare Publications Search
  message("Generating Tumor Sites List")
  doc = addTitle(doc, "SEER-Medicare Publications Search")
  doc = addTitle(doc, "Tumor Sites:", 2) %>%
    addParagraph(all_sites, par.properties = parProperties(list.style = "ordered"))

  if (!html) { doc = addPageBreak(doc) }


  # Summary of Publications
  message("Generating Summary of Publications")

  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year = metadata$prevalence_year-1
  time_period <- seq(from=last_reporting_year-6, to=last_reporting_year)

  doc = doc %>% addTitle("Summary of Publications") %>%
    myAddPlot(bar_plot("sites", publications, x_label = "tumor site")) %>% nline %>% nline %>%
    myAddPlot(bar_plot("sites", publications, years = time_period, x_label = "tumor site")) %>%
    myAddPlot(line_plot(publications)) %>% nline %>% nline %>%
    myAddPlot(line_plot(publications,"sites", by_label = "tumor site")) %>%
    myAddPlot(bar_plot("journal", publications, display = 10)) %>% nline %>% nline %>%
    myAddPlot(bar_plot("journal", publications, years = time_period, display = 10)) %>%
    myAddPlot(plot = bar_plot("authors", publications, x_label = "author", display = 10)) %>% nline %>% nline %>%
    myAddPlot(plot = bar_plot("authors", publications, x_label = "author", years = time_period, display = 10))

  if (!html) {doc = addPageBreak(doc)}


  # Exploratory Analyses: Publications Relative to Population Size
  message("Generating Exploratory Analyses: Publications Relative to Population Size")
  message("...Overview")
  doc = doc %>%
    addTitle("Exploratory Analyses: Publications Relative to Population Size") %>%
    addTitle("Methodology", 2) %>%
    addParagraph(set_of_paragraphs(
      "Publication counts were plotted against tumor types",
      "Counts of incident and prevalent patients, as well as deaths from cancer, were taken from SEER published estimates",
      "Ratios of publications to these patient counts were estimated"), par.properties = level(1))
  sapply(seer_cols(), function(x) {
    doc = addParagraph(doc, sprintf("Publications divided by %s (per %s patients)", x, format(get_scale(sites[[x]]), big.mark = ",")), par.properties = level(2))
    })

  message("...Plots")
  doc = doc %>%
    addParagraph(set_of_paragraphs("Higher numbers reflect more publications per patient",
      "The median publication ratio across all tumors was used as an index for establishing high versus low ratios"),
      par.properties = level(2)) %>%
    myAddPlot(seer_plot("incidence", seer_pub_data)) %>%
    myAddPlot(seer_plot("prevalence", seer_pub_data)) %>%
    myAddPlot(seer_plot("deaths", seer_pub_data))

  if (!html) {doc = addSection(doc, landscape = T)}

  message("...Tables")
  doc = addFlexTable(doc, seer_table(seer_pub_data, footnote = "Notes: Higher numbers indicate more publications per patient (see table for comparisons to the median ratios)"),  par.properties = parCenter())

  if (!html) {doc = addPageBreak(doc)}

  doc = addFlexTable(doc, seer_table(seer_pub_data, standardize = T, less_than = 1, footnote = "Notes: Ratios reflect the tumor-specific publication ratio relative to median publication ratio (see table for non-standardized ratios)\u000AValues above 1.0 indicate ratios higher than the median, and values below 1.0 indicate ratios below the median (shaded orange)"), par.properties = parCenter())

  if (!html) {doc = addSection(doc)}

  # Conclusions
  message("Generating Conclusions")

  doc = doc %>%
    addTitle("Conclusions") %>%
    addParagraph(set_of_paragraphs(
      "There has been steady growth in the number of publications over time using the SEER-Medicare data",
      sprintf("%s, and %s are the tumor sites with the highest number of publications", paste(c(all_sites[1], tolower(all_sites[2:3])), collapse = ", "), tolower(all_sites[4]))), par.properties = level(1)) %>%
    addParagraph("This corresponds well with estimates of incidence and prevalence in the United States population", par.properties = level(2)) %>%
    addParagraph(set_of_paragraphs(
      "The number of publications appears to align better with incidence and prevalence than with death, particularly for more common cancers",
      "Leukemia, lymphoma, lung, and renal might be under-represented in terms of the absolute number of publications"),
      par.properties = level(1)) %>% nline


  # Data Sources
  message("Generating Data Sources")

  doc = doc %>%
    addTitle("Data Sources") %>%
    addParagraph(paste0(sapply(seer_cols(), function(x){sprintf("%s (%s)", tools::toTitleCase(x), metadata[[paste0(x, "_year")]])}), collapse = ", "), par.properties = level(1)) %>%
    addParagraph(metadata$seer_citation, par.properties = level(2)) %>%
    addParagraph(sprintf("Proportion of patients age 65 or older at diagnosis (%s)", metadata$cdc_year), par.properties = level(1)) %>%
    addParagraph(metadata$seer_citation, par.properties = level(2)) %>%
    addParagraph("All Site and Morphology classifications were aligned among all data sources with a few exceptions:", par.properties = level(1)) %>%
    addParagraph(set_of_paragraphs(
      "Head and Neck was defined using SEER/CDC category: Oral Cavity and Pharynx",
      "Corpus Uteri was defined using SEER/CDC category: Endometrial Cancer"), par.properties = level(2)) %>% nline


  # Acknowledgements
  message("Generating Acknowledgements")

  doc = doc %>%
    addTitle("Acknowledgements") %>%
    addParagraph("Thanks to Ryan Duryea who helped to automate this and previous versions of this report")

  if(!html){doc = addPageBreak(doc)}

  # Contact Information
  message("Generating Contact Information")

  doc = doc %>%
    addTitle("Contact Information") %>%
    addParagraph(set_of_paragraphs("Mark D. Danese", "Outcomes Insights, Inc.", "2801 Townsgate Road, Suite #330",
                                   "Westlake Village, CA 91361", "805-498-0034", "mark@outins.com",
                                   pot("www.outins.com", hyperlink = "http://www.outins.com/", format = textProperties(underlined = T))))

  if(outputPath != "") {
    dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
    filename <- sprintf("SEER_Medicare_Report_%s.%s", metadata$access_date, ifelse(html, "html", "docx"))
    fullPath <- file.path(outputPath, filename)
    writeDoc(doc, fullPath)
    print(paste("Report written to file:", fullPath))
  }

  invisible(doc)
}
