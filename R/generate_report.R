#' Generate Full Report: Overview of All SEER-Medicare Publications
#'
#' @param seer_pub_data Result of running scrape_all_data.  Should have class "seermedicare"
#' @param outputPath Path to directory where report will be generated. Default is working directory
#' @param html Logical value - deprecated, retained for compatibility
#' @param pdf Logical value - if TRUE, will generate a PDF file
#'
#' @return Invisibly returns an officer rdocx object containing the SEER-Medicare report
#' @import magrittr officer rmarkdown flextable
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' generate_report(seer_pub_data, outputPath = "output")
#' generate_report(seer_pub_data, outputPath = "output", pdf = TRUE)
#' }
generate_report <- function(seer_pub_data, outputPath = ".", html = FALSE, pdf = FALSE){

  if(!inherits(seer_pub_data, "seermedicare")) {
    stop("seer_pub_data argument must be of class seermedicare")
  }

  metadata <- seer_pub_data$metadata
  publications <- seer_pub_data$publications
  sites <- seer_pub_data$site_summary

  if (html) {
    warning("html output is deprecated; generating Word/PDF output instead.")
  }

  doc <- officer::read_docx(path = system.file("extdata", "ReportTemplate.docx", package = "seermedicare", mustWork = TRUE))

  all_sites <- get_data("sites", publications)$values %>% levels

  message("Generating Title")

  doc <- doc_add_title(doc, "Overview of All SEER-Medicare Publications", 3)
  doc <- doc_add_title(doc, "Outcomes Insights, Inc.", 4)
  doc <- nline(doc)
  doc <- doc_add_title(doc, "Mark D. Danese, MHS, PhD", 4)
 
  doc <- nline(doc)
  doc <- doc_add_title(doc, format(Sys.Date(), "%B %d, %Y"), 4)
  doc <- officer::body_add_break(doc)

  # Overview
  message("Generating Overview")

  doc <- doc_add_title(doc, "Overview")
  doc <- doc_add_list(doc, "Information was extracted from the National Cancer Institute (NCI) online reference database for \"SEER-Medicare\" publications", level = 1)
  doc <- doc_add_hyperlink(doc, "https://healthcaredelivery.cancer.gov/publications/", metadata$nci_url, style = list_style(2))
  doc <- doc_add_list(doc, c(
    paste0(format(metadata$access_date, "Data accessed %B %d, %Y"), .underrepresented(seer_pub_data)),
    sprintf("%i citations retrieved", nrow(publications)),
    "NCI classified all publications according to the following:"
  ), level = 2)
  doc <- doc_add_list(doc, sprintf("%d tumor types (e.g., colorectal, leukemia, etc.)", length(all_sites)), level = 3)
  doc <- doc_add_list(doc, c(
    "Summary measures were calculated by year, tumor site, journal, and author",
    "Exploratory analyses were conducted to assess publication frequency relative to cancer incidence, prevalence, and deaths",
    sprintf("All analyses were conducted using %s", R.Version()$version.string),
    sprintf("All data were extracted from internet-based sources on %s using the rvest package (version %s)", format(metadata$access_date, "%B %d, %Y"), metadata$rvestvers),
    sprintf("Figures were created using the ggplot2 package (version %s)", utils::packageVersion("ggplot2")),
    sprintf("This report was generated using the officer package (version %s)", utils::packageVersion("officer"))
  ), level = 1)


  # Seer-Medicare Publications Search
  message("Generating Tumor Sites List")
  doc <- doc_add_title(doc, "SEER-Medicare Publications Search")
  doc <- doc_add_title(doc, "Tumor Sites:", 2)
  doc <- doc_add_list(doc, all_sites, ordered = TRUE)
  doc <- officer::body_add_break(doc)


  # Summary of Publications
  message("Generating Summary of Publications")

  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year <- metadata$prevalence_year
  if (is.null(last_reporting_year) || is.na(last_reporting_year)) {
    # Fallback to current year - 1 if prevalence_year is missing
    last_reporting_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  } else {
    last_reporting_year <- as.numeric(last_reporting_year) - 1
  }
  time_period <- seq(from=last_reporting_year-6, to=last_reporting_year)

  doc <- doc_add_title(doc, "Summary of Publications")
  doc <- myAddPlot(doc, bar_plot("sites", publications, x_label = "tumor site"))
  doc <- nline(doc)
  doc <- nline(doc)
  doc <- myAddPlot(doc, bar_plot("sites", publications, years = time_period, x_label = "tumor site"))
  doc <- myAddPlot(doc, line_plot(publications))
  doc <- nline(doc)
  doc <- nline(doc)
  doc <- myAddPlot(doc, line_plot(publications, "sites", by_label = "tumor site"))
  doc <- myAddPlot(doc, bar_plot("journal", publications, display = 10))
  doc <- nline(doc)
  doc <- nline(doc)
  doc <- myAddPlot(doc, bar_plot("journal", publications, years = time_period, display = 10))
  doc <- myAddPlot(doc, bar_plot("authors", publications, x_label = "author", display = 10))
  doc <- nline(doc)
  doc <- nline(doc)
  doc <- myAddPlot(doc, bar_plot("authors", publications, x_label = "author", years = time_period, display = 10))

  doc <- officer::body_add_break(doc)


  # Exploratory Analyses: Publications Relative to Population Size
  message("Generating Exploratory Analyses: Publications Relative to Population Size")
  message("...Overview")
  doc <- doc_add_title(doc, "Exploratory Analyses: Publications Relative to Population Size")
  doc <- doc_add_title(doc, "Methodology", 2)
  doc <- doc_add_list(doc, c(
    "Publication counts were plotted against tumor types",
    "Counts of incident and prevalent patients, as well as deaths from cancer, were taken from SEER published estimates",
    "Ratios of publications to these patient counts were estimated"
  ), level = 1)

  for (x in seer_cols()) {
    doc <- doc_add_list(doc, sprintf("Publications divided by %s (per %s patients)", x, format(get_scale(sites[[x]]), big.mark = ",")), level = 2)
  }

  message("...Plots")
  doc <- doc_add_list(doc, c(
    "Higher numbers reflect more publications per patient",
    "The median publication ratio across all tumors was used as an index for establishing high versus low ratios"
  ), level = 2)
  doc <- myAddPlot(doc, seer_plot("incidence", seer_pub_data))
  doc <- myAddPlot(doc, seer_plot("prevalence", seer_pub_data))
  doc <- myAddPlot(doc, seer_plot("deaths", seer_pub_data))

  doc <- officer::body_end_section_portrait(doc)

  message("...Tables")
  doc <- flextable::body_add_flextable(doc, value = seer_table(seer_pub_data, footnote = "Notes: Higher numbers indicate more publications per patient (see next table for comparisons to the median ratios)"))
  doc <- officer::body_add_break(doc)
  doc <- flextable::body_add_flextable(doc, value = seer_table(seer_pub_data, standardize = T, less_than = 1, footnote = "Notes: Ratios reflect the tumor-specific publication ratio relative to median publication ratio (see previous table for non-standardized ratios)\u000AValues above 1.0 indicate ratios higher than the median, and values below 1.0 indicate ratios below the median (shaded orange)"))
  doc <- officer::body_end_section_landscape(doc)

  # Conclusions
  message("Generating Conclusions")

  doc <- doc_add_title(doc, "Conclusions")
  doc <- doc_add_list(doc, c(
    "There was steady growth followed by a decline in the number of publications over time using the SEER-Medicare data",
    sprintf("%s, and %s are the tumor sites with the highest number of publications", paste(c(all_sites[1], tolower(all_sites[2:3])), collapse = ", "), tolower(all_sites[4]))
  ), level = 1)
  doc <- doc_add_list(doc, c(
    "The number of publications appears to align better with incidence and prevalence than with death, particularly for more common cancers",
    "Leukemia, corpus uteri, multiple myeloma, and skin might be under-represented in terms of the absolute number of publications"
  ), level = 1)
  doc <- nline(doc)


  # Data Sources
  message("Generating Data Sources")

  doc <- doc_add_title(doc, "Data Sources")
  doc <- doc_add_list(doc, paste0(sapply(seer_cols(), function(x){sprintf("%s (%s)", tools::toTitleCase(x), metadata[[paste0(x, "_year")]])}), collapse = ", "), level = 1)
  doc <- doc_add_list(doc, metadata$seer_citation, level = 2)
  doc <- doc_add_list(doc, sprintf("Proportion of patients age 65 or older at diagnosis (%s)", metadata$cdc_year), level = 1)
  doc <- doc_add_list(doc, metadata$seer_citation, level = 2)
  doc <- doc_add_list(doc, "All Site and Morphology classifications were aligned among all data sources with a few exceptions:", level = 1)
  doc <- doc_add_list(doc, c(
    "Head and Neck was defined using SEER/CDC category: Oral Cavity and Pharynx",
    "Corpus Uteri was defined using SEER/CDC category: Endometrial Cancer"
  ), level = 2)
  doc <- nline(doc)


  # Acknowledgements
  message("Generating Acknowledgements")

  doc <- doc_add_title(doc, "Acknowledgements")
  doc <- doc_add_paragraphs(doc, "Thanks to Ryan Duryea who helped to automate this and previous versions of this report")
  doc <- officer::body_add_break(doc)

  # Contact Information
  message("Generating Contact Information")

  doc <- doc_add_title(doc, "Contact Information")
  doc <- doc_add_paragraphs(doc, c(
    "Mark D. Danese",
    "Outcomes Insights, Inc.",
    "4500 Park Granada Suite 202",
    "Calabasas, CA 91302",
    "805-498-0034",
    "mark@outins.com"
  ))
  doc <- doc_add_hyperlink(doc, "www.outins.com", "http://www.outins.com/")

  if(outputPath != "") {
    dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
    # Use absolute path to avoid working directory issues
    outputPath <- normalizePath(outputPath, mustWork = TRUE)
    filename <- sprintf("SEER_Medicare_Report_%s.docx", metadata$access_date)
    fullPath <- file.path(outputPath, filename)
    print(doc, target = fullPath)
    print(paste("Report written to file:", fullPath))

    if (isTRUE(pdf)) {
      if (rmarkdown::pandoc_available()) {
        pdf_path <- sub("\\.docx$", ".pdf", fullPath)
        rmarkdown::pandoc_convert(fullPath, to = "pdf", output = pdf_path)
        print(paste("PDF written to file:", pdf_path))
      } else {
        warning("pandoc not available; skipping PDF generation.")
      }
    }
  }

  invisible(doc)
}
