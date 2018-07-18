#' Generate Full Report: Overview of All SEER-Medicare Publications
#'
#' @param seer_pub_data Result of running scrape_all_data.  Should have class "scrape_all_data"
#' @param outputPath Path to directory where report will be generated. Default is working directory
#' @param html Logical value - if TRUE, will generate html file; if FALSE, will generate Microsoft Word file
#'
#' @return doc Object of class \code{\link{docx}} or \code{\link{bsdoc}} containing SEER-Medicare report
#'
#' @import XLConnect
#' @export
generate_figures <- function(seer_pub_data, outputPath = "."){

  publications <- seer_pub_data$publications
  metadata <- seer_pub_data$metadata
  sites <- seer_pub_data$site_summary

  # SAVE FIGURES TO DIRECTORY
  figure_path <- file.path(outputPath,"Figures")
  dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)

  # XLSX File Path
  doc <- file.path(outputPath, sprintf("SEER_Medicare_Data_Tables_%s.xlsx",metadata$access_date))

  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year = metadata$prevalence_year-1
  period <- seq(from=last_reporting_year-6, to=last_reporting_year)

  count_pubs <- function(dt) {
    dt <- dt[, count(values)] %>% setnames("freq", sprintf("Publications: %s-%s", min(dt$year), max(dt$year)))
    dt
  }

  write <- function(x, label = x, ...) {

    dt1 <- get_data(x, publications)
    dt2 <- dt1[year %in% period]
    dt <- merge(count_pubs(dt1), count_pubs(dt2), by = "x", all = T) %>% setnames("x", tools::toTitleCase(label))

    png_bar <- function(dt){
      png(filename = paste0(figure_path, sprintf("/publications_by_%s_%s-%s.png", gsub("\\s", "_", label), min(dt$year), max(dt$year))), width = 650, height = 400)
      print(bar_plot(x = x, publications, x_label = label, years = c(min(dt$year):max(dt$year)), ...))
      dev.off()
    }

    png_bar(dt1)
    png_bar(dt2)

    writeWorksheetToFile(doc, data = dt, sheet = tools::toTitleCase(label))
  }

  message("Generate Figures and Tables")
  write("sites", "tumor site")
  writeWorksheetToFile(doc, data = setnames(get_data("year", publications)[, count(values)], c("Year", "Publications")), sheet = "Year")

  # Output Publication By Year Chart
  png(filename = paste0(figure_path,"/publications_by_year.png"), width = 650, height = 400)
  print(line_plot(publications))
  dev.off()

  dt <- get_data("sites", publications)[, count(values), by = "year"] %>% .[x %in% levels(x)[1:4]] %>% dcast(year~x, value.var = "freq") %>% setnames(c("Year", paste("Publications:", colnames(.)[-1])))

  writeWorksheetToFile(doc, data = dt, sheet = "Year and Top Tumor Site")

  png(filename = "Figures/publications_by_year_and_tumor_site.png", width = 650, height = 400)
  print(line_plot(publications, "sites", "tumor site"))
  dev.off()

  write("authors", "author", display = 10)
  write("journal", display = 10)


  dt <- scale_seer(sites)[, -c("allages", "min65")] %>%
    setnames(c("Tumor Site", "Publications", sapply(seer_cols(), .seer_label, sites, metadata)))

  writeWorksheetToFile(doc, data = dt, sheet = "Incidence Prevalence Deaths")

  write_seer <- function(x){
    png(filename = file.path(figure_path, sprintf("publications_and_%s.png", x)), width = 650, height = 400)
    print(seer_plot(x, seer_pub_data))
    dev.off()
  }

  lapply(seer_cols(), write_seer)

  message("Writing Figures zip file to ", figure_path)
  files <- dir(figure_path, full.names = TRUE)
  zip(zipfile = file.path(outputPath, sprintf("SEER_Medicare_Figures_%s", metadata$access_date)), files = files)

}
