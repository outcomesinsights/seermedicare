#' Generate Figures and Excel Data Tables
#'
#' @param seer_pub_data Result of running scrape_all_data. Should have class "seermedicare"
#' @param outputPath Path to directory where figures will be generated. Default is working directory
#'
#' @return Invisibly returns NULL. Side effect: writes PNG figures and Excel file to outputPath
#'
#' @import openxlsx
#' @importFrom grDevices dev.off png
#' @importFrom utils zip
#' @export
generate_figures <- function(seer_pub_data, outputPath = "."){

  publications <- seer_pub_data$publications
  metadata <- seer_pub_data$metadata
  sites <- seer_pub_data$site_summary
  
  outputPath <- normalizePath(outputPath)

  # SAVE FIGURES TO DIRECTORY
  tmpdir <- tempdir()
  curr_wd <- getwd()
  setwd(tmpdir)
  figure_path <- file.path("Figures")
  dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)
  

  # XLSX File Path
  doc <- file.path(outputPath, "SEER_Medicare_Data_Tables.xlsx")
  wb <- openxlsx::createWorkbook()
  sheet_names <- character()
  add_sheet <- function(name, data) {
    if (name %in% sheet_names) {
      name <- paste0(name, "_", length(sheet_names) + 1)
    }
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, data)
    sheet_names <<- c(sheet_names, name)
  }

  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year <- metadata$prevalence_year
  if (is.null(last_reporting_year) || is.na(last_reporting_year)) {
    # Fallback to current year - 1 if prevalence_year is missing
    last_reporting_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  } else {
    last_reporting_year <- as.numeric(last_reporting_year) - 1
  }
  period <- seq(from=last_reporting_year-6, to=last_reporting_year)

  count_pubs <- function(dt) {
    # Count values (replaces plyr::count)
    dt <- dt[, .(freq = .N), by = .(x = values)] %>% setnames("freq", sprintf("Publications: %s-%s", min(dt$year), max(dt$year)))
    dt
  }

  write <- function(x, label = x, ...) {

    dt1 <- get_data(x, publications)
    dt2 <- dt1[year %in% period]
    dt <- merge(count_pubs(dt1), count_pubs(dt2), by = "x", all = T) %>% setnames("x", tools::toTitleCase(label))

    png_bar <- function(dt){
      png(filename = paste0(figure_path, sprintf("/publications_by_%s_%s-%s.png", gsub("\\s", "_", label), min(dt$year), max(dt$year))), width = 1125, height = 900)
      print(bar_plot(x = x, publications, x_label = label, years = c(min(dt$year):max(dt$year)), ...))
      dev.off()
    }

    png_bar(dt1)
    png_bar(dt2)

    sheet <- tools::toTitleCase(label)
    add_sheet(sheet, dt)
  }

  message("Generate Figures and Tables")
  write("sites", "tumor site")
  
  year_data <- setnames(get_data("year", publications)[, .(freq = .N), by = .(x = values)], c("Year", "Publications"))
  
  setDT(year_data)
  
  year_data[, Year := as.numeric(levels(Year))]
  
  add_sheet("Year", year_data[order(-Year)])

  # Output Publication By Year Chart
  png(filename = paste0(figure_path,"/publications_by_year.png"), width = 1125, height = 900)
  print(line_plot(publications))
  dev.off()

  dt <- get_data("sites", publications)[, .(freq = .N), by = .(year, x = values)] %>% .[x %in% levels(x)[1:4]] %>% dcast(year~x, value.var = "freq") %>% setnames(c("Year", paste("Publications:", colnames(.)[-1])))

  add_sheet("Year and Top Tumor Site", dt)

  png(filename = file.path(figure_path, "publications_by_year_and_tumor_site.png"), width = 1125, height = 900)
  print(line_plot(publications, "sites", "tumor site"))
  dev.off()

  write("authors", "author", display = 10)
  write("journal", display = 10)


  dt <- scale_seer(sites)[, -c("allages", "min65")] %>%
    setnames(c("Tumor Site", "Publications", sapply(seer_cols(), .seer_label, sites, metadata)))

  add_sheet("Incidence Prevalence Deaths", dt)

  write_seer <- function(x){
    png(filename = file.path(figure_path, sprintf("publications_and_%s.png", x)), width = 1125, height = 900)
    print(seer_plot(x, seer_pub_data))
    dev.off()
  }

  lapply(seer_cols(), write_seer)

  if (length(sheet_names)) {
    message("Writing Excel file to ", doc)
    openxlsx::saveWorkbook(wb, doc, overwrite = TRUE)
  } else {
    message("No worksheets created for Excel output.")
  }

  message("Writing Figures zip file to ", figure_path)
  files <- dir(figure_path, full.names = TRUE)
  zip(zipfile = file.path(outputPath, "SEER_Medicare_Figures"), files = files)
  
  # Also copy individual PNG files to output folder
  message("Copying individual figure files to ", outputPath)
  file.copy(files, outputPath, overwrite = TRUE)
  
  setwd(curr_wd)
}
