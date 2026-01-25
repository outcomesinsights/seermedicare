#' Generate Figures and Excel Data Tables
#'
#' @param seer_pub_data Result of running scrape_all_data. Should have class "seermedicare"
#' @param outputPath Path to directory where figures will be generated. Default is working directory
#' @param report_data Optional pre-computed data from prepare_report_data(). If NULL, will compute internally.
#'
#' @return Invisibly returns NULL. Side effect: writes PNG figures and Excel file to outputPath
#'
#' @import openxlsx
#' @importFrom grDevices dev.off png
#' @importFrom utils zip
#' @export
generate_figures <- function(seer_pub_data, outputPath = ".", report_data = NULL){

  # Use pre-computed data if provided, otherwise compute it
  if (is.null(report_data)) {
    report_data <- prepare_report_data(seer_pub_data)
  }
  
  # Extract commonly used items
  metadata <- report_data$metadata
  sites <- report_data$sites
  
  outputPath <- normalizePath(outputPath)

  # ============================================
  # SET UP OUTPUT PATHS
  # ============================================
  tmpdir <- tempdir()
  curr_wd <- getwd()
  setwd(tmpdir)
  figure_path <- file.path("Figures")
  dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)
  
  excelPath <- file.path(dirname(outputPath), "excel")
  dir.create(excelPath, showWarnings = FALSE, recursive = TRUE)
  
  # ============================================
  # HELPER FUNCTIONS
  # ============================================
  
  # Truncate long labels for display
  truncate_label <- function(x, max_len = 60) {
    ifelse(nchar(x) > max_len, paste0(substr(x, 1, max_len - 3), "..."), x)
  }
  
  # Create bar plot from counts data
  create_bar_plot <- function(data_item, x_label, display = NULL) {
    counts <- copy(data_item$counts)
    year_min <- data_item$year_min
    year_max <- data_item$year_max
    
    counts[, label := truncate_label(as.character(values))]
    orig_order <- unique(truncate_label(levels(counts$values)))
    counts[, label := factor(label, levels = orig_order[orig_order %in% label])]
    
    bp <- ggplot(counts, aes(x = label, y = count)) + 
      geom_bar(stat = "identity", fill = seer_palette()[1])
    title <- sprintf("Publications by %s: %s-%s%s", x_label, year_min, year_max, 
                     ifelse(is.null(display), "", sprintf(" (top %d)", display)))
    .format_plot(bp, x_label, title = title)
  }
  
  # Save figure to file
  save_figure <- function(plot, filename) {
    png(filename = file.path(figure_path, filename), width = 1125, height = 900)
    print(plot)
    dev.off()
  }

  # ============================================
  # GENERATE FIGURES FROM report_data
  # ============================================
  message("Generating figures")
  
  # Tumor site figures
  save_figure(
    create_bar_plot(report_data$site_all, "tumor site"),
    sprintf("publications_by_tumor_site_%s-%s.png", report_data$site_all$year_min, report_data$site_all$year_max)
  )
  save_figure(
    create_bar_plot(report_data$site_recent, "tumor site"),
    sprintf("publications_by_tumor_site_%s-%s.png", report_data$site_recent$year_min, report_data$site_recent$year_max)
  )
  
  # Year figure
  year_counts <- report_data$year$counts
  lp_year <- ggplot(year_counts, aes(x = factor(year_num), y = count, group = 1)) + 
    geom_line(color = seer_palette()[1], linewidth = 1)
  lp_year <- .format_plot(lp_year, x_label = "year", title = "Publications by year")
  save_figure(lp_year, "publications_by_year.png")
  
  # Year by tumor site figure
  year_by_site <- report_data$year_by_site$counts
  lp_site <- ggplot(year_by_site, aes(x = factor(year), y = count, group = values, color = values, linetype = values)) + 
    geom_line(linewidth = 1) +
    scale_color_manual(values = seer_palette()) + 
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"))
  lp_site <- .format_plot(lp_site, x_label = "year", title = "Publications by year and top 4 tumor sites")
  save_figure(lp_site, "publications_by_year_and_tumor_site.png")
  
  # Author figures
  save_figure(
    create_bar_plot(report_data$author_all, "author", display = 10),
    sprintf("publications_by_author_%s-%s.png", report_data$author_all$year_min, report_data$author_all$year_max)
  )
  save_figure(
    create_bar_plot(report_data$author_recent, "author", display = 10),
    sprintf("publications_by_author_%s-%s.png", report_data$author_recent$year_min, report_data$author_recent$year_max)
  )
  
  # Journal figures
  save_figure(
    create_bar_plot(report_data$journal_all, "journal", display = 10),
    sprintf("publications_by_journal_%s-%s.png", report_data$journal_all$year_min, report_data$journal_all$year_max)
  )
  save_figure(
    create_bar_plot(report_data$journal_recent, "journal", display = 10),
    sprintf("publications_by_journal_%s-%s.png", report_data$journal_recent$year_min, report_data$journal_recent$year_max)
  )
  
  # SEER figures
  for (x in seer_cols()) {
    save_figure(seer_plot(x, seer_pub_data), sprintf("publications_and_%s.png", x))
  }

  # ============================================
  # GENERATE EXCEL FROM SAME report_data
  # ============================================
  message("Generating Excel file")
  
  wb <- openxlsx::createWorkbook()
  
  # Helper to format paired counts (all years + recent) for Excel
  format_paired_counts <- function(data_all, data_recent, col_name) {
    dt_all <- copy(data_all$counts)[, .(x = values, count)]
    dt_recent <- copy(data_recent$counts)[, .(x = values, count)]
    setnames(dt_all, "count", sprintf("Publications: %s-%s", data_all$year_min, data_all$year_max))
    setnames(dt_recent, "count", sprintf("Publications: %s-%s", data_recent$year_min, data_recent$year_max))
    dt <- merge(dt_all, dt_recent, by = "x", all = TRUE)
    setnames(dt, "x", col_name)
    dt
  }
  
  # Tumor Site sheet
  site_excel <- format_paired_counts(report_data$site_all, report_data$site_recent, "Tumor Site")
  openxlsx::addWorksheet(wb, "Tumor Site")
  openxlsx::writeData(wb, "Tumor Site", site_excel)
  
  # Year sheet
  year_excel <- data.table(
    Year = report_data$year$counts$year_num, 
    Publications = report_data$year$counts$count
  )
  setorder(year_excel, -Year)
  openxlsx::addWorksheet(wb, "Year")
  openxlsx::writeData(wb, "Year", year_excel)
  
  # Year and Top Tumor Site sheet
  year_site_excel <- dcast(report_data$year_by_site$counts, year ~ values, value.var = "count")
  setnames(year_site_excel, c("Year", paste("Publications:", names(year_site_excel)[-1])))
  openxlsx::addWorksheet(wb, "Year and Top Tumor Site")
  openxlsx::writeData(wb, "Year and Top Tumor Site", year_site_excel)
  
  # Author sheet
  author_excel <- format_paired_counts(report_data$author_all, report_data$author_recent, "Author")
  openxlsx::addWorksheet(wb, "Author")
  openxlsx::writeData(wb, "Author", author_excel)
  
  # Journal sheet
  journal_excel <- format_paired_counts(report_data$journal_all, report_data$journal_recent, "Journal")
  openxlsx::addWorksheet(wb, "Journal")
  openxlsx::writeData(wb, "Journal", journal_excel)
  
  # Incidence Prevalence Deaths sheet
  seer_excel <- copy(report_data$seer)[, -c("allages", "min65")]
  setnames(seer_excel, c("Tumor Site", "Publications", sapply(seer_cols(), .seer_label, sites, metadata)))
  openxlsx::addWorksheet(wb, "Incidence Prevalence Deaths")
  openxlsx::writeData(wb, "Incidence Prevalence Deaths", seer_excel)
  
  # Save Excel file
  doc <- file.path(excelPath, "SEER_Medicare_Data_Tables.xlsx")
  message("Writing Excel file to ", doc)
  openxlsx::saveWorkbook(wb, doc, overwrite = TRUE)

  # ============================================
  # PACKAGE AND COPY FIGURES
  # ============================================
  message("Writing Figures zip file")
  files <- dir(figure_path, full.names = TRUE)
  zip(zipfile = file.path(outputPath, "SEER_Medicare_Figures"), files = files)
  
  message("Copying individual figure files to ", outputPath)
  file.copy(files, outputPath, overwrite = TRUE)
  
  setwd(curr_wd)
}
