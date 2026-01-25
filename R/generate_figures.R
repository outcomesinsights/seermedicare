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

  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year <- metadata$prevalence_year
  if (is.null(last_reporting_year) || is.na(last_reporting_year)) {
    last_reporting_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  } else {
    last_reporting_year <- as.numeric(last_reporting_year) - 1
  }
  period <- seq(from=last_reporting_year-6, to=last_reporting_year)

  # ============================================
  # STEP 1: PREPARE ALL DATA TABLES
  # ============================================
  message("Preparing data tables")
  
  # Helper function to prepare count data
  prepare_counts <- function(x, pubs, display = NULL) {
    dt <- get_data(x, pubs)
    counts <- dt[, .(count = .N), by = .(values)]
    # Preserve factor ordering from get_data
    counts[, values := factor(values, levels = levels(dt$values))]
    if (!is.null(display)) {
      counts <- counts[values %in% levels(values)[1:display]]
    }
    year_range <- range(dt$year)
    attr(counts, "year_min") <- year_range[1]
    attr(counts, "year_max") <- year_range[2]
    counts
  }
  
  # Tumor site counts
  site_counts_all <- prepare_counts("sites", publications)
  site_counts_recent <- prepare_counts("sites", publications[year %in% period])
  
  # Year counts
  year_counts <- prepare_counts("year", publications)
  year_counts[, year_num := as.numeric(as.character(values))]
  setorder(year_counts, year_num)
  
  # Year by top tumor site counts
  site_data <- get_data("sites", publications)
  top_sites <- levels(site_data$values)[1:4]
  year_by_site_counts <- site_data[values %in% top_sites, .(count = .N), by = .(year, values)]
  
  # Author counts  
  author_counts_all <- prepare_counts("authors", publications, display = 10)
  author_counts_recent <- prepare_counts("authors", publications[year %in% period], display = 10)
  
  # Journal counts
  journal_counts_all <- prepare_counts("journal", publications, display = 10)
  journal_counts_recent <- prepare_counts("journal", publications[year %in% period], display = 10)
  
  # SEER data (incidence, prevalence, deaths)
  seer_scaled <- scale_seer(sites)

  # ============================================
  # STEP 2: SET UP OUTPUT PATHS
  # ============================================
  tmpdir <- tempdir()
  curr_wd <- getwd()
  setwd(tmpdir)
  figure_path <- file.path("Figures")
  dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)
  
  excelPath <- file.path(dirname(outputPath), "excel")
  dir.create(excelPath, showWarnings = FALSE, recursive = TRUE)
  
  # ============================================
  # STEP 3: GENERATE FIGURES FROM DATA
  # ============================================
  message("Generating figures")
  
  # Helper to create bar plot from pre-computed counts
  bar_plot_from_counts <- function(counts, x_label, display = NULL) {
    year_min <- attr(counts, "year_min")
    year_max <- attr(counts, "year_max")
    
    # Truncate long labels
    truncate_label <- function(x, max_len = 60) {
      ifelse(nchar(x) > max_len, paste0(substr(x, 1, max_len - 3), "..."), x)
    }
    counts <- copy(counts)
    counts[, label := truncate_label(as.character(values))]
    orig_order <- unique(truncate_label(levels(counts$values)))
    counts[, label := factor(label, levels = orig_order[orig_order %in% label])]
    
    bp <- ggplot(counts, aes(x = label, y = count)) + 
      geom_bar(stat = "identity", fill = seer_palette()[1])
    title <- sprintf("Publications by %s: %s-%s%s", x_label, year_min, year_max, 
                     ifelse(is.null(display), "", sprintf(" (top %d)", display)))
    .format_plot(bp, x_label, title = title)
  }
  
  # Helper to save figure
  save_figure <- function(plot, filename) {
    png(filename = file.path(figure_path, filename), width = 1125, height = 900)
    print(plot)
    dev.off()
  }
  
  # Tumor site figures
  save_figure(bar_plot_from_counts(site_counts_all, "tumor site"), 
              sprintf("publications_by_tumor_site_%s-%s.png", attr(site_counts_all, "year_min"), attr(site_counts_all, "year_max")))
  save_figure(bar_plot_from_counts(site_counts_recent, "tumor site"),
              sprintf("publications_by_tumor_site_%s-%s.png", attr(site_counts_recent, "year_min"), attr(site_counts_recent, "year_max")))
  
  # Year figure
  lp_year <- ggplot(year_counts, aes(x = factor(year_num), y = count, group = 1)) + 
    geom_line(color = seer_palette()[1], linewidth = 1)
  lp_year <- .format_plot(lp_year, x_label = "year", title = "Publications by year")
  save_figure(lp_year, "publications_by_year.png")
  
  # Year by tumor site figure
  lp_site <- ggplot(year_by_site_counts, aes(x = factor(year), y = count, group = values, color = values, linetype = values)) + 
    geom_line(linewidth = 1) +
    scale_color_manual(values = seer_palette()) + 
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"))
  lp_site <- .format_plot(lp_site, x_label = "year", title = "Publications by year and top 4 tumor sites")
  save_figure(lp_site, "publications_by_year_and_tumor_site.png")
  
  # Author figures
  save_figure(bar_plot_from_counts(author_counts_all, "author", display = 10),
              sprintf("publications_by_author_%s-%s.png", attr(author_counts_all, "year_min"), attr(author_counts_all, "year_max")))
  save_figure(bar_plot_from_counts(author_counts_recent, "author", display = 10),
              sprintf("publications_by_author_%s-%s.png", attr(author_counts_recent, "year_min"), attr(author_counts_recent, "year_max")))
  
  # Journal figures
  save_figure(bar_plot_from_counts(journal_counts_all, "journal", display = 10),
              sprintf("publications_by_journal_%s-%s.png", attr(journal_counts_all, "year_min"), attr(journal_counts_all, "year_max")))
  save_figure(bar_plot_from_counts(journal_counts_recent, "journal", display = 10),
              sprintf("publications_by_journal_%s-%s.png", attr(journal_counts_recent, "year_min"), attr(journal_counts_recent, "year_max")))
  
  # SEER figures (incidence, prevalence, deaths)
  for (x in seer_cols()) {
    save_figure(seer_plot(x, seer_pub_data), sprintf("publications_and_%s.png", x))
  }

  # ============================================
  # STEP 4: GENERATE EXCEL FROM SAME DATA
  # ============================================
  message("Generating Excel file")
  
  wb <- openxlsx::createWorkbook()
  
  # Helper to format counts for Excel
  format_excel_counts <- function(counts_all, counts_recent, col_name) {
    dt_all <- copy(counts_all)[, .(x = values, count)]
    dt_recent <- copy(counts_recent)[, .(x = values, count)]
    setnames(dt_all, "count", sprintf("Publications: %s-%s", attr(counts_all, "year_min"), attr(counts_all, "year_max")))
    setnames(dt_recent, "count", sprintf("Publications: %s-%s", attr(counts_recent, "year_min"), attr(counts_recent, "year_max")))
    dt <- merge(dt_all, dt_recent, by = "x", all = TRUE)
    setnames(dt, "x", col_name)
    dt
  }
  
  # Tumor Site sheet
  site_excel <- format_excel_counts(site_counts_all, site_counts_recent, "Tumor Site")
  openxlsx::addWorksheet(wb, "Tumor Site")
  openxlsx::writeData(wb, "Tumor Site", site_excel)
  
  # Year sheet
  year_excel <- data.table(Year = year_counts$year_num, Publications = year_counts$count)
  setorder(year_excel, -Year)
  openxlsx::addWorksheet(wb, "Year")
  openxlsx::writeData(wb, "Year", year_excel)
  
  # Year and Top Tumor Site sheet
  year_site_excel <- dcast(year_by_site_counts, year ~ values, value.var = "count")
  setnames(year_site_excel, c("Year", paste("Publications:", names(year_site_excel)[-1])))
  openxlsx::addWorksheet(wb, "Year and Top Tumor Site")
  openxlsx::writeData(wb, "Year and Top Tumor Site", year_site_excel)
  
  # Author sheet
  author_excel <- format_excel_counts(author_counts_all, author_counts_recent, "Author")
  openxlsx::addWorksheet(wb, "Author")
  openxlsx::writeData(wb, "Author", author_excel)
  
  # Journal sheet
  journal_excel <- format_excel_counts(journal_counts_all, journal_counts_recent, "Journal")
  openxlsx::addWorksheet(wb, "Journal")
  openxlsx::writeData(wb, "Journal", journal_excel)
  
  # Incidence Prevalence Deaths sheet
  seer_excel <- copy(seer_scaled)[, -c("allages", "min65")]
  setnames(seer_excel, c("Tumor Site", "Publications", sapply(seer_cols(), .seer_label, sites, metadata)))
  openxlsx::addWorksheet(wb, "Incidence Prevalence Deaths")
  openxlsx::writeData(wb, "Incidence Prevalence Deaths", seer_excel)
  
  # Save Excel file
  doc <- file.path(excelPath, "SEER_Medicare_Data_Tables.xlsx")
  message("Writing Excel file to ", doc)
  openxlsx::saveWorkbook(wb, doc, overwrite = TRUE)

  # ============================================
  # STEP 5: PACKAGE AND COPY FIGURES
  # ============================================
  message("Writing Figures zip file to ", figure_path)
  files <- dir(figure_path, full.names = TRUE)
  zip(zipfile = file.path(outputPath, "SEER_Medicare_Figures"), files = files)
  
  message("Copying individual figure files to ", outputPath)
  file.copy(files, outputPath, overwrite = TRUE)
  
  setwd(curr_wd)
}
