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
  # STEP 1: PREPARE ALL DATA IN A SINGLE LIST
  # ============================================
  message("Preparing data tables")
  
  # Master data list - this is the SINGLE source of truth for both figures and Excel
  data_tables <- list()
  
  # Helper function to prepare count data from publications
  prepare_counts <- function(x, pubs) {
    dt <- get_data(x, pubs)
    counts <- dt[, .(count = .N), by = .(values)]
    # Preserve factor ordering from get_data (ordered by frequency)
    counts[, values := factor(values, levels = levels(dt$values))]
    year_range <- range(dt$year)
    list(
      counts = counts,
      year_min = year_range[1],
      year_max = year_range[2]
    )
  }
  
  # --- Tumor Site Data ---
  data_tables$site_all <- prepare_counts("sites", publications)
  data_tables$site_recent <- prepare_counts("sites", publications[year %in% period])
  
  # --- Year Data ---
  year_raw <- prepare_counts("year", publications)
  year_counts <- copy(year_raw$counts)
  year_counts[, year_num := as.numeric(as.character(values))]
  setorder(year_counts, year_num)
  data_tables$year <- list(
    counts = year_counts,
    year_min = year_raw$year_min,
    year_max = year_raw$year_max
  )
  
  # --- Year by Top Tumor Site Data ---
  site_data <- get_data("sites", publications)
  top_sites <- levels(site_data$values)[1:4]
  year_by_site <- site_data[values %in% top_sites, .(count = .N), by = .(year, values)]
  data_tables$year_by_site <- list(
    counts = year_by_site,
    top_sites = top_sites
  )
  
  # --- Author Data ---
  author_all_raw <- prepare_counts("authors", publications)
  author_all_raw$counts <- author_all_raw$counts[values %in% levels(values)[1:10]]
  data_tables$author_all <- author_all_raw
  
  author_recent_raw <- prepare_counts("authors", publications[year %in% period])
  author_recent_raw$counts <- author_recent_raw$counts[values %in% levels(values)[1:10]]
  data_tables$author_recent <- author_recent_raw
  
  # --- Journal Data ---
  journal_all_raw <- prepare_counts("journal", publications)
  journal_all_raw$counts <- journal_all_raw$counts[values %in% levels(values)[1:10]]
  data_tables$journal_all <- journal_all_raw
  
  journal_recent_raw <- prepare_counts("journal", publications[year %in% period])
  journal_recent_raw$counts <- journal_recent_raw$counts[values %in% levels(values)[1:10]]
  data_tables$journal_recent <- journal_recent_raw
  
  # --- SEER Data ---
  data_tables$seer <- scale_seer(sites)

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
  # STEP 3: HELPER FUNCTIONS FOR OUTPUT
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
  # STEP 4: GENERATE FIGURES FROM data_tables
  # ============================================
  message("Generating figures")
  
  # Tumor site figures
  save_figure(
    create_bar_plot(data_tables$site_all, "tumor site"),
    sprintf("publications_by_tumor_site_%s-%s.png", data_tables$site_all$year_min, data_tables$site_all$year_max)
  )
  save_figure(
    create_bar_plot(data_tables$site_recent, "tumor site"),
    sprintf("publications_by_tumor_site_%s-%s.png", data_tables$site_recent$year_min, data_tables$site_recent$year_max)
  )
  
  # Year figure - uses data_tables$year$counts directly
  year_counts <- data_tables$year$counts
  lp_year <- ggplot(year_counts, aes(x = factor(year_num), y = count, group = 1)) + 
    geom_line(color = seer_palette()[1], linewidth = 1)
  lp_year <- .format_plot(lp_year, x_label = "year", title = "Publications by year")
  save_figure(lp_year, "publications_by_year.png")
  
  # Year by tumor site figure - uses data_tables$year_by_site$counts directly
  year_by_site <- data_tables$year_by_site$counts
  lp_site <- ggplot(year_by_site, aes(x = factor(year), y = count, group = values, color = values, linetype = values)) + 
    geom_line(linewidth = 1) +
    scale_color_manual(values = seer_palette()) + 
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"))
  lp_site <- .format_plot(lp_site, x_label = "year", title = "Publications by year and top 4 tumor sites")
  save_figure(lp_site, "publications_by_year_and_tumor_site.png")
  
  # Author figures
  save_figure(
    create_bar_plot(data_tables$author_all, "author", display = 10),
    sprintf("publications_by_author_%s-%s.png", data_tables$author_all$year_min, data_tables$author_all$year_max)
  )
  save_figure(
    create_bar_plot(data_tables$author_recent, "author", display = 10),
    sprintf("publications_by_author_%s-%s.png", data_tables$author_recent$year_min, data_tables$author_recent$year_max)
  )
  
  # Journal figures
  save_figure(
    create_bar_plot(data_tables$journal_all, "journal", display = 10),
    sprintf("publications_by_journal_%s-%s.png", data_tables$journal_all$year_min, data_tables$journal_all$year_max)
  )
  save_figure(
    create_bar_plot(data_tables$journal_recent, "journal", display = 10),
    sprintf("publications_by_journal_%s-%s.png", data_tables$journal_recent$year_min, data_tables$journal_recent$year_max)
  )
  
  # SEER figures
  for (x in seer_cols()) {
    save_figure(seer_plot(x, seer_pub_data), sprintf("publications_and_%s.png", x))
  }

  # ============================================
  # STEP 5: GENERATE EXCEL FROM SAME data_tables
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
  
  # Tumor Site sheet - from data_tables$site_all and data_tables$site_recent
  site_excel <- format_paired_counts(data_tables$site_all, data_tables$site_recent, "Tumor Site")
  openxlsx::addWorksheet(wb, "Tumor Site")
  openxlsx::writeData(wb, "Tumor Site", site_excel)
  
  # Year sheet - from data_tables$year$counts
  year_excel <- data.table(
    Year = data_tables$year$counts$year_num, 
    Publications = data_tables$year$counts$count
  )
  setorder(year_excel, -Year)
  openxlsx::addWorksheet(wb, "Year")
  openxlsx::writeData(wb, "Year", year_excel)
  
  # Year and Top Tumor Site sheet - from data_tables$year_by_site$counts
  year_site_excel <- dcast(data_tables$year_by_site$counts, year ~ values, value.var = "count")
  setnames(year_site_excel, c("Year", paste("Publications:", names(year_site_excel)[-1])))
  openxlsx::addWorksheet(wb, "Year and Top Tumor Site")
  openxlsx::writeData(wb, "Year and Top Tumor Site", year_site_excel)
  
  # Author sheet - from data_tables$author_all and data_tables$author_recent
  author_excel <- format_paired_counts(data_tables$author_all, data_tables$author_recent, "Author")
  openxlsx::addWorksheet(wb, "Author")
  openxlsx::writeData(wb, "Author", author_excel)
  
  # Journal sheet - from data_tables$journal_all and data_tables$journal_recent
  journal_excel <- format_paired_counts(data_tables$journal_all, data_tables$journal_recent, "Journal")
  openxlsx::addWorksheet(wb, "Journal")
  openxlsx::writeData(wb, "Journal", journal_excel)
  
  # Incidence Prevalence Deaths sheet - from data_tables$seer
  seer_excel <- copy(data_tables$seer)[, -c("allages", "min65")]
  setnames(seer_excel, c("Tumor Site", "Publications", sapply(seer_cols(), .seer_label, sites, metadata)))
  openxlsx::addWorksheet(wb, "Incidence Prevalence Deaths")
  openxlsx::writeData(wb, "Incidence Prevalence Deaths", seer_excel)
  
  # Save Excel file
  doc <- file.path(excelPath, "SEER_Medicare_Data_Tables.xlsx")
  message("Writing Excel file to ", doc)
  openxlsx::saveWorkbook(wb, doc, overwrite = TRUE)

  # ============================================
  # STEP 6: PACKAGE AND COPY FIGURES
  # ============================================
  message("Writing Figures zip file")
  files <- dir(figure_path, full.names = TRUE)
  zip(zipfile = file.path(outputPath, "SEER_Medicare_Figures"), files = files)
  
  message("Copying individual figure files to ", outputPath)
  file.copy(files, outputPath, overwrite = TRUE)
  
  setwd(curr_wd)
}
