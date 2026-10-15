#' Generate Interactive Web Page
#'
#' Creates an interactive HTML page with Plotly figures and DataTables
#' for exploring SEER-Medicare publication data.
#'
#' @param seer_pub_data Result of running assemble_seermedicare(). Should have class "seermedicare"
#' @param report_data Optional pre-computed data from prepare_report_data(). If NULL, will compute internally.
#' @param outputPath Path to directory where the web page will be generated. Default is working directory.
#'
#' @return Invisibly returns the path to the generated HTML file.
#' @import data.table
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom DT datatable
#' @importFrom htmltools tags HTML save_html
#' @importFrom stats reorder median
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- assemble_seermedicare(publications, cdc, seer, cdc_year, cdc_wonder_request_xml)
#' report_data <- prepare_report_data(seer_pub_data)
#' generate_web(seer_pub_data, report_data, outputPath = "output/web")
#' }
generate_web <- function(seer_pub_data, report_data = NULL, outputPath = ".") {
  
  if(!inherits(seer_pub_data, "seermedicare")) {
    stop("seer_pub_data argument must be of class seermedicare")
  }
  
  # Use pre-computed data if provided, otherwise compute it
  if (is.null(report_data)) {
    report_data <- prepare_report_data(seer_pub_data)
  }
  
  message("Generating interactive web page")
  
  # Create output directory
  dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
  outputPath <- normalizePath(outputPath, mustWork = TRUE)
  
  # ============================================
  # PREPARE DATA
  # ============================================
  
  # Publications by Year
  year_data <- report_data$year$counts[, .(year = year_num, count)]
  data.table::setorder(year_data, year)
  
  # Publications by Site (all years, all data)
  site_raw <- get_data("sites", report_data$publications)
  site_data <- site_raw[, .(count = .N), by = .(category = as.character(values))][order(-count)]
  
  # Publications by Journal (all years, all data)
  journal_raw <- get_data("journal", report_data$publications)
  journal_data <- journal_raw[, .(count = .N), by = .(category = as.character(values))][order(-count)]
  
  # Publications by Author (all years, all data)
  author_raw <- get_data("authors", report_data$publications)
  author_data <- author_raw[, .(count = .N), by = .(category = as.character(values))][order(-count)]
  
  # SEER data
  seer_data <- data.table::copy(report_data$seer)
  
  # Metadata
  metadata <- report_data$metadata
  total_pubs <- nrow(report_data$publications)
  n_sites <- length(report_data$all_sites)
  
  # ============================================
  # HELPER FUNCTION: Create bar plot with Top N dropdown
  # ============================================
  
  create_bar_plot_with_dropdown <- function(data, title_base, top_ns = c(5, 10, 20, 30), 
                                             default_idx = 2, truncate_labels = FALSE,
                                             fixed_height = 700) {
    seer_blue <- "#2171b5"
    
    fig <- plotly::plot_ly(height = fixed_height)
    
    # Add a trace for each top N value
    for (i in seq_along(top_ns)) {
      n <- min(top_ns[i], nrow(data))
      subset_data <- data.table::copy(head(data, n))
      
      # Create display labels (optionally truncated)
      if (truncate_labels) {
        subset_data[, label := ifelse(nchar(category) > 40, 
                                      paste0(substr(category, 1, 37), "..."), 
                                      category)]
      } else {
        subset_data[, label := category]
      }
      
      # Sort by count ascending so highest values appear at top of horizontal bar chart
      data.table::setorder(subset_data, count)
      
      fig <- fig %>% plotly::add_trace(
        x = subset_data$count,
        y = subset_data$label,
        type = 'bar',
        orientation = 'h',
        marker = list(color = seer_blue),
        hovertemplate = "%{y}<br>Publications: %{x}<extra></extra>",
        visible = (i == default_idx),
        name = paste("Top", top_ns[i])
      )
    }
    
    # Create buttons for dropdown
    buttons <- lapply(seq_along(top_ns), function(i) {
      visible_vec <- rep(FALSE, length(top_ns))
      visible_vec[i] <- TRUE
      list(
        label = paste("Top", top_ns[i]),
        method = "update",
        args = list(list(visible = visible_vec))
      )
    })
    
    fig <- fig %>% plotly::layout(
      title = list(text = title_base, y = 0.95, x = 0.01, xanchor = "left"),
      updatemenus = list(
        list(
          type = "dropdown",
          active = default_idx - 1,
          x = 1.0,
          y = 1.10,
          xanchor = "right",
          yanchor = "top",
          bgcolor = "#f8f9fa",
          bordercolor = "#ddd",
          font = list(size = 14),
          buttons = buttons
        )
      ),
      xaxis = list(title = "Publications"),
      yaxis = list(title = "", categoryorder = "trace"),
      margin = list(l = 200, t = 100, r = 20),
      showlegend = FALSE
    )
    
    fig
  }
  
  # ============================================
  # CREATE PLOTLY FIGURES
  # ============================================

  # Color palette and consistent sizing
  seer_blue <- "#2171b5"
  plot_height <- 700  # Consistent height for all plots
  
  # Figure 1a: Publications by Year (line chart)
  fig_year <- plotly::plot_ly(year_data, x = ~year, y = ~count, type = 'scatter', mode = 'lines+markers',
                      height = plot_height,
                      line = list(color = seer_blue, width = 2),
                      marker = list(color = seer_blue, size = 8)) %>%
    plotly::layout(title = list(text = "Publications by Year", x = 0.01, xanchor = "left"),
           xaxis = list(title = "Year"),
           yaxis = list(title = "Publications"),
           margin = list(t = 80))
  
  # Figure 1b: Publications by Tumor Site (with dropdown)
  fig_site <- create_bar_plot_with_dropdown(site_data, "Publications by Tumor Site", fixed_height = plot_height)
  
  # Figure 1c: Publications by Journal (with dropdown, truncated labels)
  fig_journal <- create_bar_plot_with_dropdown(journal_data, "Publications by Journal", truncate_labels = TRUE, fixed_height = plot_height)
  
  # Figure 1d: Publications by Author (with dropdown)
  fig_author <- create_bar_plot_with_dropdown(author_data, "Publications by Author", fixed_height = plot_height)
  
  # Figure 2: SEER Analysis - Publications relative to Incidence (per 1,000)
  seer_incidence <- data.table::copy(seer_data)[, ratio := publications / incidence][order(-ratio)]
  fig_seer_incidence <- plotly::plot_ly(seer_incidence, x = ~ratio, y = ~reorder(site, ratio), type = 'bar',
                                 height = plot_height,
                                 orientation = 'h', marker = list(color = seer_blue),
                                 hovertemplate = "%{y}<br>Ratio: %{x:.2f}<extra></extra>") %>%
    plotly::layout(title = list(text = "Publications Relative to Incidence", x = 0.01, xanchor = "left"),
           xaxis = list(title = "Publications per 1,000 incident patients"),
           yaxis = list(title = ""),
           margin = list(l = 180, t = 80))
  
  # Figure 2b: SEER Analysis - Publications relative to Prevalence (per 10,000)
  seer_prevalence <- data.table::copy(seer_data)[, ratio := publications / prevalence][order(-ratio)]
  fig_seer_prevalence <- plotly::plot_ly(seer_prevalence, x = ~ratio, y = ~reorder(site, ratio), type = 'bar',
                                  height = plot_height,
                                  orientation = 'h', marker = list(color = seer_blue),
                                  hovertemplate = "%{y}<br>Ratio: %{x:.2f}<extra></extra>") %>%
    plotly::layout(title = list(text = "Publications Relative to Prevalence", x = 0.01, xanchor = "left"),
           xaxis = list(title = "Publications per 10,000 prevalent patients"),
           yaxis = list(title = ""),
           margin = list(l = 180, t = 80))
  
  # Figure 2c: SEER Analysis - Publications relative to Deaths (per 1,000)
  seer_deaths <- data.table::copy(seer_data)[, ratio := publications / deaths][order(-ratio)]
  fig_seer_deaths <- plotly::plot_ly(seer_deaths, x = ~ratio, y = ~reorder(site, ratio), type = 'bar',
                              height = plot_height,
                              orientation = 'h', marker = list(color = seer_blue),
                              hovertemplate = "%{y}<br>Ratio: %{x:.2f}<extra></extra>") %>%
    plotly::layout(title = list(text = "Publications Relative to Deaths", x = 0.01, xanchor = "left"),
           xaxis = list(title = "Publications per 1,000 deaths"),
           yaxis = list(title = ""),
           margin = list(l = 180, t = 80))
  
  # ============================================
  # PREPARE DATA FOR TABLES AND EXCEL EXPORT
  # ============================================
  
  # Prepare year data for display (most recent first)
  year_export <- data.table::copy(year_data)
  data.table::setorder(year_export, -year)
  data.table::setnames(year_export, c("Year", "Publications"))
  
  # Prepare site data for export
  site_export <- data.table::copy(site_data)
  data.table::setnames(site_export, c("Tumor Site", "Publications"))
  
  # Prepare journal data for export
  journal_export <- data.table::copy(journal_data)
  data.table::setnames(journal_export, c("Journal", "Publications"))
  
  # Prepare author data for export
  author_export <- data.table::copy(author_data)
  data.table::setnames(author_export, c("Author", "Publications"))
  
  # Table 5: SEER Results - Per Patient Ratios (with units in column headers)
  seer_ratios <- data.table::copy(seer_data)[, .(
    `Tumor Site` = site,
    Publications = publications,
    `Pub per 1,000 Incident` = round(publications / incidence, 2),
    `Pub per 10,000 Prevalent` = round(publications / prevalence, 2),
    `Pub per 1,000 Deaths` = round(publications / deaths, 2)
  )][order(-`Pub per 1,000 Incident`)]
  
  # Table 6: SEER Results - Standardized Ratios (relative to median)
  seer_std <- data.table::copy(seer_data)
  seer_std[, `:=`(
    inc_ratio = publications / incidence,
    prev_ratio = publications / prevalence,
    death_ratio = publications / deaths
  )]
  seer_std[, `:=`(
    inc_std = round(inc_ratio / median(inc_ratio), 2),
    prev_std = round(prev_ratio / median(prev_ratio), 2),
    death_std = round(death_ratio / median(death_ratio), 2)
  )]
  seer_std_table <- seer_std[, .(
    `Tumor Site` = site,
    Publications = publications,
    `vs Median (Incidence)` = inc_std,
    `vs Median (Prevalence)` = prev_std,
    `vs Median (Deaths)` = death_std
  )][order(-`vs Median (Incidence)`)]
  
  # ============================================
  # CREATE EXCEL WORKBOOK WITH ALL DATA
  # ============================================
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Publications by Year")
  openxlsx::writeData(wb, "Publications by Year", year_export)
  
  openxlsx::addWorksheet(wb, "Publications by Site")
  openxlsx::writeData(wb, "Publications by Site", site_export)
  
  openxlsx::addWorksheet(wb, "Publications by Journal")
  openxlsx::writeData(wb, "Publications by Journal", journal_export)
  
  openxlsx::addWorksheet(wb, "Publications by Author")
  openxlsx::writeData(wb, "Publications by Author", author_export)
  
  openxlsx::addWorksheet(wb, "SEER Per Patient Ratios")
  openxlsx::writeData(wb, "SEER Per Patient Ratios", seer_ratios)
  
  openxlsx::addWorksheet(wb, "SEER Standardized Ratios")
  openxlsx::writeData(wb, "SEER Standardized Ratios", seer_std_table)
  
  # About tab with methodology and extraction details
  about_data <- data.table::data.table(
    Field = c(
      "Data Source",
      "Source URL",
      "Data Extraction Date",
      "Total Publications",
      "R Package",
      "",
      "METHODOLOGY",
      "Publication Year",
      "Journal Names",
      "SEER Statistics",
      "CDC Data",
      "",
      "NOTES",
      "Date Handling",
      "",
      "Missing Data"
    ),
    Value = c(
      "NCI Healthcare Delivery Research Program Publications API",
      "https://healthcaredelivery.cancer.gov/publications/",
      format(as.Date(metadata$access_date), "%B %d, %Y"),
      as.character(total_pubs),
      "seermedicare (https://github.com/outcomesinsights/seermedicare)",
      "",
      "",
      "Determined by electronic publication date (edate) when available; otherwise print publication date (pubdate)",
      "ISO abbreviations retrieved from PubMed E-utilities API using PMIDs",
      "Scraped from SEER Cancer Statistics Fact Sheets (seer.cancer.gov/statfacts/)",
      "Incidence by age from CDC WONDER",
      "",
      "",
      "Approximately 60% of publications have different electronic vs. print dates. Using edate assigns publications to the year they first became available online, which may differ from NCI website counts by 10-20% per year.",
      "",
      "Publications without specific tumor site data are categorized as 'Not Specified'"
    )
  )
  openxlsx::addWorksheet(wb, "About")
  openxlsx::writeData(wb, "About", about_data)
  
  excel_filename <- "SEER-Medicare-Publications.xlsx"
  excel_path <- file.path(outputPath, excel_filename)
  openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)
  message(sprintf("Excel file created: %s", excel_path))
  
  # ============================================
  # CREATE DATA TABLES (without download buttons)
  # ============================================
  
  # Table 1: Publications by Year (default: most recent first, left-aligned)
  dt_year <- DT::datatable(year_data, 
                       colnames = c("Year", "Publications"),
                       rownames = FALSE,
                       options = list(
                         pageLength = 15,
                         dom = 'frtip',
                         order = list(list(0, 'desc')),
                         columnDefs = list(
                           list(className = 'dt-left', targets = '_all')
                         )
                       ))
  
  # Table 2: Publications by Site
  dt_site <- DT::datatable(site_data,
                       colnames = c("Tumor Site", "Publications"),
                       rownames = FALSE,
                       options = list(
                         pageLength = 15,
                         dom = 'frtip'
                       ))
  
  # Table 3: Publications by Journal
  dt_journal <- DT::datatable(journal_data,
                          colnames = c("Journal", "Publications"),
                          rownames = FALSE,
                          options = list(
                            pageLength = 15,
                            dom = 'frtip'
                          ))
  
  # Table 4: Publications by Author
  dt_author <- DT::datatable(author_data,
                         colnames = c("Author", "Publications"),
                         rownames = FALSE,
                         options = list(
                           pageLength = 15,
                           dom = 'frtip'
                         ))
  
  # Table 5: SEER Per Patient Ratios
  dt_seer_ratios <- DT::datatable(seer_ratios,
                              rownames = FALSE,
                              options = list(
                                pageLength = 20,
                                dom = 'frtip'
                              ))
  
  # Table 6: SEER Standardized Ratios
  dt_seer_std <- DT::datatable(seer_std_table,
                           rownames = FALSE,
                           options = list(
                             pageLength = 20,
                             dom = 'frtip'
                           ))
  
  # ============================================
  # BUILD HTML PAGE
  # ============================================
  
  html_content <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title("SEER-Medicare Publications"),
      htmltools::tags$style(htmltools::HTML("
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
               margin: 0; padding: 0; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; padding: 20px; }
        h1, h2, h3 { color: #333; }
        h1 { border-bottom: 3px solid #2171b5; padding-bottom: 10px; }
        h2 { border-bottom: 1px solid #ddd; padding-bottom: 8px; margin-top: 40px; }
        .section { background: white; padding: 25px; margin: 20px 0; border-radius: 8px; 
                   box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .meta-box { background: #e8f4fd; padding: 15px; border-radius: 6px; margin: 15px 0; }
        .meta-box ul { margin: 10px 0; padding-left: 20px; }
        .tab-container { margin: 20px 0; }
        .tab-buttons { display: flex; gap: 5px; flex-wrap: wrap; margin-bottom: 15px; }
        .tab-btn { padding: 10px 20px; border: 1px solid #ddd; background: #f8f9fa; 
                   cursor: pointer; border-radius: 4px 4px 0 0; font-size: 15px; }
        .tab-btn.active { background: #2171b5; color: white; border-color: #2171b5; }
        .tab-content { display: none; }
        .tab-content.active { display: block; }
        .contact { background: #f8f9fa; padding: 20px; border-radius: 6px; }
        .contact p { margin: 5px 0; }
        a { color: #2171b5; }
        .subtitle { color: #666; font-size: 1.2em; margin-top: -10px; }
        /* Download button styling */
        .btn-download {
          display: inline-block;
          background-color: #2171b5;
          color: white !important;
          text-decoration: none;
          padding: 12px 24px;
          border-radius: 4px;
          font-size: 16px;
          font-weight: 500;
          cursor: pointer;
        }
        .btn-download:hover {
          background-color: #1a5a8a;
          text-decoration: none;
        }
        .download-box { 
          background: #e8f4fd; 
          padding: 20px; 
          border-radius: 6px; 
          text-align: center;
        }
        .download-box p { margin: 10px 0 15px 0; }
      "))
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container",
        # Header
        htmltools::tags$h1("SEER-Medicare Publications"),
        htmltools::tags$p(class = "subtitle", "Interactive Summary Report - Outcomes Insights, Inc."),
        
        # Overview Section
        htmltools::tags$div(class = "section",
          htmltools::tags$h2("Overview"),
          htmltools::tags$p("Information was extracted from the National Cancer Institute (NCI) online reference database for \"SEER-Medicare\" publications."),
          htmltools::tags$p(
            htmltools::tags$strong("Data Source: "), 
            htmltools::tags$a(href = metadata$nci_url, "NCI Publications Database", target = "_blank")
          ),
          htmltools::tags$p(
            htmltools::tags$strong("Code: "), 
            htmltools::tags$a(href = "https://github.com/outcomesinsights/seermedicare", "seermedicare R package", target = "_blank"),
            " (GitHub)"
          ),
          htmltools::tags$div(class = "meta-box",
            htmltools::tags$ul(
              htmltools::tags$li(htmltools::tags$strong("Data accessed: "), format(as.Date(metadata$access_date), "%B %d, %Y")),
              htmltools::tags$li(htmltools::tags$strong("Citations retrieved: "), format(total_pubs, big.mark = ",")),
              htmltools::tags$li(htmltools::tags$strong("Tumor types: "), n_sites)
            )
          ),
          htmltools::tags$h3("Methodology"),
          htmltools::tags$ul(
            htmltools::tags$li("Summary measures were calculated by year, tumor site, journal, and author"),
            htmltools::tags$li("Publication counts were plotted against tumor types"),
            htmltools::tags$li("Counts of incident and prevalent patients, as well as deaths from cancer, were taken from SEER published estimates"),
            htmltools::tags$li("Ratios of publications to these patient counts were estimated:"),
            htmltools::tags$ul(
              htmltools::tags$li("Incidence (per 1,000 patients)"),
              htmltools::tags$li("Prevalence (per 10,000 patients)"),
              htmltools::tags$li("Deaths (per 1,000 patients)")
            ),
            htmltools::tags$li("Higher numbers reflect more publications per patient"),
            htmltools::tags$li("The median publication ratio across all tumors was used as an index for establishing high versus low ratios")
          ),
          htmltools::tags$h3("Data Note"),
          htmltools::tags$p("Publication counts on this page may differ from the ",
            htmltools::tags$a(href = "https://healthcaredelivery.cancer.gov/publications/", 
                              "NCI SEER-Medicare Publications Search", target = "_blank"),
            ". This is due to how publication year is determined:"
          ),
          htmltools::tags$ul(
            htmltools::tags$li(htmltools::tags$strong("This page: "), 
              "Uses the electronic publication date when available, falling back to print publication date. This reflects when papers first became available online."),
            htmltools::tags$li(htmltools::tags$strong("NCI website: "), 
              "Filters by a computed date based on the print publication date.")
          ),
          htmltools::tags$p(htmltools::tags$em(
            "Approximately 60% of publications have different electronic vs. print dates, which can shift year-over-year counts by 10-20%."
          ))
        ),
        
        # Publications Explorer Section
        htmltools::tags$div(class = "section",
          htmltools::tags$h2("Publications Explorer"),
          htmltools::tags$p("Explore publication patterns by different dimensions."),
          
          # Tabs for figure selection
          htmltools::tags$div(class = "tab-container",
            htmltools::tags$div(class = "tab-buttons",
              htmltools::tags$button(class = "tab-btn active", onclick = "showTab('pub', 'year')", "By Year"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('pub', 'site')", "By Tumor Site"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('pub', 'journal')", "By Journal"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('pub', 'author')", "By Author")
            ),
            htmltools::tags$div(id = "pub-year", class = "tab-content active", fig_year),
            htmltools::tags$div(id = "pub-site", class = "tab-content", fig_site),
            htmltools::tags$div(id = "pub-journal", class = "tab-content", fig_journal),
            htmltools::tags$div(id = "pub-author", class = "tab-content", fig_author)
          ),
          
          htmltools::tags$h3("Data Tables"),
          htmltools::tags$div(class = "tab-container",
            htmltools::tags$div(class = "tab-buttons",
              htmltools::tags$button(class = "tab-btn active", onclick = "showTab('table', 'year')", "Year"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('table', 'site')", "Tumor Site"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('table', 'journal')", "Journal"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('table', 'author')", "Author")
            ),
            htmltools::tags$div(id = "table-year", class = "tab-content active", dt_year),
            htmltools::tags$div(id = "table-site", class = "tab-content", dt_site),
            htmltools::tags$div(id = "table-journal", class = "tab-content", dt_journal),
            htmltools::tags$div(id = "table-author", class = "tab-content", dt_author)
          )
        ),
        
        # SEER Analysis Section
        htmltools::tags$div(class = "section",
          htmltools::tags$h2("SEER Analysis"),
          htmltools::tags$p("Publication frequency relative to cancer incidence, prevalence, and deaths."),
          htmltools::tags$p(htmltools::tags$em("Higher values indicate more publications per patient.")),
          
          # Tabs for SEER figures
          htmltools::tags$div(class = "tab-container",
            htmltools::tags$div(class = "tab-buttons",
              htmltools::tags$button(class = "tab-btn active", onclick = "showTab('seer', 'incidence')", "Incidence"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('seer', 'prevalence')", "Prevalence"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('seer', 'deaths')", "Deaths")
            ),
            htmltools::tags$div(id = "seer-incidence", class = "tab-content active", fig_seer_incidence),
            htmltools::tags$div(id = "seer-prevalence", class = "tab-content", fig_seer_prevalence),
            htmltools::tags$div(id = "seer-deaths", class = "tab-content", fig_seer_deaths)
          ),
          
          htmltools::tags$h3("SEER Results Tables"),
          htmltools::tags$div(class = "tab-container",
            htmltools::tags$div(class = "tab-buttons",
              htmltools::tags$button(class = "tab-btn active", onclick = "showTab('seertable', 'ratios')", "Per Patient Ratios"),
              htmltools::tags$button(class = "tab-btn", onclick = "showTab('seertable', 'std')", "Standardized Ratios")
            ),
            htmltools::tags$div(id = "seertable-ratios", class = "tab-content active", dt_seer_ratios),
            htmltools::tags$div(id = "seertable-std", class = "tab-content", dt_seer_std)
          )
        ),
        
        # Download Data Section
        htmltools::tags$div(class = "section",
          htmltools::tags$h2("Download Data"),
          htmltools::tags$div(class = "download-box",
            htmltools::tags$p("Download all data shown on this page in a single Excel file with multiple worksheets."),
            htmltools::tags$a(
              href = excel_filename,
              download = excel_filename,
              class = "btn-download",
              "Download All Data (Excel)"
            )
          )
        ),
        
        # Contact Section
        htmltools::tags$div(class = "section",
          htmltools::tags$h2("Contact Information"),
          htmltools::tags$div(class = "contact",
            htmltools::tags$p(htmltools::tags$strong("Mark D. Danese, MHS, PhD")),
            htmltools::tags$p("Outcomes Insights, Inc."),
            htmltools::tags$p("4500 Park Granada Suite 202"),
            htmltools::tags$p("Calabasas, CA 91302"),
            htmltools::tags$p("805-498-0034"),
            htmltools::tags$p(htmltools::tags$a(href = "mailto:mark@outins.com", "mark@outins.com")),
            htmltools::tags$p(htmltools::tags$a(href = "https://www.outins.com/", target = "_blank", "www.outins.com"))
          )
        )
      ),
      
      # Tab switching JavaScript
      htmltools::tags$script(htmltools::HTML("
        function showTab(group, name) {
          // Hide all tabs in group
          document.querySelectorAll('[id^=\"' + group + '-\"]').forEach(el => {
            el.classList.remove('active');
          });
          // Show selected tab
          document.getElementById(group + '-' + name).classList.add('active');
          
          // Update buttons
          const container = document.getElementById(group + '-' + name).parentElement;
          container.querySelectorAll('.tab-btn').forEach((btn, idx) => {
            btn.classList.remove('active');
          });
          event.target.classList.add('active');
          
          // Trigger resize for plotly
          window.dispatchEvent(new Event('resize'));
        }
      "))
    )
  )
  
  # Save to file
  html_path <- file.path(outputPath, "index.html")
  lib_path <- file.path(outputPath, "lib")
  # Start lib/ fresh so library folders from older package versions don't accumulate
  unlink(lib_path, recursive = TRUE)

  htmltools::save_html(html_content, file = html_path, libdir = lib_path)
  
  message(sprintf("Web page generated: %s", html_path))
  message(sprintf("Dependencies saved to: %s", lib_path))
  
  invisible(html_path)
}
