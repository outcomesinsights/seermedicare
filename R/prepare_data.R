#' Prepare All Data for Reports and Figures
#'
#' Creates a single source of truth for all data used in reports, figures, and Excel output.
#' This function should be called once, and the result passed to generate_report() and generate_figures().
#'
#' @param seer_pub_data Result of running assemble_seermedicare(). Should have class "seermedicare"
#'
#' @return A list containing all computed data tables:
#' \itemize{
#'   \item site_all - Tumor site counts for all years
#'   \item site_recent - Tumor site counts for recent 7 years
#'   \item year - Publication counts by year
#'   \item year_by_site - Yearly counts for top 4 tumor sites
#'   \item author_all - Author counts for all years (top 10)
#'   \item author_recent - Author counts for recent 7 years (top 10)
#'   \item journal_all - Journal counts for all years (top 10)
#'   \item journal_recent - Journal counts for recent 7 years (top 10)
#'   \item seer - Scaled SEER data (incidence, prevalence, deaths)
#'   \item all_sites - Vector of all tumor site names (ordered by frequency)
#'   \item period - The recent time period (7 years)
#'   \item metadata - Original metadata from seer_pub_data
#'   \item publications - Original publications data
#'   \item sites - Original site_summary data
#' }
#'
#' @import data.table magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- assemble_seermedicare(publications, cdc, seer, cdc_year, cdc_wonder_request_xml)
#' report_data <- prepare_report_data(seer_pub_data)
#' generate_report(seer_pub_data, report_data = report_data)
#' generate_figures(seer_pub_data, report_data = report_data)
#' }
prepare_report_data <- function(seer_pub_data) {
  
  if(!inherits(seer_pub_data, "seermedicare")) {
    stop("seer_pub_data argument must be of class seermedicare")
  }
  
  publications <- seer_pub_data$publications
  metadata <- seer_pub_data$metadata
  sites <- seer_pub_data$site_summary
  
  ## Get last 7 years not including the most recent year as there is a lag
  last_reporting_year <- metadata$prevalence_year
  if (is.null(last_reporting_year) || is.na(last_reporting_year)) {
    last_reporting_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  } else {
    last_reporting_year <- as.numeric(last_reporting_year) - 1
  }
  period <- seq(from = last_reporting_year - 6, to = last_reporting_year)
  
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
  
  # Initialize data list
  data_tables <- list()
  
  # Store original data and metadata
  data_tables$metadata <- metadata
  data_tables$publications <- publications
  data_tables$sites <- sites
  data_tables$period <- period
  
  # --- Tumor Site Data ---
  data_tables$site_all <- prepare_counts("sites", publications)
  data_tables$site_recent <- prepare_counts("sites", publications[year %in% period])
  
  # All sites vector (for report text)
  data_tables$all_sites <- levels(data_tables$site_all$counts$values)
  
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
  
  data_tables
}
