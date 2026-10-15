#' Create Error Message - Column Not Found in Dataset
#'
#' @param x String column name to check for in dataset
#' @param dataset dataset to search
#'
#' @return Stops with error if column not found, otherwise returns invisibly
.arg_error <- function(x, dataset){
  if(!x %in% colnames(dataset)){
    stop(paste0("Invalid argument '", x, "': column not found in dataset"), call. = F)
  }
}

#' Get Publications Data
#'
#' @param x String representing data to return - should be column name in dataset
#' @param dataset Dataset from which to get column data
#'
#' @return Data with parameter column factored by frequency (NCI publications data used in bar and line plot functions)
#'
#' @examples
#' \dontrun{
#' get_data("sites")
#' }
get_data <- function(x, dataset = publications) {
  .arg_error(x, dataset)
  if (x == "year") {
    # Special case: when x is "year", we need both a "values" column and keep "year"
    data <- dataset[, lapply(.SD, unlist), by = "id", .SDcols = "year"]
    data[, values := year]
  } else {
    data <- dataset[, lapply(.SD, unlist), by = "id", .SDcols = c(x, "year")] %>% setnames(x, "values")
  }
  data[, `:=`(values = factor(values, levels = names(sort(table(values), decreasing = T))))]
}

#' Create Data Scales (used for incidence, prevalence, and deaths data)
#'
#' @param x Data column to use to calculate scale - should be "incidence", "prevalence", or "deaths"
#' @param limit Numeric limit - scales will be created to keep max values under this limit
#'
#' @return Base 10 scale calculated from max value and target limit (to use as denominator)
#'
#' @examples \dontrun{
#' get_scale(sites$incidence)
#' }
get_scale <- function(x, limit = 500) {
  10^ceiling(log10(max(x, na.rm = T) / limit))
}

#' Get Column Names for Relevant SEER Data
#'
#' @return Character vector column names
#'
#' @examples \dontrun{
#' seer_cols()
#' }
seer_cols <- function() {
  c("incidence", "prevalence", "deaths")
}

#' Scale Publications to Incidence, Prevalence, and Deaths Data
#'
#' @param sites data.table of site summary data from scrape_all_data
#'
#' @return data.table with scaled incidence, prevalence, and deaths columns
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' scale_seer(seer_pub_data$site_summary)
#' }
scale_seer <- function(sites) {
  copy(sites) %>% setkey("site") %>% .[, (seer_cols()) := lapply(.SD, function(x){x / get_scale(x)}), .SDcols = seer_cols()]
}

#' Packages the project code needs, read from DESCRIPTION's Imports
#' (DESCRIPTION is only a dependency list; this is not an installable package)
project_packages <- function(description = "DESCRIPTION") {
  imports <- read.dcf(description, fields = "Imports")[1, "Imports"]
  pkgs <- trimws(strsplit(imports, ",")[[1]])
  sub("\\s*\\(.*\\)$", "", pkgs[nzchar(pkgs)])
}
