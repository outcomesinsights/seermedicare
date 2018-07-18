#' Create Error Message - Column Not Found in Dataset
#'
#' @param x String column name to check for in dataset
#' @param dataset dataset to search
#'
#' @return Appropriate error
#' @import data.table
#'
#' @examples \dontrun{}
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
#' @import data.table magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' get_data("sites")
#' }
get_data <- function(x, dataset = publications) {
  .arg_error(x, dataset)
  data <- dataset[, lapply(.SD, unlist), by = "id", .SDcols = c(x, "year")] %>% setnames(x, "values")
  data[, `:=`(values = factor(values, levels = names(sort(table(values), decreasing = T))))]
}

#' Get Metadata Field
#'
#' @param x String representing data to return
#'
#' @return Parameter metadata field
#' @export
#'
#' @examples
#' \dontrun{
#' get_metadata("access_date")
#' }
get_metadata <- function(x) {
  metadata[[x]]
}


#' Create Data Scales (used for incidence, prevalence, and deaths data)
#'
#' @param x Data column to use to calculate scale - should be "incidence", "prevalence", or "deaths"
#' @param limit Numeric limit - scales will be created to keep max values under this limit
#'
#' @return Base 10 scale calculated from max value and target limit (to use as denominator)
#' @export
#'
#' @examples \dontrun{
#' get_scale(sites$incidence)
#' }
get_scale <- function(x, limit = 500) {
  10^ceiling(log10(max(x, na.rm = T) / limit))
}

#' Get Color Palette for Graphics
#'
#' @return Character vector of colors
#' @export
#'
#' @examples \dontrun{
#' palette()
#' }
palette <- function() {
  c("steelblue4", "black", "darkorange4", "chartreuse4")
}

#' Get Column Names for Relevant SEER Data
#'
#' @return Character vector column names
#' @export
#'
#' @examples \dontrun{
#' seer_cols()
#' }
seer_cols <- function() {
  c("incidence", "prevalence", "deaths")
}

#' Format Seer Labels
#'
#' @return String in the form [SEER Data] per [SEER Data Scale] ([SEER Data Year]) - e.g., Incidence per 1,000 (2016)
#' @export
#'
#' @examples \dontrun{}
.seer_label <- function(x, sites, metadata, measure = tools::toTitleCase(x)) {
  sprintf("%s per %s (%s)", measure, format(get_scale(sites[[x]]), big.mark = ","), metadata[[paste0(x, "_year")]])
}

