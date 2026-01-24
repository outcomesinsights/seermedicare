#' Format SEER Tables Using flextable
#'
#' @param dt data.table to use as FlexTable data
#' @param title String to use as table title
#' @param footnote String to use as table footnote
#' @param less_than numeric value to use as threshold for cell colors (cells with values less than this parameter are colored orange) -
#' if NULL, table has banded rows
#'
#' @return flextable object
#' @import data.table magrittr flextable
#' @export
.format_table <- function(dt, title = "", footnote = "", less_than = NULL) {
  table <- flextable::flextable(dt)
  table <- flextable::align(table, align = "center", part = "all")
  table <- flextable::fontsize(table, size = 10, part = "body")
  table <- flextable::bg(table, part = "header", bg = seer_palette()[1])
  table <- flextable::color(table, part = "header", color = "white")
  table <- flextable::bold(table, part = "header")
  table <- flextable::add_header_row(table, values = title, colwidths = ncol(dt))

  if (is.null(less_than)) {
    table <- flextable::theme_zebra(table)
  } else {
    for (col in seq_len(ncol(dt))) {
      values <- suppressWarnings(as.numeric(dt[[col]]))
      if (!all(is.na(values))) {
        table <- flextable::bg(
          table,
          j = col,
          bg = ifelse(values < less_than, "sandybrown", "gainsboro"),
          part = "body"
        )
      }
    }
  }

  if (nzchar(footnote)) {
    table <- flextable::add_footer_lines(table, values = footnote)
  }
  table
}

#' Scale Publications to Incidence, Prevalence, and Deaths Data
#'
#' @param sites data.table of site summary data from scrape_all_data
#'
#' @return data.table with scaled incidence, prevalence, and deaths columns
#' @import data.table magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' scale_seer(seer_pub_data$site_summary)
#' }
scale_seer <- function(sites) {
  copy(sites) %>% setkey("site") %>% .[, (seer_cols()) := lapply(.SD, function(x){x / get_scale(x)}), .SDcols = seer_cols()]
}

#' Create SEER Tables
#'
#' @param seer_pub_data Object of class seermedicare from scrape_all_data
#' @param standardize Logical value - if TRUE, standardizes publications ratios to median
#' @param ... Further arguments passed to .format_table()
#'
#' @return flextable object containing total publications, percent incident patients age 65 or older, publications to incidence ratios,
#' publications to prevalence ratios, and publications to deaths ratios for each tumor site
#' @import data.table magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' seer_table(seer_pub_data)
#' seer_table(seer_pub_data, standardize = TRUE)
#' }
seer_table <- function(seer_pub_data, standardize = F, ...) {
  publications <- seer_pub_data$publications
  sites <- seer_pub_data$site_summary
  metadata <- seer_pub_data$metadata

  # calculate publications to incidence, prevalence, and deaths ratios (scaled)
  dt <- scale_seer(sites)[, (seer_cols()) := lapply(.SD, function(x){publications / x}), .SDcols = seer_cols()]

  # standardize to median
  if (standardize) {dt[, (seer_cols()) := lapply(.SD, function(x){x / stats::median(x)}), .SDcols = seer_cols()]}

  # format numeric columns and transpose
  dt[, (seer_cols()) := lapply(.SD, sprintf, fmt = "%.1f"), .SDcols = seer_cols()]
  dt[, `:=`(publications = sprintf("%.f", publications), min65 = sprintf("%.f%%", dt$min65 / dt$allages * 100), allages = NULL)]
  dt <- melt(dt, id.vars = c("site")) %>% dcast(variable ~ site) %>% .[c("publications", "min65", seer_cols())]

  # create labels/names for table and format as flex table
  dt$variable <- c("Total\u000APublications", sprintf("%% incident patients age 65 years\u000Aor older (%s)", metadata$cdc_year), sapply(seer_cols(), function(x){.seer_label(x, sites, metadata, sprintf("Pub/%s ratio\u000A", x))}))
  setnames(dt, "variable", " ")
  .format_table(dt, paste0("Publications per patient according to incidence, prevalence, and deaths", ifelse(standardize, ", standardized to the median ratio", "")), ...)
}
