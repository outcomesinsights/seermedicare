#' Format SEER Tables Using ReporteRs FlexTable
#'
#' @param dt data.table to use as FlexTable data
#' @param title String to use as table title
#' @param footnote String to use as table footnote
#' @param less_than numeric value to use as threshold for cell colors (cells with values less than this parameter are colored orange) -
#' if NULL, table has banded rows
#'
#' @return ReporteRs package FlexTable
#' @import data.table magrittr ReporteRs
#' @export
#'
#' @examples \dontrun{}
.format_table <- function(dt, title = "", footnote = "", less_than = NULL) {
  table = FlexTable(data = dt, header.cell.props = cellProperties(text.direction = "btlr", background.color = palette()[1]),
                    header.par.props = parCenter(), header.text.props = textProperties(color = "white", font.size = 10),
                    body.text.props = textProperties(font.size = 10), body.par.props = parCenter(padding.left = 0, padding.right = 0))
  if (is.null(less_than)) {
    setZebraStyle(table, odd = "lightsteelblue", even = "gainsboro")
  } else { # color cells that are less than less_than parameter
    invisible(suppressWarnings(sapply(1:ncol(dt), function(x){setFlexTableBackgroundColors(table, j = x, colors = ifelse(!is.na(as.numeric(dt[[x]])), ifelse(as.numeric(dt[[x]]) < less_than, "sandybrown", "gainsboro"), "gainsboro"))})))
  }
  addHeaderRow(table, title, ncol(dt), textProperties(font.size = 12, font.weight = "bold"), cell.properties = cellProperties(border.style = "none", padding.bottom = 12), first = T)
  addFooterRow(table, footnote, ncol(dt), textProperties(font.size = 10), parLeft(), cellProperties(border.style = "none", padding.top = 6))
}

#' Scale Publications to Incidence, Prevalence, and Deaths Data
#'
#' @return data.table
#' @import data.table magrittr
#' @export
#'
#' @examples \dontrun{
#' scale_seer()
#' }
scale_seer <- function(sites) {
  copy(sites) %>% setkey("site") %>% .[, (seer_cols()) := lapply(.SD, function(x){x / get_scale(x)}), .SDcols = seer_cols()]
}

#' Create SEER Tables
#'
#' @param standardize Logical value - if TRUE, standardizes publications ratios to median
#' @param ... Further arguments passed to .format_table()
#'
#' @return ReporteRs package FlexTable containing total publications, % incident patients 65 or older, publications to incidence ratios,
#' publications to prevalence ratios, and publications to deaths ratios for each tumor site
#' @import data.table magrittr
#' @export
#'
#' @examples \dontrun{
#' seer_table()
#' seer_table(standardize = T)
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
  dt$variable <- c("Total\u000APublications", sprintf("%% incident patients 65 years\u000Aor older (%s)", metadata$cdc_year), sapply(seer_cols(), function(x){.seer_label(x, sites, metadata, sprintf("Pub/%s ratio\u000A", x))}))
  setnames(dt, "variable", " ")
  .format_table(dt, paste0("Publications per patient according to incidence, prevalence, and deaths", ifelse(standardize, ", standardized to the median ratio", "")), ...)
}
