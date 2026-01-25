
#' Add Plot to Document suppressing warnings
#'
#' Uses grDevices::png() with pixel dimensions to avoid rendering issues
#' with certain ggplot configurations (e.g., bar plots with factors having many levels).
#'
#' @param doc officer docx object where plot should be added
#' @param plot plot to be added to the document
#' @param width plot width in inches (default 7 for near full-page width)
#' @param height plot height in inches (default 5.5 for good proportions)
#' @param res plot resolution in DPI (default 150 for sharper images)
#'
#' @return doc officer docx object
#' @importFrom grDevices png dev.off
#' @export
myAddPlot <- function(doc, plot, width = 7.5, height = 6, res = 150) {
  # Use pixel dimensions instead of units="in" to avoid rendering issues
  file <- tempfile(fileext = ".png")
  width_px <- round(width * res)
  height_px <- round(height * res)
  grDevices::png(filename = file, width = width_px, height = height_px)
  print(plot)
  grDevices::dev.off()
  on.exit(unlink(file))
  officer::body_add_img(doc, src = file, width = width, height = height)
}

#' Add Blank Line to Report
#'
#' @param doc officer rdocx object where line should be added
#'
#' @return officer rdocx object with blank line added
#' @export
nline <- function(doc){
  officer::body_add_par(doc, "")
}


#' Determine list style name for level
#'
#' @param n Integer representing list level
#' @param ordered Logical value for ordered list
#'
#' @return style name
list_style <- function(n, ordered = FALSE){
  base <- if (ordered) "List Number" else "List Bullet"
  if (n > 1) {
    paste(base, n)
  } else {
    base
  }
}

#' Set Unordered List Level
#'
#' @param n Integer representing unordered list level
#'
#' @return style name
#' @export
#'
#' @examples \dontrun{
#' level(1)
#' }
level <- function(n){
  list_style(n, ordered = FALSE)
}

#' Add paragraphs to a doc
#'
#' @param doc officer docx object
#' @param text character vector of paragraphs
#' @param style Word style name
#'
#' @return doc officer docx object
doc_add_paragraphs <- function(doc, text, style = "Normal") {
  for (item in text) {
    doc <- officer::body_add_par(doc, value = item, style = style)
  }
  doc
}

#' Add list items to a doc
#'
#' @param doc officer docx object
#' @param items character vector
#' @param ordered Logical value for ordered list
#' @param level Integer list level
#'
#' @return doc officer docx object
doc_add_list <- function(doc, items, ordered = FALSE, level = 1) {
  style <- list_style(level, ordered = ordered)
  available <- officer::styles_info(doc, type = "paragraph")$style_name
  if (!style %in% available) {
    style <- "Normal"
  }
  for (item in items) {
    doc <- officer::body_add_par(doc, value = item, style = style)
  }
  doc
}

#' Add a title to a doc
#'
#' @param doc officer docx object
#' @param text title text
#' @param level Integer heading level
#'
#' @return doc officer docx object
doc_add_title <- function(doc, text, level = 1) {
  style <- switch(
    as.character(level),
    "1" = "heading 1",
    "2" = "heading 2",
    "3" = "heading 3",
    "4" = "Title",
    "heading 1"
  )
  officer::body_add_par(doc, value = text, style = style)
}

#' Add a hyperlink paragraph
#'
#' @param doc officer docx object
#' @param text link text
#' @param url link URL
#' @param style Word style name
#'
#' @return doc officer docx object
doc_add_hyperlink <- function(doc, text, url, style = "Normal") {
  available <- officer::styles_info(doc, type = "paragraph")$style_name
  if (!style %in% available) {
    style <- "Normal"
  }
  if (is.null(url) || !nzchar(url)) {
    return(officer::body_add_par(doc, value = text, style = style))
  }
  fp <- officer::fpar(officer::hyperlink_ftext(text = text, href = url))
  officer::body_add_fpar(doc, value = fp, style = style)
}


#' Determine Underrepresented Years in Publications Data
#'
#' Assumes that publication data for any given year is fully represented 3 months after the year ends
#' Calculates if data is underrepresented using data access date and latest year in data
#'
#' @param seer_pub_data Object of class seermedicare from scrape_all_data
#'
#' @return String containing underrepresented data years, or NULL, if no years are underrepresented
#' @export
.underrepresented <- function(seer_pub_data){
  accessdate <- seer_pub_data$metadata$access_date
  yeardata <- seer_pub_data$publications

  accessyear <- lubridate::year(accessdate)
  maxyear <- max(yeardata$year) %>% as.numeric
  ay <- NA
  py <- NA
  if(accessyear - maxyear < 2){
    ay <- ifelse(accessyear <= maxyear, maxyear, NA)
    py <- ifelse(lubridate::month(accessdate) < 4, accessyear - 1, NA)
  }
  if(!is.na(py) || !is.na(ay)){
    urep <- c(py, ay) %>% na.omit %>% paste(., collapse = " and ")
    urep <- paste(" - articles published in", urep, "may be underrepresented due to reporting lag")
  }else{
    urep <- NULL
  }
  return(urep)
}

