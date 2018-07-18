
#' Add Plot to Document suppressing warnings
#'
#' @param doc Object of class \code{\link{docx}} where plot should be added
#' @param plot plot to be added to the document
#'
#' @return doc Object of class \code{\link{docx}}
#' @export
myAddPlot <- function(doc, plot) {
  doc = suppressWarnings(addPlot(doc, print, x = plot, width = 6.5, height = 4))
}

#' Add Blank Line to Report
#'
#' @param doc Object of class \code{\link{docx}} where line should be added
#'
#' @return Object of class \code{\link{docx}} with line added
#' @export
#'
#' @examples \dontrun{}
nline <- function(doc){
  addParagraph(doc, "")
}


#' Set Unordered List Level
#'
#' @param n Integer representing unordered list level
#'
#' @return parProperties object containing unordered list level
#' @export
#'
#' @examples \dontrun{
#' ReporteRs::addParagraph("mytext", par.properties = level(1))
#' }
level <- function(n){
  return(parProperties(list.style = "unordered", level = n))
}


#' Determine Underrepresented Years in Publications Data
#'
#' Assumes that publication data for any given year is fully represented 3 months after the year ends
#' Calculates if data is underrepresented using data access date and latest year in data
#'
#' @return String containing underrepresented data years, or NULL, if no years are underrepresented
#' @export
#'
#' @examples \dontrun{}
.underrepresented <- function(seer_pub_data){
  accessdate <- seer_pub_data$metadata$access_date
  yeardata <- seer_pub_data$publications

  accessyear <- lubridate::year(accessdate)
  maxyear <- max(yeardata$year) %>% as.numeric
  if(accessyear - maxyear < 2){
    ay <- ifelse(accessyear <= maxyear, maxyear, NA)
    py <- ifelse(lubridate::month(accessdate) < 4, accessyear - 1, NA)
  }
  if(py | ay){
    urep <- c(py, ay) %>% na.omit %>% paste(., collapse = " and ")
    urep <- paste(" - articles published in", urep, "may be underrepresented due to reporting lag")
  }else{
    urep <- NULL
  }
  return(urep)
}

