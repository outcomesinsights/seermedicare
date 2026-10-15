
# CDC WONDER U.S. Cancer Statistics incidence dataset the request XML is written for.
# Each new CDC release gets a new dataset ID and its own parameter names, so moving
# to a newer release means rebuilding config/cdc_wonder_request.xml (see README).
.wonder_dataset <- list(id = "D205", release = 2022L)

#' Get CDC WONDER Cancer Incidence Counts by Site and Age Group
#'
#' Uses a CDC WONDER request XML to fetch incidence counts by cancer site and age group for a single year.
#'
#' @param wonder_request_xml \code{character} Path to CDC WONDER request XML file
#' @param year \code{character} Year to request from CDC WONDER (single year). If NULL, uses the most recent year WONDER has available.
#'
#' @return \code{\link{data.table}}
#' @examples
#' \dontrun{
#' cdc <- get_cdc_data("config/cdc_wonder_request.xml", year = "2022")
#' }
get_cdc_data <- function(wonder_request_xml, year = NULL) {
  if (is.null(year) || !nzchar(year)) {
    return(.fetch_wonder_latest_year(wonder_request_xml))
  }
  params <- .read_wonder_request_xml(wonder_request_xml, year = year, export_format = "csv")
  csv_text <- .fetch_wonder_csv(params, url = .wonder_url())
  .parse_wonder_csv(csv_text, year = params[["V_D205.V1"]][1])
}

.wonder_url <- function() {
  paste0("https://wonder.cdc.gov/controller/datarequest/", .wonder_dataset$id)
}

#' Latest U.S. Cancer Statistics release listed on CDC WONDER
#'
#' Reads the WONDER cancer landing page for links like "cancer-v2023.html".
#' Returns NA if the page can't be read, so callers can warn rather than fail.
latest_wonder_release <- function(url = "https://wonder.cdc.gov/cancer.html") {
  html <- tryCatch(
    httr::content(httr::GET(url), as = "text", encoding = "UTF-8"),
    error = function(e) NA_character_
  )
  if (is.na(html)) return(NA_integer_)
  years <- stringi::stri_match_all_regex(html, "cancer-v(\\d{4})\\.html")[[1]][, 2]
  years <- suppressWarnings(as.integer(years))
  if (all(is.na(years))) NA_integer_ else max(years, na.rm = TRUE)
}

.read_wonder_request_xml <- function(path, year = NULL, export_format = "csv") {
  if (is.null(path) || !nzchar(path)) {
    stop("wonder_request_xml must be provided when requesting CDC WONDER data.")
  }

  doc <- xml2::read_xml(path)
  params <- xml2::xml_find_all(doc, ".//parameter")
  names_vec <- xml2::xml_text(xml2::xml_find_first(params, "name"))
  values_list <- lapply(params, function(param) {
    vals <- xml2::xml_text(xml2::xml_find_all(param, "value"))
    trimws(vals)
  })

  # Use setNames to create named list directly (split doesn't handle lists of lists well)
  param_list <- setNames(values_list, names_vec)

  if (!is.null(year)) {
    param_list[["V_D205.V1"]] <- as.character(year)
  }

  param_list
}

.default_cdc_year <- function(current_date = Sys.Date(), update_month = 4, lag_years = 3) {
  year_val <- as.integer(format(current_date, "%Y"))
  month_val <- as.integer(format(current_date, "%m"))
  update_year <- if (month_val > update_month) year_val else (year_val - 1)
  as.character(update_year - lag_years)
}

# CDC WONDER releases lag the calendar unpredictably and reject years not yet
# in the dataset (HTTP 500), so start from the date-based guess and step back.
.fetch_wonder_latest_year <- function(wonder_request_xml, start_year = .default_cdc_year(), max_tries = 4) {
  candidates <- as.character(as.integer(start_year) - seq_len(max_tries) + 1)
  for (year in candidates) {
    params <- .read_wonder_request_xml(wonder_request_xml, year = year, export_format = "csv")
    csv_text <- tryCatch(
      .fetch_wonder_csv(params, url = .wonder_url()),
      error = function(e) {
        message(sprintf("CDC WONDER year %s not available (%s)", year, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(csv_text)) {
      message(sprintf("CDC WONDER year not provided; using latest available year %s", year))
      return(.parse_wonder_csv(csv_text, year = year))
    }
  }
  stop(sprintf("CDC WONDER returned no data for years %s.", paste(candidates, collapse = ", ")))
}

.fetch_wonder_csv <- function(params, url) {
  # Flatten params for form encoding: repeat parameter names for multi-value params
  flat_params <- list()
  for (name in names(params)) {
    vals <- params[[name]]
    for (v in vals) {
      flat_params <- c(flat_params, setNames(list(v), name))
    }
  }
  resp <- httr::POST(url, body = flat_params, encode = "form")
  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "latin1")
}

.parse_wonder_csv <- function(html_text, year) {
  if (!nzchar(html_text)) {
    stop("CDC WONDER response was empty.")
  }

  # Parse HTML and find the data table
  page <- xml2::read_html(html_text)
  tables <- rvest::html_table(page, fill = TRUE)
  
  if (!length(tables)) {
    stop("CDC WONDER response did not contain any tables.")
  }
  
  # Find the table with Cancer Sites, Age Groups, and Count columns
  # It's typically the largest table with data rows
  data_table <- NULL
  for (tbl in tables) {
    col_names <- tolower(trimws(as.character(names(tbl))))
    # Check if this looks like the data table (has cancer sites in column names or first row)
    if (nrow(tbl) > 100 && ncol(tbl) >= 3) {
      # Check first column values for cancer site names
      first_col <- as.character(tbl[[1]])
      if (any(grepl("Oral Cavity|Breast|Lung|Colon|Prostate", first_col, ignore.case = TRUE))) {
        data_table <- tbl
        break
      }
    }
  }
  
  if (is.null(data_table)) {
    stop("CDC WONDER response missing expected data table with cancer sites.")
  }
  
  # The table structure from CDC WONDER HTML has columns: Cancer Site, Age Group, Count
  # But may have extra/duplicate columns due to HTML structure
  # Take first 3 meaningful columns
  dt <- data.table::as.data.table(data_table)
  
  # Find columns with actual data
  # First column should be cancer sites, second age groups, third count
  site_col <- NULL
  age_col <- NULL
  count_col <- NULL
  
  for (i in seq_len(ncol(dt))) {
    col_vals <- as.character(dt[[i]])
    col_vals <- col_vals[!is.na(col_vals) & nzchar(col_vals)]
    
    if (is.null(site_col) && any(grepl("Oral Cavity|Breast|Lung|Colon", col_vals, ignore.case = TRUE))) {
      site_col <- i
    } else if (!is.null(site_col) && is.null(age_col) && any(grepl("years|\\d+-\\d+", col_vals, ignore.case = TRUE))) {
      age_col <- i
    } else if (!is.null(age_col) && is.null(count_col) && any(grepl("^\\d+$", col_vals))) {
      count_col <- i
      break
    }
  }
  
  if (is.null(site_col) || is.null(age_col) || is.null(count_col)) {
    stop("CDC WONDER response missing expected columns: Cancer Sites, Age Groups, Count.")
  }
  
  # Extract relevant columns
  dt <- dt[, c(site_col, age_col, count_col), with = FALSE]
  setnames(dt, c("site", "age", "count"))
  
  # Clean data - remove header rows and empty rows
  dt <- dt[!is.na(site) & nzchar(site)]
  dt <- dt[!grepl("^Cancer Sites$|^$", site, ignore.case = TRUE)]
  dt <- dt[!is.na(age) & nzchar(age)]
  
  dt[, age := gsub("\\s*years$", "", trimws(age))]
  dt[, `:=`(
    count = as.numeric(gsub(",", "", count)),
    year = as.character(year),
    event_type = "Incidence",
    race = "All Races",
    sex = "Male and Female"
  )]
  
  # Remove rows where count is NA (header rows, etc.)
  dt <- dt[!is.na(count)]

  dt
}
