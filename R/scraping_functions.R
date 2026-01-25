
#' Gets all data needed to generate SEER Publication report
#'
#' Scrapes publication data from NCI, cancer statistics from CDC WONDER,
#' and incidence/prevalence/deaths data from SEER. Returns a combined
#' data object suitable for report generation.
#'
#' @param nci_url \code{character} URL used to fetch NCI publications data
#' @param cdc_wonder_request_xml \code{character} Path to CDC WONDER request XML file.
#' Use \code{system.file("extdata", "cdc_wonder_request.xml", package = "seermedicare", mustWork = TRUE)}
#' to load the bundled request template.
#' @param cdc_year \code{character} Year to request from CDC WONDER (single year)
#' @param cdc_cancer_stat_url \code{character} Path to zip file on cdc website. Base url is http://www.cdc.gov/cancer/npcr/uscs/ which contains multiple years of CDC cancer data
#' @param seer_cancer_stats_url \code{character} URL to Main SEER cancer fact site that contains links to each cancer
#'
#' @return Object of class \code{seermedicare} containing publications, seer, cdc, site_summary, and metadata
#'
#' @import rvest xml2 stringi
#' @importFrom stats na.omit setNames
#' @importFrom utils download.file packageVersion tail unzip
#' @examples
#' \dontrun{
#' xml_path <- system.file("extdata", "cdc_wonder_request.xml",
#'                         package = "seermedicare", mustWork = TRUE)
#' seer_pub_data <- scrape_all_data(cdc_wonder_request_xml = xml_path, cdc_year = "2022")
#' }
#' @export
scrape_all_data <- function(
  nci_url = "https://toolbox.imsweb.com/pubsearch-api/publications/hdrp/advanced/",
  cdc_wonder_request_xml = NULL,
  cdc_year = NULL,
  cdc_cancer_stat_url = "https://www.cdc.gov/cancer/uscs/USCS-1999-2017-ASCII.zip", # https://www.cdc.gov/cancer/uscs/dataviz/download_data.htm for all years
  seer_cancer_stats_url = "https://seer.cancer.gov/statfacts/"
) {

  message("Starting Process")
  if (!is.null(cdc_wonder_request_xml) && (is.null(cdc_year) || !nzchar(cdc_year))) {
    cdc_year <- .default_cdc_year()
    message(sprintf("CDC WONDER year not provided; using default %s", cdc_year))
  }

  ## SCRAPE PUBLICATIONS DATA
  message("Scape SEER-Medicare Publications from NCI")

  publications <- get_publication_data(nci_search_url = nci_url,
                                       table_results_css_selector = "table.pubsearch-results" )

  # ----- CANCER SITE STATISTICS -----
  ## Get CDC US CANCER STATISTICS BY AGE DATA
  message("Get CDC Incidence Rates")
  if (!is.null(cdc_wonder_request_xml)) {
    cdc <- get_cdc_data(wonder_request_xml = cdc_wonder_request_xml, year = cdc_year)
  } else {
    cdc <- get_cdc_data(pathToZip = cdc_cancer_stat_url)
  }

  # cdc year data contains column year that has single years plus one summary of the last 5 years
  if (is.null(cdc_year)) {
    cdc_year <- cdc[nchar(year) > 4, unique(year)]
    if (!length(cdc_year)) {
      cdc_year <- as.character(suppressWarnings(max(as.numeric(cdc$year), na.rm = TRUE)))
    }
  }

  ## Get SEER INCIDENCE, PREVALENCE, DEATHS Data
  message("Get SEER INCIDENCE, PREVALENCE, DEATHS Data")
  seer_url <- seer_cancer_stats_url
  seer <- get_seer_data(seer_url, link_css_selector = ".alphaList.row a")

  message("Map CDC and SEER site to NCI Publication Site")
  ## Get mapping of site to publications site and add site_std variable to cdc and seer data
  site_mapping <- create_std_site_lookup(publications, seer, cdc)

  ## Add publication standardized sites to cdc and seer data
  cdc[site_mapping, site_std := site_std, on=.(site)]
  seer[site_mapping, site_std := site_std, on=.(site)]

  message("Create Summary Data")
    # COMBINE PUBLICATIONS DATA, SEER DATA, AND CDC DATA TO CREATE CANCER SITE STATISTICS DATASET
  sites <- summarize_site_data(publications, seer, cdc, cdc_year = cdc_year)

  # ----- METADATA -----
  message("Generate Metadata")
  # incidence, prevalence, and death data years
  seeryears <- unique(seer[, c("x", "year")])
  metadata <- setNames(as.list(seeryears$year), paste0(seeryears$x, "_year"))

  # cdc age data years
  metadata[["cdc_year"]] <- cdc_year

  # citations
  if (!is.null(cdc_wonder_request_xml)) {
    metadata[["cdc_citation"]] <- sprintf("CDC WONDER request XML: %s", cdc_wonder_request_xml)
  } else {
    metadata[["cdc_citation"]] <- sprintf("Data collected from CDC website: %s", cdc_cancer_stat_url)
  }
  metadata[["seer_citation"]] <- session(seer_url) %>% session_follow_link("Cancer Statistics Review") %>% html_element("h2+ p") %>% html_text

  # data access
  if (.is_pubsearch_api_url(nci_url)) {
    metadata[["nci_url"]] <- "https://healthcaredelivery.cancer.gov/publications/"
  } else {
    metadata[["nci_url"]] <- nci_url
  }
  metadata[["access_date"]] <- Sys.Date()

  # rvest version
  metadata[["rvestvers"]] <- packageVersion("rvest")

  structure(
    list(
      publications = publications,
      seer = seer,
      cdc = cdc,
      site_summary = sites,
      metadata = metadata
    ),
    class = "seermedicare"
  )
}


#' Assemble seermedicare object from pre-fetched data components
#'
#' This function is useful for workflows using the targets package, where
#' each data source (publications, CDC, SEER) is cached independently.
#' It performs the same processing as \code{\link{scrape_all_data}} but accepts
#' pre-fetched data rather than fetching it directly.
#'
#' @param publications \code{data.table} from \code{\link{get_publication_data}}
#' @param cdc \code{data.table} from \code{\link{get_cdc_data}}
#' @param seer \code{data.table} from \code{\link{get_seer_data}}
#' @param cdc_year \code{character} Year used for CDC data. If NULL, will attempt
#'   to infer from the cdc data.
#' @param nci_url \code{character} URL for NCI publications (for metadata).
#'   Default is the public-facing publications page.
#' @param cdc_wonder_request_xml \code{character} Path to CDC WONDER XML file
#'   used to fetch data (for metadata/citation). If NULL, a generic citation is used.
#' @param seer_url \code{character} URL for SEER cancer statistics (for metadata).
#' @param seer_citation \code{character} Optional pre-fetched SEER citation. If NULL,
#'   the function will attempt to fetch it from the SEER website.
#'
#' @return Object of class \code{seermedicare} containing publications, seer, cdc,
#'   site_summary, and metadata
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # For use with targets package:
#' xml_path <- system.file("extdata", "cdc_wonder_request.xml",
#'                         package = "seermedicare", mustWork = TRUE)
#'
#' # Fetch data independently (these can be cached by targets)
#' publications <- get_publication_data()
#' cdc <- get_cdc_data(wonder_request_xml = xml_path, year = "2022")
#' seer <- get_seer_data()
#'
#' # Assemble into seermedicare object
#' seer_pub_data <- assemble_seermedicare(
#'   publications, cdc, seer,
#'   cdc_year = "2022",
#'   cdc_wonder_request_xml = xml_path
#' )
#'
#' # Generate report
#' generate_report(seer_pub_data, outputPath = "output")
#' }
assemble_seermedicare <- function(
    publications,
    cdc,
    seer,
    cdc_year = NULL,
    nci_url = "https://healthcaredelivery.cancer.gov/publications/",
    cdc_wonder_request_xml = NULL,
    seer_url = "https://seer.cancer.gov/statfacts/",
    seer_citation = NULL
) {

  message("Assembling seermedicare object from pre-fetched data")


  # Infer cdc_year from data if not provided
  if (is.null(cdc_year) || !nzchar(cdc_year)) {
    cdc_year <- cdc[nchar(year) > 4, unique(year)]
    if (!length(cdc_year)) {
      cdc_year <- as.character(suppressWarnings(max(as.numeric(cdc$year), na.rm = TRUE)))
    }
    message(sprintf("CDC year inferred from data: %s", cdc_year))
  }

  # Create site mapping
  message("Mapping CDC and SEER sites to publication sites")
  site_mapping <- create_std_site_lookup(publications, seer, cdc)

  # Add publication standardized sites to cdc and seer data
  # Make copies to avoid modifying the original cached data
  cdc <- data.table::copy(cdc)
  seer <- data.table::copy(seer)
  cdc[site_mapping, site_std := site_std, on = .(site)]
  seer[site_mapping, site_std := site_std, on = .(site)]

  # Create summary data
  message("Creating site summary data")
  sites <- summarize_site_data(publications, seer, cdc, cdc_year = cdc_year)

  # ----- METADATA -----
  message("Generating metadata")

  # Incidence, prevalence, and death data years from seer
  # Take the most common (mode) or max year for each measure type
  seeryears <- seer[!is.na(year), .(year = as.numeric(names(sort(table(year), decreasing = TRUE))[1])), by = x]
  metadata <- setNames(as.list(seeryears$year), paste0(seeryears$x, "_year"))

  # CDC age data year
  metadata[["cdc_year"]] <- cdc_year

  # Citations
  if (!is.null(cdc_wonder_request_xml)) {
    metadata[["cdc_citation"]] <- sprintf("CDC WONDER request XML: %s", basename(cdc_wonder_request_xml))
  } else {
    metadata[["cdc_citation"]] <- "Data collected from CDC cancer statistics"
  }

  # SEER citation - use provided or attempt to fetch
  if (!is.null(seer_citation)) {
    metadata[["seer_citation"]] <- seer_citation
  } else {
    metadata[["seer_citation"]] <- tryCatch({
      session(seer_url) %>%
        session_follow_link("Cancer Statistics Review") %>%
        html_element("h2+ p") %>%
        html_text()
    }, error = function(e) {
      warning("Could not fetch SEER citation from website: ", e$message)
      sprintf("SEER Cancer Statistics: %s", seer_url)
    })
  }

  # Data access info
  metadata[["nci_url"]] <- nci_url
  metadata[["access_date"]] <- Sys.Date()

  # rvest version
  metadata[["rvestvers"]] <- packageVersion("rvest")

  structure(
    list(
      publications = publications,
      seer = seer,
      cdc = cdc,
      site_summary = sites,
      metadata = metadata
    ),
    class = "seermedicare"
  )
}


#' Create lookup from publication site to SEER and CDC site
#'
#' Used to make sure a match between the SEER or CDC tumor site data and the publication data can be found.
#'
#' @param publications \code{data.table} publication data from running get_publication_data
#' @param seer \code{data.table} seer data from running get_seer_data
#' @param cdc \code{data.table} cdc data from running get_cdc_data
#'
#' @return \code{\link{data.table}}
create_std_site_lookup <- function(publications, seer, cdc) {
  unique_seer_sites <- seer[,.(site=unique(site), in_s=1)]
  unique_pub_sites <- publications[,.(site=unique(unlist(sites)),in_p=1)]
  unique_cdc_sites <- cdc[,.(site=unique(site),in_c=1)]

  sites <- merge( merge(unique_pub_sites,unique_seer_sites, by="site", all=T), unique_cdc_sites, all = T)

  # Create site_std and set all matches
  sites[in_p == 1 & in_s == 1 & in_c, site_std := site]

  # Set all Leukemia cancers to Leukemia
  sites[grepl("Leukemia", site), site_std := "Leukemia"]

  # Set Melanoma of the Skin to Skin
  sites[grepl("Skin", site), site_std := "Skin"]

  # Set all Lymphoma's (Hodgkin and Non-Hodgkin) together
  sites[grepl("Lymphoma", site), site_std := "Lymphoma"]

  # Set Breast
  sites[grepl("Breast", site), site_std := "Breast"]

  # Set Brain
  sites[grepl("Brain", site), site_std := "Brain & Other Nervous System"]

  # Set Brain
  sites[grepl("Colo", site), site_std := "Colorectal"]

  # Set Uterus
  sites[site %in% c("Uterus", "Corpus Uteri", "Corpus and Uterus, NOS"), site_std := "Corpus Uteri"]

  # Set Lung (Add Mesothelioma from CDC)
  sites[grepl("Lung", site) | site == "Mesothelioma", site_std := "Lung"]

  # Set Renal
  sites[grepl("Renal", site), site_std := "Renal"]

  # Set Gastric
  sites[site %in% c("Gastric", "Stomach"), site_std := "Gastric"]

  # Set Liver
  sites[grepl("Liver", site), site_std := "Liver"]

  # Set Cervical
  sites[grepl("Cervi", site), site_std := "Cervical"]

  # Set Bladder
  sites[grepl("Bladder", site), site_std := "Urinary Bladder"]

  # Set Head and Neck (Added Tongue and Larynx based on https://www.cancer.gov/types/head-and-neck/head-neck-fact-sheet)
  sites[site %in% c("Head and Neck", "Oral Cavity and Pharynx", "Tongue", "Larynx"), site_std := "Head and Neck"]

  # Set Myeloma
  sites[grepl("Myeloma", site), site_std := "Multiple Myeloma"]

  # All other site_std get set to site
  sites[is.na(site_std), site_std := site]

  sites
}

#' Get CDC United States Cancer Statistics Data: All Data Combined (Mortality and Incidence Rates)
#'
#' Downloads and unzips pathToZip file from CDC website returns data.table version of BYAGE.TXT file which contains mortality and incidence rates by age group, race, sex, site, and year.
#' Alternatively, uses a CDC WONDER request XML to fetch incidence counts by cancer site and age group for a single year.
#'
#' see https://www.cdc.gov/cancer/npcr/uscs/download_data.htm which has links to all historical data
#'
#' @param wonder_request_xml \code{character} Path to CDC WONDER request XML file
#' @param year \code{character} Year to request from CDC WONDER (single year)
#' @param pathToZip \code{character} Path to zip file on cdc website
#'
#' @return \code{\link{data.table}}
#' @import httr
#' @export
#' @examples
#' \dontrun{
#' xml_path <- system.file("extdata", "cdc_wonder_request.xml",
#'                         package = "seermedicare", mustWork = TRUE)
#' cdc <- get_cdc_data(wonder_request_xml = xml_path, year = "2022")
#' }
get_cdc_data <- function(wonder_request_xml = NULL, year = NULL, pathToZip = "http://www.cdc.gov/cancer/npcr/uscs/USCS_1999_2014_ASCII.zip") {
  if (!is.null(wonder_request_xml)) {
    if (is.null(year) || !nzchar(year)) {
      year <- .default_cdc_year()
      message(sprintf("CDC WONDER year not provided; using default %s", year))
    }
    params <- .read_wonder_request_xml(wonder_request_xml, year = year, export_format = "csv")
    csv_text <- .fetch_wonder_csv(params, url = "https://wonder.cdc.gov/controller/datarequest/D205")
    year_value <- params[["V_D205.V1"]][1]
    return(.parse_wonder_csv(csv_text, year = year_value))
  }

  if (is.null(pathToZip)) {
    stop("Either wonder_request_xml or pathToZip must be provided.")
  }

  zipfile <- pathToZip
  tf <- tempfile()
  download.file(zipfile, tf)
  x <- unzip(tf, exdir = tempdir())
  # Get BYAGE.TXT
  byage_file <- x[grepl(pattern = "/BYAGE.TXT", x)]
  cdc <- fread(byage_file, verbose = TRUE, na.strings = c("~", ".")) %>% setnames(tolower(colnames(.)))
  unlink(x) # removes the tempfiles

  cdc
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

.is_pubsearch_api_url <- function(url) {

  grepl("pubsearch-api", url, fixed = TRUE)
}

# Mapping of publication API site codes to standardized full names
# These codes are returned by the NCI publications API and need to be
# converted to match SEER and CDC site naming conventions.
# The values here must match the site_std values set in create_std_site_lookup().
.pub_site_code_map <- c(
  "BONS" = "Brain & Other Nervous System",
  "BREAST" = "Breast",
  "CERVICAL" = "Cervical",
  "COLO" = "Colorectal",
  "CU" = "Corpus Uteri",
  "ESO" = "Esophagus",
  "GASTRIC" = "Gastric",
  "HEADNECK" = "Head and Neck",
  "LEU" = "Leukemia",
  "LIVER" = "Liver",
  "LUNG" = "Lung",
  "LYMPHOMA" = "Lymphoma",
  "MM" = "Multiple Myeloma",
  "OVARY" = "Ovary",
  "PANCREAS" = "Pancreas",
  "PROSTATE" = "Prostate",
  "RENAL" = "Renal",
  "SKIN" = "Skin",
  "TESTIS" = "Testis",
  "UB" = "Urinary Bladder",
  "OTHER" = "Not Specified"
)

#' Map publication site codes to full names
#'
#' Converts abbreviated site codes from the NCI publications API
#' to full names that match SEER and CDC naming conventions.
#'
#' @param codes Character vector of site codes
#' @return Character vector of full site names
#' @keywords internal
.map_pub_site_codes <- function(codes) {
  # Handle NA or empty
  if (is.null(codes) || length(codes) == 0) {
    return(codes)
  }
  
  # Map each code to its full name, keeping original if not found
  vapply(codes, function(code) {
    code_upper <- toupper(trimws(code))
    if (code_upper %in% names(.pub_site_code_map)) {
      .pub_site_code_map[[code_upper]]
    } else {
      # Keep original (title case) if not in mapping
      tools::toTitleCase(tolower(code))
    }
  }, character(1), USE.NAMES = FALSE)
}

.fetch_pubsearch_publications <- function(api_url, data_source, auth_token, batch_size = 1000) {
  token <- auth_token
  if (is.null(token) || !nzchar(token)) {
    message("Fetching publications API token")
    token <- .fetch_pubsearch_token()
  }

  b_start <- 0
  numresults <- NULL
  publications <- list()

  repeat {
    params <- list(
      data_source = data_source,
      b_start = b_start,
      b_size = batch_size,
      sort_on = "date_author"
    )
    resp <- .pubsearch_get(api_url, token, params)
    status <- httr::status_code(resp)
    if (status %in% c(401, 403)) {
      message("Refreshing publications API token")
      token <- .fetch_pubsearch_token()
      resp <- .pubsearch_get(api_url, token, params)
      httr::stop_for_status(resp)
    } else {
      httr::stop_for_status(resp)
    }
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)

    if (is.null(numresults)) {
      numresults <- payload$metadata$numresults
    }

    batch <- payload$publications
    if (is.null(batch) || !length(batch)) {
      break
    }
    publications <- c(publications, batch)
    b_start <- b_start + length(batch)

    if (!is.null(numresults) && b_start >= numresults) {
      break
    }
  }

  .normalize_pubsearch_publications(publications)
}

.pubsearch_get <- function(api_url, token, params) {
  headers <- httr::add_headers(
    Authorization = token,
    `User-Agent` = "Mozilla/5.0",
    Accept = "application/json",
    Referer = "https://healthcaredelivery.cancer.gov/publications/"
  )
  httr::GET(api_url, headers, query = params)
}

.fetch_pubsearch_token <- function() {
  main_page <- httr::GET("https://healthcaredelivery.cancer.gov/publications/")
  httr::stop_for_status(main_page)
  html <- httr::content(main_page, as = "text", encoding = "UTF-8")

  script_paths <- regmatches(html, gregexpr("/vue-apps/assets/main-[^\"']+\\.js", html))
  if (!length(script_paths[[1]])) {
    stop("Could not locate publications main JS bundle to extract token.")
  }
  script_url <- paste0("https://healthcaredelivery.cancer.gov", script_paths[[1]][1])

  script_resp <- httr::GET(script_url)
  httr::stop_for_status(script_resp)
  js <- httr::content(script_resp, as = "text", encoding = "UTF-8")

  token_match <- regmatches(js, regexpr("Token [a-f0-9]{40}", js))
  if (!length(token_match) || !nzchar(token_match)) {
    stop("Could not extract publications API token from JS bundle.")
  }
  token_match
}

.normalize_pubsearch_publications <- function(publications) {
  collapse_field <- function(value, sep = "; ") {
    if (is.null(value)) {
      return(NA_character_)
    }
    if (is.list(value)) {
      value <- unlist(value, use.names = FALSE)
    }
    value <- value[!is.na(value) & nzchar(value)]
    if (!length(value)) {
      return(NA_character_)
    }
    paste(value, collapse = sep)
  }

  build_link <- function(pub) {
    if (!is.null(pub$pmid) && nzchar(pub$pmid)) {
      return(paste0("https://pubmed.ncbi.nlm.nih.gov/", pub$pmid, "/"))
    }
    if (!is.null(pub$weburl) && nzchar(pub$weburl)) {
      return(pub$weburl)
    }
    if (!is.null(pub$linktofulltext) && nzchar(pub$linktofulltext)) {
      return(pub$linktofulltext)
    }
    if (!is.null(pub$linktopdf) && nzchar(pub$linktopdf)) {
      return(pub$linktopdf)
    }
    NA_character_
  }

  data.table::rbindlist(lapply(publications, function(pub) {
    authors_val <- collapse_field(pub$authors_formatted, sep = ", ")
    if (is.na(authors_val)) {
      authors_val <- collapse_field(pub$authors, sep = ", ")
    }
    topics_val <- collapse_field(pub$medicare_topics, sep = "; ")
    if (is.na(topics_val)) {
      topics_val <- "Unknown"
    }
    sites_val <- collapse_field(pub$tumorsite, sep = "; ")

    list(
      authors = authors_val,
      date = pub$pubdate,
      journal = pub$journal,
      topics = topics_val,
      sites = sites_val,
      link = build_link(pub),
      title = pub$Title,
      pmid = pub$pmid
    )
  }), fill = TRUE)
}

#' Get SEER Medicare based Publications data
#'
#' Searches https://healthcaredelivery.cancer.gov/publications/ for SEER-CMS Linkages publications
#' and returns data.table with information about each publication
#'
#' @param nci_search_url \code{character} URL that searches NCI publications
#' @param table_results_css_selector \code{character} CSS selector to select main result table
#' @param data_source \code{character} Data source filter (e.g. "SEERM" for SEER-Medicare)
#' @param auth_token \code{character} Authorization token for the publications API.
#' If NULL, the token will be fetched from the publications site.
#' @param batch_size \code{numeric} Number of records to request per batch
#'
#' @importFrom tools toTitleCase
#'
#' @return \code{\link{data.table}}
#' @export
get_publication_data <- function(
  nci_search_url = "https://toolbox.imsweb.com/pubsearch-api/publications/hdrp/advanced/",
  table_results_css_selector = "table.pubsearch-results",
  data_source = "SEERM",
  auth_token = NULL,
  batch_size = 1000
) {
  if (.is_pubsearch_api_url(nci_search_url)) {
    publications <- .fetch_pubsearch_publications(
      api_url = nci_search_url,
      data_source = data_source,
      auth_token = auth_token,
      batch_size = batch_size
    )
    # Continue to formatting section below
  } else {

  # Functions to parse publication data results
  parserow <- function(row) {
    setNames(row %>% html_elements("td") %>% html_text(trim = T) %>% .[length(.)], # extract table data ("td") into vector and take last (some return more than 1 row)
             row %>% html_element("th") %>% html_text(trim = T) %>% tolower %>% gsub(":", "", .)) # extract table headings ("th") and use for vector names
  }

  # get href for pubmed id link
  parselink <- function(table) {
    setNames(table %>% html_elements("tr:last-child a:nth-child(1)") %>% html_attr("href", default = "NR"), "link")
  }

  parserows <- function(table) {
    rows <- table %>% html_elements("tr") %>% .[-c(9,10)] # take table and parse into rows ("tr") (exclude abstract links)
    
    data <- sapply(rows, parserow)
    link <- tryCatch( parselink(table), error = function(e) {
      return(NA_character_)
    })
    df <- c(data, link) %>% as.list %>% data.frame(check.names = F, stringsAsFactors = FALSE) # put rows into dataframe
  }

  parsetables <- function(tables) {
    rbindlist(lapply(tables, parserows), use.names = T, fill = T)
  }


  # extract each table on webpage, parse, and combine to create full dataset
  nci_url <- nci_search_url

    publications <- read_html(nci_url) %>% html_elements(table_results_css_selector) %>% parsetables
  }

  ## FORMAT PUBLICATIONS DATA

  # clean column names (only for HTML-scraped data which has "tumor sites")
  if ("tumor sites" %in% names(publications)) {
    setnames(publications, sub("(s)", "s", names(publications), fixed = T)) %>% setnames("tumor sites", "sites")
  }

  # set blank cells to NA and remove rows with any critical fields missing - note that reports are dropped
  publications <- publications[, lapply(.SD, function(x){ifelse(x == "", NA, x)})] %>%
    na.omit(cols = c("authors", "date", "journal", "topics", "sites"))

  # create and clean columns
  generateids <- function(n) {
    sprintf(paste0("x%0", nchar(n), "d"), 1:n)
  }

  publications[,`:=`(id = generateids(nrow(publications)), # new column -  unique identifiers
                     year = stringi::stri_extract_first_regex(date, "[[:digit:]]{4}"), # new column - publication year only
                     issue = journal, # new column - journal with volume and issue (because journal will be modified)
                     journal = stringi::stri_extract_all_regex(journal, "[[:alpha:]]+") %>% sapply(paste, collapse = " "))] %>% setkey(id)
  # Note: sites are converted to full names via .map_pub_site_codes after listify

  # for columns with multiple pieces of data per cell, split pieces and convert to list
  listify <- function(col, split = ";", data = publications, removeItems = "") {
    data[, (col) := lapply(.SD, function(x, pattern, removeItems) {
      res <- stringi::stri_split_regex(x,pattern)

      if(removeItems != "") {
        res <- lapply( res, function(x) {
          x[!(x %in% removeItems)]
        })
      }
      res
    }, paste0("[[:blank:]]*", split, "[[:blank:]]*"), removeItems), .SDcols = col]
  }

  listify("authors", split = ",")
  listify("sites", removeItems = "All Sites or No Specific Site") # Remove All Sites from Sites

  # Map publication site codes to full names (for API data which uses abbreviated codes)
  publications[, sites := lapply(sites, .map_pub_site_codes)]

  # Convert "Other" to "Not Specified" for publications without specific cancer types
  publications[, sites := lapply(sites, function(x) {
    x[x == "Other"] <- "Not Specified"
    x
  })]

  # For publications with no site information, assign "Not Specified"
  publications[, sites := lapply(sites, function(x) {
    if (length(x) == 0) "Not Specified" else x
  })]

  # for uniform formatting, remove author middle initials unless they distinguish two authors with same first and last name
  remove_middle_init <- function(x) {
    sub("[[:alpha:]]$", "", x)
  }

  authorref <- setNames(unlist(publications$authors), unlist(publications$authors)) # all authors
  to_remove_middle <- grep("[[:alpha:]]+ [[:alpha:]]{2}$", authorref, value = T) %>% unique %>% .[!remove_middle_init(.) %>% duplicated] # authors with middle initial listed that have unique first initials and last names (middle initial can be removed)
  authorref[to_remove_middle] <- remove_middle_init(to_remove_middle) # remove middle initials from appropriate authors
  publications$authors <- lapply(publications$authors, function(x){authorref[x]})

  # Remove report and NA_character column
  if("report" %in% names(publications)) publications[,report := NULL]
  if("NA_character_" %in% names(publications)) publications[,`NA_character_`  := NULL]
  
  publications
}


#' Get SEER Incidence, Prevalence, and Mortality Data
#'
#' Loops through each link at http://seer.cancer.gov/statfacts/more.html, which contains more information about different cancer sites.
#'
#'  Returns data set with a column for site, year, x (A categorical variable defining what is in the value column. Can be "incidence", "prevalence", or "deaths"), value ( the number associated with the measure in x)
#'
#' @param seer_url \code{character} Url to Main SEER cancer fact site that contains links to each cancer
#' @param link_css_selector \code{character} CSS selector to select all links to each cancer site
#' @param delay_seconds \code{numeric} Delay between requests in seconds
#' @param retries \code{numeric} Number of retries for HTTP fetches
#' @param user_agent \code{character} User agent string for HTTP requests
#'
#' @return \code{\link{data.table}}
#' @export
get_seer_data <- function(
  seer_url = "https://seer.cancer.gov/statfacts/",
  link_css_selector = ".alphaList.row a",
  delay_seconds = 0.2,
  retries = 3,
  user_agent = "Mozilla/5.0"
) {

  # get hyperlinks for each cancer type from seer statfacts homepage
  seer_index <- .seer_fetch_html(seer_url, retries = retries, user_agent = user_agent)
  links <- seer_index %>% html_elements(link_css_selector)
  if (!length(links)) {
    warning("No SEER site links found. Check link_css_selector or page structure.")
  }

  # get data from each cancer site page
  res <- list()
  for( link in links ) {
    link_text <- html_text(link)

    # Don't pull in All Cancers data
    if(link_text != "All Cancers") {
      message("Getting Data For...", link_text)
      res[[link_text]] <- tryCatch(
        parse_seer_cancer_fact_page(
          link,
          retries = retries,
          user_agent = user_agent
        ),
        error = function(e) {
          warning(sprintf("Failed to parse SEER page for %s: %s", link_text, e$message))
          data.table(site = character(0), year = numeric(0), x = character(0), value = numeric(0))
        }
      )
      if (delay_seconds > 0) {
        Sys.sleep(delay_seconds)
      }
    }
  }
  seer <- rbindlist(res, use.names = T, fill = T)
  .seer_scrape_summary(seer)
  seer
}

#' Parse SEER Cancer Fact Page to get Latest Incidence, Prevalence, and Mortality Data
#'
#' Takes link to SEER site specific cancer stat fact page and returns data set with a column for site, year, x (A categorical variable defining what is in the value column. Can be "incidence", "prevalence", or "deaths"), value ( the number associated with the measure in x)
#'
#' @param link \code{character} URL to site specific SEER cancer fact site
#' @param retries \code{numeric} Number of retries for HTTP fetches (default 3)
#' @param user_agent \code{character} User agent string for HTTP requests
#'
#' @return \code{\link{data.table}}
parse_seer_cancer_fact_page <- function(link, retries = 3, user_agent = "Mozilla/5.0") {
  page <- .seer_fetch_html(paste0("https://seer.cancer.gov", html_attr(link, "href")), retries = retries, user_agent = user_agent)
  site = html_text(link)

  year_node <- .seer_extract_text(page, c(
    ".glanceBox.new p",
    ".glanceBox p",
    ".glanceBox .year"
  ))

  if(!is.na(year_node)) {

    # Get year
    year <- as.numeric(stringi::stri_extract_first_regex(year_node, "[[:digit:]]{4}"))

    # Get incidence and deaths from table
    incidence <- .seer_extract_numeric(page, c(
      ".glance-factSheet .glanceBox.new",
      ".glanceBox.new",
      ".glanceBox.incidence"
    ))

    deaths <- .seer_extract_numeric(page, c(
      ".glance-factSheet .glanceBox.death",
      ".glanceBox.death",
      ".glanceBox.mortality"
    ))

    # Get prevalence from main text - search all paragraphs for "Prevalence" or "living with"
    prevalence <- .seer_extract_prevalence_from_page(page)

    dt <- data.table(site=site, year = year,
                     x = c("incidence", "prevalence", "deaths"),
                     value = c(incidence, prevalence, deaths))
  } else {
    warning("Could not parse page for ", site)
    dt <- data.table(site=character(0), year = numeric(0),
                     x = character(0),
                     value = numeric(0))
  }

  return(dt)
}

.seer_fetch_html <- function(url, retries = 3, user_agent = "Mozilla/5.0") {
  attempt <- 1
  repeat {
    resp <- httr::GET(url, httr::add_headers(`User-Agent` = user_agent))
    if (httr::status_code(resp) == 200) {
      html <- httr::content(resp, as = "text", encoding = "UTF-8")
      return(xml2::read_html(html))
    }
    if (attempt >= retries) {
      httr::stop_for_status(resp)
    }
    Sys.sleep(0.5 * attempt)
    attempt <- attempt + 1
  }
}

.seer_extract_text <- function(page, selectors) {
  for (selector in selectors) {
    node <- html_element(page, selector)
    if (!is.null(node)) {
      text <- html_text(node)
      if (!is.na(text) && nzchar(trimws(text))) {
        return(text)
      }
    }
  }
  NA_character_
}

.seer_extract_numeric <- function(page, selectors) {
 for (selector in selectors) {
    node <- html_element(page, selector)
    if (!is.null(node)) {
      # Look for the first <p> containing "Estimated" to get the count row
      p_nodes <- html_elements(node, "p")
      for (p_node in p_nodes) {
        p_text <- html_text(p_node)
        if (grepl("Estimated", p_text, ignore.case = TRUE)) {
          # Extract just from this paragraph
          numbers <- stringi::stri_extract_all_regex(p_text, "[0-9][0-9,]*")[[1]]
          if (length(numbers) >= 2) {
            # First number is year, second is the count
            count_str <- numbers[2]
            return(as.numeric(gsub(",", "", count_str)))
          }
        }
      }
      
      # Fallback: try to find a large number (count with comma) in the full text
      text <- html_text(node)
      # Look for numbers with commas (likely counts, not percentages)
      large_numbers <- stringi::stri_extract_all_regex(text, "[0-9]{1,3}(?:,[0-9]{3})+")[[1]]
      if (length(large_numbers)) {
        return(as.numeric(gsub(",", "", large_numbers[1])))
      }
      
      # Final fallback: original logic
      numbers <- stringi::stri_extract_all_regex(text, "[0-9][0-9,]*")[[1]]
      if (length(numbers)) {
        # Filter out likely years (4 digits starting with 19 or 20) and small percentages
        counts <- numbers[!grepl("^(19|20)\\d{2}$", numbers) & nchar(gsub(",", "", numbers)) > 2]
        if (length(counts)) {
          return(as.numeric(gsub(",", "", counts[1])))
        }
      }
    }
  }
  NA_real_
}

.seer_extract_prevalence_from_page <- function(page) {
  # Search all paragraphs for prevalence-related text
  p_nodes <- html_elements(page, ".factSheet p, .tog-content p")
  
  for (p_node in p_nodes) {
    text <- html_text(p_node)
    # Look for paragraphs mentioning prevalence or "living with"
    if (grepl("Prevalence|living with", text, ignore.case = TRUE)) {
      # Extract large numbers (with commas) - these are the counts
      large_numbers <- stringi::stri_extract_all_regex(text, "[0-9]{1,3}(?:,[0-9]{3})+")[[1]]
      if (length(large_numbers)) {
        return(as.numeric(gsub(",", "", large_numbers[1])))
      }
      # Fallback: look for "estimated N" pattern
      match <- stringi::stri_match_first_regex(text, "(?i)estimated\\s+([0-9][0-9,]*)")
      if (!is.na(match[, 2])) {
        return(as.numeric(gsub(",", "", match[, 2])))
      }
    }
  }
  
  # Final fallback: original approach with selectors
  main_selector <- ".factSheet .tog-content div.row p:nth-child(3)"
  non_inner_row_selector <- ".factSheet .tog-content .glance-factSheet ~ p:nth-of-type(3)"
  prev_text <- .seer_extract_text(page, c(
    main_selector,
    non_inner_row_selector,
    ".factSheet .tog-content p"
  ))
  .seer_extract_prevalence(prev_text)
}

.seer_extract_prevalence <- function(text) {
  if (is.na(text) || !nzchar(text)) {
    return(NA_real_)
  }
  match <- stringi::stri_match_first_regex(text, "(?i)estimated\\s+([0-9][0-9,]*)")
  if (!is.na(match[, 2])) {
    return(as.numeric(gsub(",", "", match[, 2])))
  }
  # Look for large numbers with commas
  large_numbers <- stringi::stri_extract_all_regex(text, "[0-9]{1,3}(?:,[0-9]{3})+")[[1]]
  if (length(large_numbers)) {
    return(as.numeric(gsub(",", "", large_numbers[1])))
  }
  fallback <- stringi::stri_extract_first_regex(text, "[0-9][0-9,]*")
  if (!is.na(fallback)) {
    return(as.numeric(gsub(",", "", fallback)))
  }
  NA_real_
}

.seer_scrape_summary <- function(seer) {
  if (nrow(seer) == 0) {
    warning("SEER scrape returned no data.")
    return(invisible(NULL))
  }
  missing <- seer[is.na(value), .N, by = x]
  if (nrow(missing)) {
    msg <- paste(sprintf("%s: %d missing", missing$x, missing$N), collapse = "; ")
    message("SEER scrape missing values - ", msg)
  }
  invisible(NULL)
}

#' Summarize CDC incidence data by standardized cancer site
#'
#'
#' @param cdc \code{data.table} from running get_cdc_data and creating site_std variable which is the site standardized to publication site
#' @param cdc_year \code{character} The cdc data has results for each individual year and a summary of the last 4 years in the form (YYYY-YYYY). As new data comes in this variable will need to be udpated to match the newest 4 year summary.
#'
#' @return \code{\link{data.table}}
#' @export
summarize_cdc_incidence_data_by_site <- function(cdc, cdc_year = "2010-2014") {

  cdc <- cdc[year == cdc_year & event_type == "Incidence" & race == "All Races" & sex == "Male and Female"]

  # add flag for age >= 65 years
  cdc[, ("min65") := ifelse(age %in% c("65-69", "70-74", "75-79", "80-84", "85+"), 1, 0)]

  # count incident patients 65+ for each site
  cdc_count_by_site_all <- cdc[, .(allages = sum(count, na.rm = T)), by = .(site=site_std)] # Gets number of people by site for all ages
  cdc_count_by_site_gt65 <- cdc[min65 == 1, .(min65 = sum(count, na.rm = T)), by = .(site=site_std)]
  cdc <- merge(cdc_count_by_site_all, cdc_count_by_site_gt65, by = "site", all = T)

  cdc
}

#' Summarize SEER data by standardized cancer site
#'
#' Returns summarized counts for each measure
#'
#' @param seer \code{data.table} from running get_seer_data and creating site_std variable which is the site standardized to publication site
#'
#' @return \code{\link{data.table}} Summarized seer data by measure ("incidence", "prevalence", "deaths"), site, and year
#' @export
summarize_seer_data_by_site <- function(seer) {
  seer[!is.na(site), .(value = sum(value)), by = .(x, site=site_std, year)]
}

#' Summarize Site Data from Publication, CDC, and SEER
#'
#' @param publications \code{data.table} publication data from running get_publication_data
#' @param seer \code{data.table} seer data from running get_seer_data
#' @param cdc \code{data.table} cdc data from running get_cdc_data
#' @param cdc_year \code{character} The cdc data has results for each individual year and a summary of the last 4 years in the form (YYYY-YYYY). As new data comes in this variable will need to be udpated to match the newest 4 year summary. Default gets the year from the cdc data that has more then 4 characters (all other years are single year YYYY)
#'
#' @return \code{\link{data.table}} Summarized seer data by measure ("incidence", "prevalence", "deaths"), site, and year
#' @export
summarize_site_data <- function(publications, seer, cdc, cdc_year = NULL) {
  if (is.null(cdc_year)) {
    cdc_year <- cdc[nchar(year) > 4, unique(year)]
    if (!length(cdc_year)) {
      cdc_year <- as.character(suppressWarnings(max(as.numeric(cdc$year), na.rm = TRUE)))
    }
  }

  seer_summary <- summarize_seer_data_by_site(seer)
  cdc_incidence <- summarize_cdc_incidence_data_by_site(cdc, cdc_year)

  # COMBINE PUBLICATIONS DATA, SEER DATA, AND CDC DATA TO CREATE CANCER SITE STATISTICS DATASET
  # Count publications by site (replaces plyr::count)
  pub_sites <- unlist(publications$sites)
  pub_counts <- data.table(site = pub_sites)[, .(publications = .N), by = site]
  
  sites <- data.table(Reduce(function(...) merge(..., by = "site", all = T),
                             list(pub_counts, cdc_incidence, dcast(seer_summary, site ~ x, value.var = "value")))) %>% setorder(-publications)
  sites[, `:=`(site = factor(site, levels = site))]
  sites <- sites[, lapply(.SD, as.numeric), by = site] %>% na.omit
}
