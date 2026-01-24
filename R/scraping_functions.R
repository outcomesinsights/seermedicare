
#' Gets all data needed to generate SEER Publication report
#'
#' Returns
#'
#' @param nci_url \code{data.table} publication data from running get_publication_data
#' @param cdc_wonder_request_xml \code{character} Path to CDC WONDER request XML file
#' @param cdc_year \code{character} Year to request from CDC WONDER (single year)
#' @param cdc_cancer_stat_url \code{character} Path to zip file on cdc website. Base url is http://www.cdc.gov/cancer/npcr/uscs/ whic contains multiple years of CDC cancer  data
#' @param seer_cancer_stats_url \code{character} Url to Main SEER cancer fact site that contains links to each cancer
#'
#' @return \code{seermedicare} obj
#'
#' @import rvest xml2 stringi plyr
#' @export
scrape_all_data <- function(
  nci_url = "http://healthcaredelivery.cancer.gov/cgi-bin-pubsearch/pubsearch/index.pl?simple_search=&searchOpt=and&initiative=SEERM",
  cdc_wonder_request_xml = NULL,
  cdc_year = NULL,
  cdc_cancer_stat_url = "https://www.cdc.gov/cancer/uscs/USCS-1999-2017-ASCII.zip", # https://www.cdc.gov/cancer/uscs/dataviz/download_data.htm for all years
  seer_cancer_stats_url = "http://seer.cancer.gov/statfacts/more.html"
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
  seer <- get_seer_data(seer_url, link_css_selector = ".alphaList.rpw a")

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
  metadata[["seer_citation"]] <- html_session(seer_url) %>% follow_link("Cancer Statistics Review") %>% html_node("h2+ p") %>% html_text

  # data access
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

  param_list <- split(values_list, names_vec)
  param_list <- lapply(param_list, function(values) unlist(values, use.names = FALSE))

  if (!is.null(year)) {
    param_list[["V_D205.V1"]] <- as.character(year)
  }
  if (!is.null(export_format)) {
    param_list[["O_export-format"]] <- export_format
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
  resp <- httr::POST(url, body = params, encode = "form")
  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "UTF-8")
}

.parse_wonder_csv <- function(csv_text, year) {
  lines <- readLines(textConnection(csv_text))
  if (!length(lines)) {
    stop("CDC WONDER response was empty.")
  }

  cutoff <- which(grepl("^\"---\"", lines))
  if (length(cutoff)) {
    lines <- lines[1:(cutoff[1] - 1)]
  }
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    stop("CDC WONDER response did not contain tabular data.")
  }

  dt <- fread(text = paste(lines, collapse = "\n"))
  setnames(dt, trimws(names(dt)))

  if ("Notes" %in% names(dt)) {
    dt[, Notes := NULL]
  }

  if (!all(c("Cancer Sites", "Age Groups", "Count") %in% names(dt))) {
    stop("CDC WONDER response missing expected columns: Cancer Sites, Age Groups, Count.")
  }

  setnames(dt, c("Cancer Sites", "Age Groups", "Count"), c("site", "age", "count"))
  dt[, age := gsub("\\s*years$", "", trimws(age))]
  dt[, `:=`(
    count = as.numeric(count),
    year = as.character(year),
    event_type = "Incidence",
    race = "All Races",
    sex = "Male and Female"
  )]

  dt
}

#' Get SEER Medicare based Publications data
#'
#' Searches https://healthcaredelivery.cancer.gov/publications/ for all SEER-Medicare Publications (Data Source/Project/Initiative = "SEER-Medicare Linked Database) and returns data.table with information about each publication
#'
#' @param nci_search_url \code{character} URL that searches NCI publications for all SEER Medicare publications
#' @param table_results_css_selector \code{character} CSS selector to select main result table
#'
#' @importFrom tools toTitleCase
#'
#' @return \code{\link{data.table}}
get_publication_data <- function(nci_search_url = "http://healthcaredelivery.cancer.gov/cgi-bin-pubsearch/pubsearch/index.pl?simple_search=&searchOpt=and&initiative=SEERM", table_results_css_selector = "table.pubsearch-results" ) {

  # Functions to parse publication data results
  parserow <- function(row) {
    setNames(row %>% html_nodes("td") %>% html_text(trim = T) %>% .[length(.)], # extract table data ("td") into vector and take last (some return more than 1 row)
             row %>% html_node("th") %>% html_text(trim = T) %>% tolower %>% gsub(":", "", .)) # extract table headings ("th") and use for vector names
  }

  # get href for pubmed id link
  parselink <- function(table) {
    setNames(table %>% html_nodes("tr:last-child a:nth-child(1)") %>% html_attr("href", default = "NR"), "link")
  }

  parserows <- function(table) {
    rows <- table %>% html_nodes("tr") %>% .[-c(9,10)] # take table and parse into rows ("tr") (exclude abstract links)
    
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

  publications <- read_html(nci_url) %>% html_nodes(table_results_css_selector) %>% parsetables

  ## FORMAT PUBLICATIONS DATA

  # clean column names
  setnames(publications, sub("(s)", "s", names(publications), fixed = T)) %>% setnames("tumor sites", "sites")

  # set blank cells to NA and remove rows with any critical fields missing - note that reports are dropped
  publications <- publications[, lapply(.SD, function(x){ifelse(x == "", NA, x)})] %>% na.omit(cols = c("authors", "date", "journal", "topics", "sites"))

  # create and clean columns
  generateids <- function(n) {
    sprintf(paste0("x%0", nchar(n), "d"), 1:n)
  }

  publications[,`:=`(id = generateids(nrow(publications)), # new column -  unique identifiers
                     year = stringi::stri_extract_first_regex(date, "[[:digit:]]{4}"), # new column - publication year only
                     issue = journal, # new column - journal with volume and issue (because journal will be modified)
                     journal = stringi::stri_extract_all_regex(journal, "[[:alpha:]]+") %>% sapply(paste, collapse = " "), # change journal column to include name only
                     sites = toTitleCase(sites))] %>% setkey(id)

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

  # Remove journals with no site information
  publications <- publications[sapply(sites, function(x) length(x)!=0)]

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
#'
#' @return \code{\link{data.table}}
get_seer_data <- function(seer_url = "http://seer.cancer.gov/statfacts/more.html", link_css_selector = ".alphaList.rpw a") {

  # get hyperlinks for each cancer type from seer statfacts homepage
  links <- read_html(seer_url) %>% html_nodes(link_css_selector)

  # get data from each cancer site page
  res <- list()
  for( link in links ) {
    link_text <- html_text(link)

    # Don't pull in All Cancers data
    if(link_text != "All Cancers") {
      message("Getting Data For...", link_text)
      res[[link_text]] <- parse_seer_cancer_fact_page(link)
    }
  }
  seer <- rbindlist(res, use.names = T, fill = T)
}

#' Parse SEER Cancer Fact Page to get Latest Incidence, Prevalence, and Mortality Data
#'
#' Takes link to SEER site specific cancer stat fact page and returns data set with a column for site, year, x (A categorical variable defining what is in the value column. Can be "incidence", "prevalence", or "deaths"), value ( the number associated with the measure in x)
#'
#' @param link \code{character} Url to site specific SEER cancer fact site
#'
#' @return \code{\link{data.table}}
parse_seer_cancer_fact_page <- function(link) {
  page <- read_html(paste0("https://seer.cancer.gov", html_attr(link, "href")))
  site = html_text(link)

  year_node <- html_text(html_node(page, ".glanceBox.new p" ))

  if(!is.na(year_node)) {

    # Get year
    year <- as.numeric( stringi::stri_extract_first_regex(year_node, "[[:digit:]]{4}") )

    # Get incidence and deaths from table
    inc_row <- html_node(page, ".glance-factSheet .glanceBox.new")
    incidence <- as.numeric(gsub(",","",html_text( html_node(inc_row,"p:nth-child(1) span:nth-child(2)" ))))
    
    death_row <- html_node(page, ".glance-factSheet .glanceBox.death")
    deaths <- as.numeric(gsub(",","",html_text( html_node(death_row,"p:nth-child(1) span:nth-child(2)" ))))

    # Get prevalence from main text
    ## Main selector
    main_selector <- ".factSheet .tog-content div.row p:nth-child(3)"
    ## Some pages dont have an inner div.row
    non_inner_row_selector <- ".factSheet .tog-content .glance-factSheet ~ p:nth-of-type(3)"
    prev_text <- html_text(html_node(page, paste(c(main_selector, non_inner_row_selector), collapse = ",")))
    
    if(is.na(prev_text)) {
      prevalence <- NA_integer_
    }else {
      prevalence <- as.numeric(gsub(",","",stri_match_all_regex(prev_text,"estimated (\\S+) " )[[1]][,2]))
    }

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
  sites <- data.table(Reduce(function(...) merge(..., by = "site", all = T),
                             list(setnames(publications[, count(unlist(sites))], c("site", "publications")), cdc_incidence, dcast(seer_summary, site ~ x, value.var = "value")))) %>% setorder(-publications)
  sites[, `:=`(site = factor(site, levels = site))]
  sites <- sites[, lapply(.SD, as.numeric), by = site] %>% na.omit
}
