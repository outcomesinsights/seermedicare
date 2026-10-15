
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
#'
#' @return \code{\link{data.table}}
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

  # set blank cells to NA and remove rows with critical fields missing
  # Only require authors, date, and journal - sites get "Not Specified", topics get "Unknown"
  publications <- publications[, lapply(.SD, function(x){ifelse(x == "", NA, x)})] %>%
    na.omit(cols = c("authors", "date", "journal"))
  
  # Fill missing topics with "Unknown"
  if ("topics" %in% names(publications)) {
    publications[is.na(topics), topics := "Unknown"]
  }

  # create and clean columns
  generateids <- function(n) {
    sprintf(paste0("x%0", nchar(n), "d"), 1:n)
  }

  publications[,`:=`(id = generateids(nrow(publications)), # new column -  unique identifiers
                     year = stringi::stri_extract_first_regex(date, "[[:digit:]]{4}"), # new column - publication year only
                     issue = journal, # new column - journal with volume and issue (because journal will be modified)
                     journal = stringi::stri_extract_all_regex(journal, "[[:alpha:]]+") %>% sapply(paste, collapse = " "))] %>% setkey(id)
  
  # Fetch ISO journal abbreviations from PubMed (if PMIDs available)
  if ("pmid" %in% names(publications)) {
    message("Fetching journal abbreviations from PubMed")
    journal_abbrevs <- .fetch_journal_abbreviations(publications$pmid)
    if (nrow(journal_abbrevs) > 0) {
      # Merge abbreviations back to publications
      publications <- merge(publications, journal_abbrevs, by = "pmid", all.x = TRUE)
      # Use abbreviation if available, otherwise keep cleaned journal name
      publications[!is.na(journal_abbrev), journal := journal_abbrev]
      publications[, journal_abbrev := NULL]  # Remove temporary column
      setkey(publications, id)
    }
  }
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

#' Fetch journal abbreviations from PubMed E-utilities API
#'
#' Takes a vector of PMIDs and returns a data.table mapping PMID to ISO journal abbreviation.
#' Batches requests to avoid rate limits (max 200 PMIDs per request).
#'
#' @param pmids Character vector of PubMed IDs
#' @param batch_size Number of PMIDs to process per API request (default 200)
#' @return data.table with columns: pmid, journal_abbrev
.fetch_journal_abbreviations <- function(pmids, batch_size = 200) {
  # Remove NA values
  valid_pmids <- pmids[!is.na(pmids) & nzchar(pmids)]
  if (length(valid_pmids) == 0) {
    return(data.table::data.table(pmid = character(), journal_abbrev = character()))
  }
  
  # Remove duplicates for efficiency
  unique_pmids <- unique(valid_pmids)
  message(sprintf("Fetching journal abbreviations for %d unique PMIDs", length(unique_pmids)))
  
  # Process in batches
  results <- list()
  n_batches <- ceiling(length(unique_pmids) / batch_size)
  
  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(unique_pmids))
    batch_pmids <- unique_pmids[start_idx:end_idx]
    
    # Build E-utilities URL
    pmid_string <- paste(batch_pmids, collapse = ",")
    url <- sprintf(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=%s&rettype=xml",
      pmid_string
    )
    
    # Make request with retry logic
    resp <- tryCatch({
      httr::GET(url, httr::timeout(60))
    }, error = function(e) {
      warning(sprintf("PubMed API request failed for batch %d: %s", i, e$message))
      return(NULL)
    })
    
    if (is.null(resp) || httr::status_code(resp) != 200) {
      warning(sprintf("PubMed API returned non-200 status for batch %d", i))
      next
    }
    
    # Parse XML response
    xml_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # Extract PMID and ISOAbbreviation pairs using regex (faster than full XML parsing)
    # Pattern: <PMID...>12345</PMID> ... <ISOAbbreviation>J Name</ISOAbbreviation>
    articles <- strsplit(xml_text, "<PubmedArticle>")[[1]][-1]  # Split by article, remove first empty
    
    for (article in articles) {
      pmid_match <- regmatches(article, regexpr("<PMID[^>]*>([0-9]+)</PMID>", article))
      iso_match <- regmatches(article, regexpr("<ISOAbbreviation>([^<]+)</ISOAbbreviation>", article))
      
      if (length(pmid_match) && length(iso_match)) {
        pmid_val <- gsub("<PMID[^>]*>|</PMID>", "", pmid_match)
        iso_val <- gsub("<ISOAbbreviation>|</ISOAbbreviation>", "", iso_match)
        results[[length(results) + 1]] <- list(pmid = pmid_val, journal_abbrev = iso_val)
      }
    }
    
    # Rate limiting: NCBI allows 3 requests/second without API key
    if (i < n_batches) {
      Sys.sleep(0.4)  # ~2.5 requests/second to be safe
    }
  }
  
  if (length(results) == 0) {
    return(data.table::data.table(pmid = character(), journal_abbrev = character()))
  }
  
  data.table::rbindlist(results)
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

    # Handle pmid - convert NULL/empty to NA
    pmid_val <- if (is.null(pub$pmid) || !length(pub$pmid) || !nzchar(pub$pmid)) {
      NA_character_
    } else {
      as.character(pub$pmid)
    }
    
    # Handle journal - convert NULL/empty to NA
    journal_val <- if (is.null(pub$journal) || !length(pub$journal) || !nzchar(pub$journal)) {
      NA_character_
    } else {
      as.character(pub$journal)
    }
    
    # Handle date fields - extract all three and create coalesced date
    edate_val <- if (is.null(pub$edate) || !length(pub$edate) || !nzchar(pub$edate)) {
      NA_character_
    } else {
      as.character(pub$edate)
    }
    
    pubdate_val <- if (is.null(pub$pubdate) || !length(pub$pubdate) || !nzchar(pub$pubdate)) {
      NA_character_
    } else {
      as.character(pub$pubdate)
    }
    
    computed_date_val <- if (is.null(pub$computed_date) || !length(pub$computed_date) || !nzchar(pub$computed_date)) {
      NA_character_
    } else {
      as.character(pub$computed_date)
    }
    
    # Use edate (electronic) if available, otherwise pubdate (print)
    date_val <- if (!is.na(edate_val)) edate_val else pubdate_val
    
    list(
      authors = authors_val,
      edate = edate_val,
      pubdate = pubdate_val,
      computed_date = computed_date_val,
      date = date_val,
      journal = journal_val,
      topics = topics_val,
      sites = sites_val,
      link = build_link(pub),
      title = pub$Title,
      pmid = pmid_val
    )
  }), fill = TRUE)
}
