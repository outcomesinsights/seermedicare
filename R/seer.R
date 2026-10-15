
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

#' Format Seer Labels
#'
#' @param x Character string for the SEER data type ("incidence", "prevalence", or "deaths")
#' @param sites data.table of site summary data from scrape_all_data
#' @param metadata List of metadata from scrape_all_data
#' @param measure Character string for the measure label (defaults to title case of x)
#'
#' @return String in the form [SEER Data] per [SEER Data Scale] ([SEER Data Year]) - e.g., Incidence per 1,000 (2016)
.seer_label <- function(x, sites, metadata, measure = tools::toTitleCase(x)) {
  sprintf("%s per %s (%s)", measure, format(get_scale(sites[[x]]), big.mark = ","), metadata[[paste0(x, "_year")]])
}
