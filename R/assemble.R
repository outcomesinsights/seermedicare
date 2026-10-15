
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
    metadata[["seer_citation"]] <- sprintf("SEER Cancer Stat Facts: %s", seer_url)
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

#' Summarize CDC incidence data by standardized cancer site
#'
#'
#' @param cdc \code{data.table} from running get_cdc_data and creating site_std variable which is the site standardized to publication site
#' @param cdc_year \code{character} The cdc data has results for each individual year and a summary of the last 4 years in the form (YYYY-YYYY). As new data comes in this variable will need to be udpated to match the newest 4 year summary.
#'
#' @return \code{\link{data.table}}
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
