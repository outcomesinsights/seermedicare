# Parsers tested against real responses saved from each source (tests/testthat/fixtures/).
# If a source changes its format, re-capture the fixture and update the expected values;
# a failure here with an unchanged fixture means the parsing code broke.

fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")

test_that("CDC WONDER results page parses to site x age counts", {
  dt <- suppressWarnings(.parse_wonder_csv(fixture("wonder_2022.html"), year = "2022"))
  expect_equal(nrow(dt), 1483)
  expect_equal(dt[site == "Lung and Bronchus" & age == "65-69", count], 40272)
  expect_equal(dt[site == "Prostate" & age == "85+", count], 9379)
  expect_equal(unique(dt$year), "2022")
  expect_false(anyNA(dt$count))
})

test_that("SEER stat facts index lists cancer site links", {
  page <- xml2::read_html(test_path("fixtures", "seer_index.html"))
  links <- rvest::html_elements(page, ".alphaList.row a")
  expect_gt(length(links), 30)
  expect_true("Anus" %in% rvest::html_text(links))
})

test_that("SEER stat facts site page parses incidence, deaths, prevalence, year", {
  page <- xml2::read_html(test_path("fixtures", "seer_anus.html"))
  rlang::local_bindings(.seer_fetch_html = function(url, retries, user_agent) page, .env = globalenv())
  link <- rvest::html_element(xml2::read_html('<a href="/statfacts/html/anus.html">Anus</a>'), "a")

  dt <- parse_seer_cancer_fact_page(link)
  expect_equal(dt[x == "incidence", value], 11270)
  expect_equal(dt[x == "deaths", value], 1700)
  expect_equal(dt[x == "prevalence", value], 88564)
  expect_equal(unique(dt$year), 2026)
})

test_that("NCI publication search + PubMed lookup produce publication rows", {
  rlang::local_bindings(
    .pubsearch_get = function(api_url, token, params) {
      body <- if (params$b_start == 0) fixture("pubsearch_page.json") else
        '{"metadata": {"numresults": 5}, "publications": []}'
      list(status = 200, body = body)
    },
    .env = globalenv()
  )
  local_mocked_bindings(
    GET = function(url, ...) list(status = 200, body = fixture("pubmed_efetch.xml")),
    status_code = function(resp) resp$status,
    content = function(resp, as, encoding) resp$body,
    stop_for_status = function(...) NULL,
    .package = "httr"
  )

  dt <- suppressMessages(get_publication_data(auth_token = "Token test"))
  expect_equal(nrow(dt), 5)
  row <- dt[pmid == "42832123"]
  expect_equal(row$year, "2026")
  expect_equal(row$journal, "Cancer Causes Control")  # ISO abbreviation from PubMed
  expect_equal(row$sites[[1]], "Multiple Myeloma")     # site code MM mapped to name
  expect_equal(row$link, "https://pubmed.ncbi.nlm.nih.gov/42832123/")
  expect_match(row$title, "^Emergency diagnosis of multiple myeloma")
})
