test_that("default CDC year uses April update lag", {
  expect_equal(seermedicare:::.default_cdc_year(as.Date("2026-01-15")), "2022")
  expect_equal(seermedicare:::.default_cdc_year(as.Date("2026-05-02")), "2023")
})

test_that("read_wonder_request_xml overrides year", {
  xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<request-parameters>",
    "<parameter><name>V_D205.V1</name><value>2020</value></parameter>",
    "<parameter><name>O_export-format</name><value>xls</value></parameter>",
    "</request-parameters>"
  )
  path <- tempfile(fileext = ".xml")
  writeLines(xml, path)
  params <- seermedicare:::.read_wonder_request_xml(path, year = "2022", export_format = "csv")
  expect_equal(params[["V_D205.V1"]], "2022")
})

test_that("parse_wonder_csv maps expected columns from WONDER HTML table", {
  # WONDER returns an HTML page; the parser looks for a data table with > 100 rows
  rows <- paste0("<tr><td>Breast</td><td>65-69 years</td><td>", 100:200, "</td></tr>", collapse = "")
  html <- paste0(
    "<html><body><table>",
    "<tr><th>Cancer Sites</th><th>Age Groups</th><th>Count</th></tr>",
    rows,
    "</table></body></html>"
  )
  dt <- seermedicare:::.parse_wonder_csv(html, year = "2022")
  expect_equal(nrow(dt), 101)
  expect_equal(dt$site[[1]], "Breast")
  expect_equal(dt$age[[1]], "65-69")
  expect_equal(dt$count[[1]], 100)
  expect_equal(dt$year[[1]], "2022")
  expect_equal(dt$event_type[[1]], "Incidence")
})

test_that("normalize_pubsearch_publications maps core fields", {
  publications <- list(list(
    authors_formatted = c("A B", "C D"),
    pubdate = "2020",
    journal = "Journal Name",
    medicare_topics = c("Topic1", "Topic2"),
    tumorsite = c("Breast"),
    pmid = "12345",
    Title = "Sample Title"
  ))
  dt <- seermedicare:::.normalize_pubsearch_publications(publications)
  expect_equal(dt$authors[[1]], "A B, C D")
  expect_equal(dt$topics[[1]], "Topic1; Topic2")
  expect_equal(dt$sites[[1]], "Breast")
  expect_equal(dt$link[[1]], "https://pubmed.ncbi.nlm.nih.gov/12345/")
})

test_that("fetch_pubsearch_token extracts token from JS bundle", {
  token <- paste0("Token ", paste(rep("a", 40), collapse = ""))
  main_html <- "<script src=\"/vue-apps/assets/main-abc123.js\"></script>"
  js <- paste0("const x = \"", token, "\";")

  testthat::local_mocked_bindings(
    GET = function(url, ...) list(url = url),
    content = function(resp, as, encoding) {
      if (grepl("publications/$", resp$url)) {
        return(main_html)
      }
      js
    },
    stop_for_status = function(...) NULL,
    .package = "httr"
  )

  expect_equal(seermedicare:::.fetch_pubsearch_token(), token)
})

test_that("pubsearch token refresh occurs on auth errors", {
  calls <- new.env(parent = emptyenv())
  calls$token_refresh <- 0
  calls$get_calls <- 0

  make_resp <- function(status, body = "") {
    list(status = status, body = body)
  }

  ok_payload <- jsonlite::toJSON(
    list(
      metadata = list(numresults = 1),
      publications = list(list(
        authors = c("A B"),
        pubdate = "2020",
        journal = "Journal",
        medicare_topics = c("Topic"),
        tumorsite = c("Breast"),
        pmid = "123",
        Title = "Sample Title"
      ))
    ),
    auto_unbox = TRUE
  )

  testthat::local_mocked_bindings(
    .pubsearch_get = function(api_url, token, params) {
      calls$get_calls <- calls$get_calls + 1
      if (calls$get_calls == 1) {
        return(make_resp(401))
      }
      make_resp(200, ok_payload)
    },
    .fetch_pubsearch_token = function() {
      calls$token_refresh <- calls$token_refresh + 1
      "Token refreshed"
    },
    .package = "seermedicare"
  )

  testthat::local_mocked_bindings(
    status_code = function(resp) resp$status,
    content = function(resp, as, encoding) resp$body,
    stop_for_status = function(...) NULL,
    .package = "httr"
  )

  dt <- seermedicare:::.fetch_pubsearch_publications(
    api_url = "https://toolbox.imsweb.com/pubsearch-api/publications/hdrp/advanced/",
    data_source = "SEERM",
    auth_token = "Token stale",
    batch_size = 1000
  )
  expect_equal(calls$token_refresh, 1)
  expect_equal(nrow(dt), 1)
})
