test_that("get_cdc_data handles WONDER XML flow", {
  xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<request-parameters>",
    "<parameter><name>V_D205.V1</name><value>2022</value></parameter>",
    "<parameter><name>O_export-format</name><value>csv</value></parameter>",
    "</request-parameters>"
  )
  csv <- paste(
    "\"Notes\",\"Cancer Sites\",\"Cancer Sites Code\",\"Age Groups\",\"Age Groups Code\",Count",
    ",\"Breast\",\"26000\",\"65-69 years\",\"65-69\",100",
    sep = "\n"
  )
  path <- tempfile(fileext = ".xml")
  writeLines(xml, path)

  testthat::local_mocked_bindings(
    POST = function(url, body, encode) list(body = csv),
    content = function(resp, as, encoding) resp$body,
    stop_for_status = function(...) NULL,
    .package = "httr"
  )

  dt <- get_cdc_data(wonder_request_xml = path, year = "2022")
  expect_equal(dt$site[[1]], "Breast")
  expect_equal(dt$count[[1]], 100)
  expect_equal(dt$year[[1]], "2022")
})

test_that("get_publication_data uses pubsearch API flow", {
  payload <- jsonlite::toJSON(
    list(
      metadata = list(numresults = 1),
      publications = list(list(
        authors = c("A B"),
        pubdate = "2020",
        journal = "Journal",
        medicare_topics = c("Topic"),
        tumorsite = c("Breast"),
        pmid = "123"
      ))
    ),
    auto_unbox = TRUE
  )

  testthat::local_mocked_bindings(
    .pubsearch_get = function(api_url, token, params) {
      if (params$b_start > 0) {
        return(list(status = 200, body = jsonlite::toJSON(list(metadata = list(numresults = 1), publications = list()), auto_unbox = TRUE)))
      }
      list(status = 200, body = payload)
    },
    .package = "seermedicare"
  )

  testthat::local_mocked_bindings(
    status_code = function(resp) resp$status,
    content = function(resp, as, encoding) resp$body,
    stop_for_status = function(...) NULL,
    .package = "httr"
  )

  dt <- get_publication_data(auth_token = "Token test")
  expect_equal(dt$journal[[1]], "Journal")
  expect_equal(dt$sites[[1]], "Breast")
  expect_equal(dt$link[[1]], "https://pubmed.ncbi.nlm.nih.gov/123/")
})

test_that("get_seer_data parses site pages end-to-end", {
  index_html <- paste0(
    "<div class=\"alphaList rpw\">",
    "<a href=\"/statfacts/testcancer.html\">Test Cancer</a>",
    "</div>"
  )
  site_html <- paste0(
    "<div class=\"glance-factSheet\">",
    "<div class=\"glanceBox new\"><p>2022</p><p><span></span><span>1,234</span></p></div>",
    "<div class=\"glanceBox death\"><p><span></span><span>321</span></p></div>",
    "</div>",
    "<div class=\"factSheet\"><div class=\"tog-content\"><div class=\"row\">",
    "<p></p><p></p><p>It is estimated 5,678 people were living.</p>",
    "</div></div></div>"
  )

  testthat::local_mocked_bindings(
    .seer_fetch_html = function(url, retries, user_agent) {
      if (grepl("more.html", url)) {
        return(xml2::read_html(index_html))
      }
      xml2::read_html(site_html)
    },
    .package = "seermedicare"
  )

  dt <- get_seer_data(seer_url = "http://seer.cancer.gov/statfacts/more.html")
  expect_true(any(dt$x == "incidence"))
  expect_true(any(dt$x == "prevalence"))
  expect_true(any(dt$x == "deaths"))
  expect_equal(dt[dt$x == "incidence", ]$value[[1]], 1234)
})
