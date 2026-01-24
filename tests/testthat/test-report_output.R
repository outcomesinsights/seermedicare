test_that("generate_report creates docx output", {
  testthat::skip_if_not_installed("officer")
  testthat::skip_if_not_installed("flextable")

  publications <- data.table::data.table(
    id = c("x1", "x2"),
    authors = list(c("Alpha A"), c("Beta B")),
    date = c("2020-01-01", "2021-01-01"),
    journal = c("Journal A", "Journal B"),
    topics = list("Topic1", "Topic2"),
    sites = list(c("Breast"), c("Lung")),
    year = c(2020, 2021),
    issue = c("J A", "J B")
  )

  site_summary <- data.table::data.table(
    site = factor(c("Breast", "Lung"), levels = c("Breast", "Lung")),
    publications = c(10, 8),
    allages = c(1000, 900),
    min65 = c(600, 500),
    incidence = c(200, 180),
    prevalence = c(300, 250),
    deaths = c(50, 45)
  )

  metadata <- list(
    prevalence_year = 2022,
    incidence_year = 2022,
    deaths_year = 2022,
    cdc_year = "2020-2022",
    seer_citation = "SEER citation",
    nci_url = "https://healthcaredelivery.cancer.gov/publications/",
    access_date = as.Date("2026-01-01"),
    rvestvers = "1.0.0"
  )

  seer_pub_data <- structure(
    list(
      publications = publications,
      site_summary = site_summary,
      metadata = metadata
    ),
    class = "seermedicare"
  )

  out_dir <- tempfile("seer-report-")
  dir.create(out_dir, recursive = TRUE)
  seermedicare::generate_report(seer_pub_data, outputPath = out_dir)

  expect_true(length(list.files(out_dir, pattern = "docx$")) > 0)
})

test_that("generate_report attempts PDF when pandoc available", {
  testthat::skip_if_not_installed("officer")
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())

  publications <- data.table::data.table(
    id = "x1",
    authors = list(c("Alpha A")),
    date = "2020-01-01",
    journal = "Journal A",
    topics = list("Topic1"),
    sites = list(c("Breast")),
    year = 2020,
    issue = "J A"
  )

  site_summary <- data.table::data.table(
    site = factor("Breast", levels = "Breast"),
    publications = 10,
    allages = 1000,
    min65 = 600,
    incidence = 200,
    prevalence = 300,
    deaths = 50
  )

  metadata <- list(
    prevalence_year = 2022,
    incidence_year = 2022,
    deaths_year = 2022,
    cdc_year = "2020-2022",
    seer_citation = "SEER citation",
    nci_url = "https://healthcaredelivery.cancer.gov/publications/",
    access_date = as.Date("2026-01-01"),
    rvestvers = "1.0.0"
  )

  seer_pub_data <- structure(
    list(
      publications = publications,
      site_summary = site_summary,
      metadata = metadata
    ),
    class = "seermedicare"
  )

  out_dir <- tempfile("seer-report-pdf-")
  dir.create(out_dir, recursive = TRUE)
  seermedicare::generate_report(seer_pub_data, outputPath = out_dir, pdf = TRUE)

  expect_true(length(list.files(out_dir, pattern = "pdf$")) > 0)
})
