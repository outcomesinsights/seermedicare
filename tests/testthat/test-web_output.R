# The web page builds from a real (sampled) assembled dataset.

test_that("generate_web writes the page, Excel download, and JS libraries", {
  d <- readRDS(test_path("fixtures", "seer_pub_data_sample.rds"))
  out <- withr::local_tempdir()
  # plotly warns "Ignoring 1 observations" for a site with no SEER value in the sample
  suppressWarnings(suppressMessages(generate_web(d, prepare_report_data(d), outputPath = out)))

  expect_true(file.exists(file.path(out, "index.html")))
  expect_true(file.exists(file.path(out, "SEER-Medicare-Publications.xlsx")))
  expect_true(dir.exists(file.path(out, "lib")))

  html <- paste(readLines(file.path(out, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "150")                     # total publication count appears
  expect_match(html, "SEER-Medicare-Publications.xlsx", fixed = TRUE)
  expect_equal(sort(openxlsx::getSheetNames(file.path(out, "SEER-Medicare-Publications.xlsx")))[1], "About")
})

test_that("generate_web clears library folders left by older package versions", {
  d <- readRDS(test_path("fixtures", "seer_pub_data_sample.rds"))
  out <- withr::local_tempdir()
  stale <- file.path(out, "lib", "plotly-binding-0.0.1")
  dir.create(stale, recursive = TRUE)
  suppressWarnings(suppressMessages(generate_web(d, prepare_report_data(d), outputPath = out)))

  expect_false(dir.exists(stale))
  expect_true(length(list.files(file.path(out, "lib"))) > 0)
})
