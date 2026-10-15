# Update checks, snapshot, and change summary.

sample_data <- function() readRDS(test_path("fixtures", "seer_pub_data_sample.rds"))

# Fake a full-size scrape by repeating the sample with distinct PMIDs
make_pubs <- function(n = 1500) {
  p <- sample_data()$publications
  p <- p[rep(seq_len(nrow(p)), length.out = n)]
  p[, pmid := as.character(10000000 + seq_len(n))]
  p[, link := paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")]
  p[]
}
make_seer <- function(sites = 33) {
  data.table::data.table(site = rep(paste("Site", seq_len(sites)), each = 3), year = 2026,
                         x = c("incidence", "prevalence", "deaths"), value = 100)
}
make_cdc <- function(n = 1483) data.table::data.table(site = "Lung", age = as.character(seq_len(n)), count = 1)
as_baseline <- function(pubs) {
  b <- publications_snapshot(pubs)
  b[, names(b) := lapply(.SD, as.character)][]
}

test_that("publication_key prefers PMID and falls back to normalized title", {
  expect_equal(publication_key(c("123", NA, ""), c("A", "Some Title!", "x  y")),
               c("pmid:123", "title:some title", "title:x y"))
})

test_that("a healthy update passes with no errors or warnings", {
  pubs <- make_pubs()
  res <- check_update(pubs, make_seer(), make_cdc(), baseline = as_baseline(pubs[1:1490]), cdc_release = 2022L)
  expect_length(res$errors, 0)
  expect_length(res$warnings, 0)
})

test_that("a broken publications scrape is an error", {
  res <- check_update(make_pubs(200), make_seer(), make_cdc())
  expect_match(res$errors, "Only 200 publications", all = FALSE)
})

test_that("losing many previously listed publications is an error; a few is a warning", {
  pubs <- make_pubs()
  base <- as_baseline(pubs)
  res_many <- check_update(pubs[1:1400], make_seer(), make_cdc(), baseline = base)
  expect_match(res_many$errors, "100 previously listed publications are missing", all = FALSE)

  res_few <- check_update(pubs[1:1497], make_seer(), make_cdc(), baseline = base)
  expect_length(res_few$errors, 0)
  expect_match(res_few$warnings, "3 previously listed", all = FALSE)
})

test_that("a year's count going down is a warning", {
  pubs <- make_pubs()
  base <- as_baseline(pubs)
  yr <- pubs$year[1]
  drop_one <- pubs[-which(pubs$year == yr)[1]]
  res <- check_update(drop_one, make_seer(), make_cdc(), baseline = base)
  expect_match(res$warnings, sprintf("went down for %s", yr), all = FALSE)
})

test_that("SEER and CDC shortfalls are errors", {
  pubs <- make_pubs()
  res <- check_update(pubs, make_seer(sites = 10), make_cdc(n = 50))
  expect_match(res$errors, "Only 10 SEER cancer sites", all = FALSE)
  expect_match(res$errors, "Only 50 CDC WONDER rows", all = FALSE)
})

test_that("a newer CDC WONDER release is flagged", {
  res <- check_update(make_pubs(), make_seer(), make_cdc(), cdc_release = 2023L, dataset_release = 2022L)
  expect_match(res$warnings, "newer cancer statistics release \\(v2023\\)", all = FALSE)
})

test_that("assert_checks stops on errors and passes otherwise", {
  expect_error(assert_checks(list(errors = "boom", warnings = character())), "boom")
  expect_true(assert_checks(list(errors = character(), warnings = character())))
})

test_that("summarize_changes lists added and removed publications and year changes", {
  pubs <- make_pubs()
  base <- as_baseline(pubs[-(1:2)])        # first two are new
  base <- rbind(base, data.table::data.table(pub_id = "pmid:1", pmid = "1", year = "1999", date = "", journal = "J",
    title = "Gone paper", authors = "", sites = "", topics = "", link = "https://pubmed.ncbi.nlm.nih.gov/1/"))
  md <- summarize_changes(pubs, sample_data(), list(errors = character(), warnings = "check me"), base)

  expect_match(md, "2 added, 1 removed")
  expect_match(md, "### Needs a look")
  expect_match(md, "- check me", fixed = TRUE)
  expect_match(md, "### Added (2)", fixed = TRUE)
  expect_match(md, "[Gone paper](https://pubmed.ncbi.nlm.nih.gov/1/)", fixed = TRUE)
  expect_match(md, "| 1999 | 1 | 0 | -1 |", fixed = TRUE)
})

test_that("write_snapshot writes stable, sorted CSVs", {
  dir <- withr::local_tempdir()
  d <- sample_data()
  files <- write_snapshot(d$publications, make_seer(), make_cdc(), dir = dir)
  expect_true(all(file.exists(files)))
  snap <- data.table::fread(files[1], colClasses = "character")
  expect_equal(nrow(snap), 150)
  expect_false(is.unsorted(snap$pub_id))
  # same input, same bytes: no spurious diffs between runs
  first <- readLines(files[1]); write_snapshot(d$publications, make_seer(), make_cdc(), dir = dir)
  expect_identical(readLines(files[1]), first)
})

test_that("SEER values missing in the committed snapshot too are not flagged again", {
  seer <- make_seer(); seer[site == "Site 1" & x == "prevalence", value := NA]
  base <- data.table::data.table(site = "Site 1", year = "2026", x = "prevalence", value = "")
  expect_length(check_update(make_pubs(), seer, make_cdc(), seer_baseline = base)$warnings, 0)
  expect_match(check_update(make_pubs(), seer, make_cdc())$warnings, "Site 1 prevalence", all = FALSE)
})

test_that("checks work on a baseline read back from CSV (blank fields become NA)", {
  pubs <- make_pubs()
  pubs[1:5, sites := list(list(character()))]
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(publications_snapshot(pubs), path)
  base <- data.table::fread(path, colClasses = "character")
  expect_true(anyNA(base$sites))
  res <- check_update(pubs, make_seer(), make_cdc(), baseline = base)
  expect_length(res$errors, 0)
  expect_length(res$warnings, 0)
})
