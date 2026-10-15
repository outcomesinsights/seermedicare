# Run the test suite: Rscript run_tests.R
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
