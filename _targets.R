# SEER-Medicare publications pipeline
#
#   targets::tar_make()   # scrape, check, write data/ snapshot, build site/
#
# Checks run before anything is written: if a scrape looks broken, the run
# stops and data/ and site/ are left untouched. See README for the full flow.

library(targets)

tar_source("R")
tar_option_set(packages = project_packages(), format = "rds")

list(
  # --- Configuration ---
  tar_target(cdc_wonder_xml, "config/cdc_wonder_request.xml", format = "file"),

  # --- Data acquisition (always re-scraped; sources change without notice) ---
  tar_target(publications, get_publication_data(), cue = tar_cue(mode = "always")),
  tar_target(cdc, get_cdc_data(cdc_wonder_xml), cue = tar_cue(mode = "always")),
  tar_target(seer, get_seer_data(), cue = tar_cue(mode = "always")),
  tar_target(cdc_release, latest_wonder_release(), cue = tar_cue(mode = "always")),

  # --- Baseline: the snapshot committed at HEAD ---
  tar_target(baseline, read_committed_csv("data/publications.csv"), cue = tar_cue(mode = "always")),
  tar_target(seer_baseline, read_committed_csv("data/seer.csv"), cue = tar_cue(mode = "always")),

  # --- Checks (block publishing on errors) ---
  tar_target(checks, check_update(publications, seer, cdc, baseline, cdc_release,
                                  seer_baseline = seer_baseline)),
  tar_target(checks_passed, assert_checks(checks)),

  # --- Assembly ---
  tar_target(seer_pub_data, {
    checks_passed
    assemble_seermedicare(publications = publications, cdc = cdc, seer = seer,
                          cdc_wonder_request_xml = cdc_wonder_xml)
  }),
  tar_target(report_data, prepare_report_data(seer_pub_data)),

  # --- Outputs ---
  tar_target(snapshot, {
    checks_passed
    write_snapshot(publications, seer, cdc, dir = "data")
  }, format = "file"),
  tar_target(web_page, {
    generate_web(seer_pub_data, report_data = report_data, outputPath = "site")
    "site/index.html"
  }, format = "file"),
  tar_target(update_summary, {
    dir.create("output", showWarnings = FALSE)
    writeLines(summarize_changes(publications, seer_pub_data, checks, baseline), "output/update_summary.md")
    "output/update_summary.md"
  }, format = "file")
)
