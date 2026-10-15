# SEER-Medicare Publications Summary

Builds the interactive summary of published research using SEER-Medicare linked data,
live at **https://seer-medicare.outins.com/**.

Each update scrapes three sources, checks the results, and rebuilds the page:

| Source | What | Code |
|---|---|---|
| [NCI SEER-Medicare publications](https://healthcaredelivery.cancer.gov/publications/) (+ PubMed for journal abbreviations) | The publication list | `R/publications.R` |
| [SEER Cancer Stat Facts](https://seer.cancer.gov/statfacts/) | Incidence, prevalence, deaths by cancer site | `R/seer.R` |
| [CDC WONDER](https://wonder.cdc.gov/cancer.html) U.S. Cancer Statistics | Incidence counts by site and age (for the 65+ share) | `R/cdc.R` |

## How updates happen

A GitHub Actions workflow (`.github/workflows/quarterly-update.yml`) runs on the 5th of
January, April, July and October. It:

1. runs the tests,
2. scrapes all sources and runs the **update checks** (below),
3. if the data changed, opens a **pull request assigned to @markdanese** with a summary of
   what changed (new and removed publications, year-by-year counts, anything that needs a look).

Merging the PR publishes the site (`.github/workflows/publish-site.yml`). Closing it
without merging publishes nothing.

If anything fails, the workflow opens an **issue assigned to @markdanese** with a link to
the log, and the live site is left as it was. GitHub emails you about the PR or the issue.

To run an update now: Actions tab → *Quarterly update* → *Run workflow*.

### Update checks (`R/checks.R`)

Each run is compared with the snapshot in `data/` as committed on `master`, so the
comparison is always against what is live.

**Stop the run** (nothing is published): fewer than 1,000 publications; more than 2% of
previously listed publications missing; publications with no year or title; fewer than 30
SEER cancer sites; fewer than 1,000 CDC rows.

**Flagged in the PR**: a few publications removed; a year's count going down; more
publications with no cancer site; SEER values that were present last time now missing;
CDC WONDER publishing a newer cancer statistics release than the one used.

## Layout

```
R/                 pipeline code (loaded with targets::tar_source(), not installed as a package)
_targets.R         the pipeline: scrape -> check -> snapshot -> build site
config/            CDC WONDER request (dataset D205)
data/              committed snapshot of each source, the baseline for the next update
site/              the published page (index.html, Excel download, JS libraries)
tests/testthat/    tests; fixtures/ holds real responses saved from each source
DESCRIPTION        dependency list only
```

## Running locally

```r
# install.packages(c("targets", "testthat", "withr", "data.table", "DT", "htmltools",
#                    "httr", "jsonlite", "magrittr", "openxlsx", "plotly", "rvest", "stringi", "xml2"))
targets::tar_make()        # scrape, check, write data/ and site/, write output/update_summary.md
source("run_tests.R")      # or: Rscript run_tests.R
```

Open `site/index.html` to review. Committing and pushing `data/` and `site/` to `master`
publishes, the same as merging an update PR.

## Maintenance notes

**New CDC WONDER release.** When the PR flags a newer release (e.g. v2023), CDC has
published a new dataset with its own ID and parameter names, so the request has to be
rebuilt: on [wonder.cdc.gov/cancer.html](https://wonder.cdc.gov/cancer.html) open the new
incidence release, group by Cancer Sites and Age Groups for one year, save the request
XML over `config/cdc_wonder_request.xml`, and update `.wonder_dataset` (and the
`V_D205.V1` parameter name) in `R/cdc.R`.

**A test fails on a fixture.** The tests in `test-real_responses.R` parse saved real
responses. If a source changed format, the quarterly checks will usually catch it first;
fix the parser, re-save the fixture from the live source, and update the expected values.

**SEER pages with no figure.** Some SEER pages don't publish every figure (Tongue and
Vulva have no prevalence). These show as blank in `data/seer.csv` and are not flagged again.
