# Data snapshot, validation checks, and change summary for each update.
#
# The committed CSVs in data/ are the record of what is live. Each run compares
# fresh scrapes against the versions committed at HEAD (via `git show`), so the
# baseline is always "what was last merged", regardless of the working tree.

#' Stable key for a publication: PubMed ID, or normalized title when there is none
publication_key <- function(pmid, title) {
  norm_title <- tolower(gsub("[^[:alnum:]]+", " ", title))
  ifelse(is.na(pmid) | !nzchar(pmid), paste0("title:", trimws(norm_title)), paste0("pmid:", pmid))
}

#' Flatten publications to a one-row-per-paper table suitable for CSV
publications_snapshot <- function(publications) {
  collapse <- function(x) vapply(x, function(v) paste(unname(unlist(v)), collapse = "; "), character(1))
  dt <- data.table::data.table(
    pub_id = publication_key(publications$pmid, publications$title),
    pmid = publications$pmid,
    year = as.character(publications$year),
    date = publications$edate,
    journal = publications$journal,
    title = publications$title,
    authors = collapse(publications$authors),
    sites = collapse(publications$sites),
    topics = collapse(publications$topics),
    link = publications$link
  )
  data.table::setorder(dt, pub_id)
  dt
}

#' Write the data snapshot CSVs; returns the file paths (for targets format = "file")
write_snapshot <- function(publications, seer, cdc, dir = "data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- c(
    publications = file.path(dir, "publications.csv"),
    seer = file.path(dir, "seer.csv"),
    cdc = file.path(dir, "cdc.csv")
  )
  data.table::fwrite(publications_snapshot(publications), files[["publications"]])
  data.table::fwrite(data.table::setorder(data.table::copy(seer), site, x), files[["seer"]])
  data.table::fwrite(data.table::setorder(data.table::copy(cdc), site, age), files[["cdc"]])
  unname(files)
}

#' Read a committed snapshot CSV from git HEAD; NULL if unavailable (first run, no git)
read_committed_csv <- function(path) {
  text <- tryCatch(
    suppressWarnings(system2("git", c("show", paste0("HEAD:", path)), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (!length(text) || !is.null(attr(text, "status"))) return(NULL)
  data.table::fread(text = text, colClasses = "character")
}

#' Validate a fresh scrape against the committed baseline
#'
#' Errors are problems that should block publishing (the scrape is probably broken).
#' Warnings are things a person should look at before merging.
#'
#' @return list(errors = character, warnings = character)
check_update <- function(publications, seer, cdc, baseline = NULL, cdc_release = NA_integer_,
                         dataset_release = .wonder_dataset$release, seer_baseline = NULL) {
  errors <- character(); warnings <- character()
  new <- publications_snapshot(publications)

  # --- Publications ---
  if (nrow(new) < 1000) {
    errors <- c(errors, sprintf("Only %d publications scraped; expected thousands.", nrow(new)))
  }
  missing_year <- sum(is.na(new$year) | !grepl("^\\d{4}$", new$year))
  if (missing_year > 0.01 * nrow(new)) {
    errors <- c(errors, sprintf("%d publications have no valid year.", missing_year))
  }
  missing_title <- sum(is.na(new$title) | !nzchar(new$title))
  if (missing_title > 0) {
    errors <- c(errors, sprintf("%d publications have no title.", missing_title))
  }
  dup <- sum(duplicated(new$pub_id))
  if (dup > 0) {
    warnings <- c(warnings, sprintf("%d publications share a PubMed ID or title with another entry.", dup))
  }

  if (!is.null(baseline) && nrow(baseline)) {
    dropped <- setdiff(baseline$pub_id, new$pub_id)
    if (length(dropped) > max(10, 0.02 * nrow(baseline))) {
      errors <- c(errors, sprintf(
        "%d previously listed publications are missing (%.1f%% of %d); the NCI scrape may be broken.",
        length(dropped), 100 * length(dropped) / nrow(baseline), nrow(baseline)))
    } else if (length(dropped)) {
      warnings <- c(warnings, sprintf(
        "%d previously listed publication(s) no longer appear in the NCI list (see 'Removed' below).", length(dropped)))
    }

    # A completed year's count shouldn't go down
    old_years <- baseline[, .N, by = year]
    new_years <- new[, .N, by = year]
    cmp <- merge(old_years, new_years, by = "year", suffixes = c("_old", "_new"))
    fell <- cmp[N_new < N_old]
    if (nrow(fell)) {
      warnings <- c(warnings, sprintf("Publication count went down for %s.",
        paste(sprintf("%s (%d -> %d)", fell$year, fell$N_old, fell$N_new), collapse = ", ")))
    }

    # blank sites come back from CSV as NA
    not_spec <- function(d) mean(is.na(d$sites) | d$sites %in% c("", "Not Specified"))
    if (not_spec(new) > not_spec(baseline) + 0.02) {
      warnings <- c(warnings, sprintf("Share of publications with no cancer site rose from %.1f%% to %.1f%%.",
        100 * not_spec(baseline), 100 * not_spec(new)))
    }
  }

  # --- SEER ---
  seer_sites <- data.table::uniqueN(seer$site)
  if (seer_sites < 30) {
    errors <- c(errors, sprintf("Only %d SEER cancer sites parsed; expected about 33.", seer_sites))
  }
  # Some SEER pages never publish a figure (e.g. no prevalence for Tongue), so only
  # flag values that were present in the committed snapshot and are now missing
  missing <- seer[is.na(value), paste(site, x)]
  if (!is.null(seer_baseline)) {
    missing <- setdiff(missing, seer_baseline[is.na(value) | !nzchar(value), paste(site, x)])
  }
  if (length(missing)) {
    warnings <- c(warnings, sprintf("SEER values missing for: %s.", paste(missing, collapse = ", ")))
  }

  # --- CDC ---
  if (nrow(cdc) < 1000) {
    errors <- c(errors, sprintf("Only %d CDC WONDER rows; expected about 1,500.", nrow(cdc)))
  }
  if (!is.na(cdc_release) && cdc_release > dataset_release) {
    warnings <- c(warnings, sprintf(
      "CDC WONDER has a newer cancer statistics release (v%d) than the one used (v%d). Updating needs a new request XML; see README.",
      cdc_release, dataset_release))
  }

  list(errors = errors, warnings = warnings)
}

#' Stop the pipeline if any check failed, so nothing broken gets published
assert_checks <- function(checks) {
  for (w in checks$warnings) warning(w, call. = FALSE)
  if (length(checks$errors)) {
    stop("Update checks failed:\n- ", paste(checks$errors, collapse = "\n- "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Markdown summary of what changed since the committed baseline (used as the PR description)
summarize_changes <- function(publications, seer_pub_data, checks, baseline = NULL, max_listed = 200) {
  new <- publications_snapshot(publications)
  md <- c(sprintf("## SEER-Medicare publications update (%s)", format(Sys.Date())), "")

  if (length(checks$warnings)) {
    md <- c(md, "### Needs a look", "", paste0("- ", checks$warnings), "")
  }

  if (is.null(baseline) || !nrow(baseline)) {
    md <- c(md, sprintf("**%s publications.** No committed baseline to compare against.",
      format(nrow(new), big.mark = ",")), "")
  } else {
    added <- new[!pub_id %in% baseline$pub_id]
    removed <- baseline[!pub_id %in% new$pub_id]
    md <- c(md, sprintf("**%s publications** (was %s): %d added, %d removed.",
      format(nrow(new), big.mark = ","), format(nrow(baseline), big.mark = ","),
      nrow(added), nrow(removed)), "")

    years <- merge(baseline[, .(before = .N), by = year], new[, .(after = .N), by = year], all = TRUE)
    years[is.na(before), before := 0L][is.na(after), after := 0L]
    years <- years[before != after]
    if (nrow(years)) {
      data.table::setorder(years, -year)
      md <- c(md, "### Counts by year (changed years only)", "",
        "| Year | Before | After | Change |", "|---|---:|---:|---:|",
        sprintf("| %s | %d | %d | %+d |", years$year, years$before, years$after, years$after - years$before), "")
    }

    list_pubs <- function(d, heading) {
      if (!nrow(d)) return(character())
      data.table::setorder(d, -year, title)
      shown <- head(d, max_listed)
      out <- c(sprintf("### %s (%d)", heading, nrow(d)), "",
        sprintf("- %s. [%s](%s) *%s*", shown$year, shown$title, shown$link, shown$journal))
      if (nrow(d) > max_listed) {
        out <- c(out, sprintf("- ...and %d more (see the `data/publications.csv` diff)", nrow(d) - max_listed))
      }
      c(out, "")
    }
    md <- c(md, list_pubs(added, "Added"), list_pubs(removed, "Removed"))
  }

  meta <- seer_pub_data$metadata
  md <- c(md, "### Data sources", "",
    sprintf("- SEER statistics year: %s", meta$incidence_year),
    sprintf("- CDC WONDER incidence year: %s (dataset %s, release v%d)", meta$cdc_year,
      .wonder_dataset$id, .wonder_dataset$release),
    "")
  paste(md, collapse = "\n")
}
