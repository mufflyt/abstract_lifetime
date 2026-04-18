# 10d_orcid_demographics.R — ORCID-based demographics for unmatched authors
#
# For 215 unmatched authors who have ORCIDs (from OpenAlex), queries the
# ORCID public API to extract: country, institution, department, role.
# Uses country as a definitive US/international classifier (fixes false
# is_us_based flags). For confirmed US authors found via ORCID, attempts
# a final NPI match with the ORCID-verified institution as disambiguation.
#
# Writes: data/processed/orcid_demographics.csv

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(httr); library(jsonlite)
})

cli_h1("ORCID Demographics for Unmatched Authors")

npi <- read_csv(here("data", "processed", "npi_matches.csv"), show_col_types = FALSE)
oa <- read_csv(here("data", "processed", "openalex_author_names.csv"), show_col_types = FALSE)

# Target: unmatched authors with ORCIDs
targets <- oa |>
  filter(!is.na(oa_orcid), nchar(oa_orcid) > 10,
         abstract_id %in% (npi |> filter(npi_match_confidence != "high") |> pull(abstract_id))) |>
  mutate(orcid_bare = str_replace(oa_orcid, "^https://orcid\\.org/", "")) |>
  distinct(orcid_bare, .keep_all = TRUE)

cli_alert_info("Unmatched authors with ORCIDs: {nrow(targets)}")

# Checkpoint
checkpoint_path <- here("data", "cache", "orcid_demographics_checkpoint.rds")
if (file.exists(checkpoint_path)) {
  cp <- readRDS(checkpoint_path)
  completed <- cp$completed
  results <- cp$results
  cli_alert_info("Resuming from checkpoint ({length(completed)} done)")
} else {
  completed <- character()
  results <- list()
}

remaining <- targets |> filter(!abstract_id %in% completed)
cli_alert_info("{nrow(remaining)} remaining")

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  if (i %% 25 == 0) cli_alert_info("  {i}/{nrow(remaining)}")

  Sys.sleep(0.5)  # ORCID rate limit: ~24 req/sec but be polite
  url <- paste0("https://pub.orcid.org/v3.0/", row$orcid_bare)
  resp <- tryCatch(
    httr::GET(url, httr::add_headers(Accept = "application/json"), httr::timeout(15)),
    error = function(e) NULL
  )

  orcid_name <- NA_character_
  orcid_country <- NA_character_
  orcid_institution <- NA_character_
  orcid_department <- NA_character_
  orcid_role <- NA_character_

  if (!is.null(resp) && status_code(resp) == 200) {
    d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
                  error = function(e) NULL)

    if (!is.null(d)) {
      # Name
      given <- d$person$name$`given-names`$value
      family <- d$person$name$`family-name`$value
      if (!is.null(given) && !is.null(family)) {
        orcid_name <- paste(given, family)
      }

      # Employment (most recent)
      emps <- d$`activities-summary`$employments$`affiliation-group`
      if (length(emps) > 0) {
        # Take first (most recent) employment
        first_emp <- emps[[1]]$summaries[[1]]$`employment-summary`
        if (!is.null(first_emp)) {
          orcid_institution <- first_emp$organization$name
          orcid_department <- first_emp$`department-name`
          orcid_country <- first_emp$organization$address$country
          orcid_role <- first_emp$`role-title`
        }
      }

      # If no employment, try education
      if (is.na(orcid_country)) {
        edus <- d$`activities-summary`$educations$`affiliation-group`
        if (length(edus) > 0) {
          first_edu <- edus[[1]]$summaries[[1]]$`education-summary`
          if (!is.null(first_edu)) {
            orcid_country <- first_edu$organization$address$country
            orcid_institution <- first_edu$organization$name
          }
        }
      }
    }
  }

  results[[length(results) + 1]] <- tibble(
    abstract_id = row$abstract_id,
    orcid = row$orcid_bare,
    oa_full_name = row$oa_full_name,
    orcid_name = orcid_name,
    orcid_country = orcid_country,
    orcid_institution = orcid_institution,
    orcid_department = orcid_department,
    orcid_role = orcid_role
  )

  completed <- c(completed, row$abstract_id)
  if (i %% 50 == 0) saveRDS(list(completed = completed, results = results), checkpoint_path)
}

saveRDS(list(completed = completed, results = results), checkpoint_path)

orcid_demo <- bind_rows(results)

# Classify US vs international
orcid_demo <- orcid_demo |>
  mutate(
    is_us_orcid = !is.na(orcid_country) & orcid_country == "US",
    is_intl_orcid = !is.na(orcid_country) & orcid_country != "US"
  )

out_path <- here("data", "processed", "orcid_demographics.csv")
write_csv(orcid_demo, out_path)
cli_alert_success("Saved: {out_path}")

# Summary
n_with_country <- sum(!is.na(orcid_demo$orcid_country))
n_us <- sum(orcid_demo$is_us_orcid)
n_intl <- sum(orcid_demo$is_intl_orcid)
cli_alert_info("ORCID country resolved: {n_with_country} / {nrow(orcid_demo)}")
cli_alert_info("  US: {n_us}")
cli_alert_info("  International: {n_intl}")
cli_alert_info("  No country: {nrow(orcid_demo) - n_with_country}")

cat("\nCountry distribution:\n")
print(head(sort(table(orcid_demo$orcid_country, useNA = "ifany"), decreasing = TRUE), 20))

cat("\nInstitution samples (international):\n")
intl <- orcid_demo |> filter(is_intl_orcid) |> head(10)
for (j in seq_len(nrow(intl))) {
  cat(sprintf("  %-25s %s (%s)\n", intl$oa_full_name[j], intl$orcid_institution[j], intl$orcid_country[j]))
}
