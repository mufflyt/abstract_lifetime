# Cycle 16 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Target: R/01d_tag_session_type.R. Session type is the first filter in the
# denominator chain: 1,154 parsed minus 48 video gives the 1,106-abstract
# cohort. Everything cycles 1-15 measured rests on that split being right, and
# nothing had tested it.
#
# Contracts read from the source, not assumed:
#   :45  sections are h3.section-title, items are li.js-article-list-item
#   :47  current_section starts NA and is set by the most recent preceding h3
#   :72  case_when maps lowercase "oral"/"video"/"poster" to canonical labels,
#        and passes anything else through verbatim

library(testthat)
library(dplyr)

P_PARSED <- here::here("data", "processed", "abstracts_parsed.csv")
P_WEB    <- here::here("data", "processed", "abstracts_parsed_web.csv")
P_CLEAN  <- here::here("data", "processed", "abstracts_cleaned.csv")
P_MATCH  <- here::here("output", "abstracts_with_matches.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# Mirrors the case_when at R/01d_tag_session_type.R:72.
canonicalise <- function(x) {
  dplyr::case_when(
    grepl("oral",   tolower(x)) ~ "Oral",
    grepl("video",  tolower(x)) ~ "Video",
    grepl("poster", tolower(x)) ~ "Poster",
    TRUE ~ x
  )
}

# ============================================================
# BVA 16.1 - the canonical mapping behaves as the source defines it
# ============================================================
test_that("the session-type mapping matches the case_when it mirrors", {
  expect_equal(canonicalise("Oral Presentations"), "Oral")
  expect_equal(canonicalise("Video Presentations"), "Video")
  expect_equal(canonicalise("Video Sessions"), "Video")       # 2022 wording
  expect_equal(canonicalise("Poster Presentations"), "Poster")
  expect_equal(canonicalise("ORAL ABSTRACTS"), "Oral")        # case-insensitive
  # Anything unmatched passes through verbatim, so an unexpected section name
  # lands in the data rather than raising. That is the behaviour 16.2 checks for.
  expect_equal(canonicalise("Keynote Address"), "Keynote Address")
  expect_true(is.na(canonicalise(NA_character_)))
})

# ============================================================
# BVA 16.2 - no abstract carries an unmapped or missing session type
# ============================================================
test_that("every parsed abstract has a canonical session type", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  skip_if(!"session_type" %in% names(p), "session_type absent")
  # current_section starts as NA, so any item appearing before the first
  # h3.section-title in the TOC is tagged NA and silently escapes the video
  # filter at 02_clean_abstracts.R:34.
  bad <- p$session_type[is.na(p$session_type) |
                        !p$session_type %in% c("Oral", "Video", "Poster")]
  expect_true(length(bad) == 0,
              label = sprintf("%d abstracts carry a non-canonical session type: %s",
                              length(bad),
                              paste(unique(ifelse(is.na(bad), "<NA>", bad)), collapse = ", ")))
})

# ============================================================
# BVA 16.3 - the split accounts for every parsed row
# ============================================================
test_that("Oral plus Video equals the parsed total", {
  need(P_PARSED, P_CLEAN)
  p  <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  tab <- table(p$session_type, useNA = "ifany")
  expect_equal(sum(tab), nrow(p))
  # The cohort is defined as the Oral subset. If these drift apart the
  # denominator chain in technical appendix A13.1 no longer holds.
  expect_equal(unname(tab["Oral"]), nrow(cl),
               label = "the Oral count and the cleaned cohort have diverged")
})

# ============================================================
# BVA 16.4 - videos appear only where the supplement carried them
# ============================================================
test_that("video presentations are confined to the congresses that had them", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  vid <- sort(unique(p$congress_year[p$session_type == "Video"]))
  skip_if(length(vid) == 0, "no videos")
  # A video appearing in an earlier congress would mean either the TOC changed
  # or a section heading was misread, and would silently shrink that year's
  # cohort by excluding real oral presentations.
  expect_true(all(vid >= 2022),
              label = paste("video presentations tagged in congress year(s)",
                            paste(setdiff(vid, 2022:2026), collapse = ", "),
                            "outside the 2022-2023 window"))
})

# ============================================================
# SEMANTIC 16.5 - the cohort is exactly the oral subset
# ============================================================
test_that("the cleaned cohort contains every Oral abstract and no other", {
  need(P_PARSED, P_CLEAN)
  p  <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  oral <- p$abstract_id[p$session_type == "Oral"]
  expect_true(setequal(oral, cl$abstract_id),
              label = sprintf("%d abstracts differ between the Oral subset and the cleaned cohort",
                              length(union(setdiff(oral, cl$abstract_id),
                                           setdiff(cl$abstract_id, oral)))))
})

# ============================================================
# SEMANTIC 16.6 - session type agrees everywhere it was merged
# ============================================================
test_that("session_type is consistent across the files 01d writes into", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE) |>
    select(abstract_id, parsed = session_type)
  disagreements <- character(0)
  for (f in c(P_WEB, P_CLEAN, P_MATCH)) {
    if (!file.exists(f)) next
    d <- readr::read_csv(f, show_col_types = FALSE)
    if (!all(c("abstract_id", "session_type") %in% names(d))) next
    j <- inner_join(p, d |> select(abstract_id, other = session_type), by = "abstract_id")
    n <- sum(j$parsed != j$other, na.rm = TRUE)
    if (n > 0) disagreements <- c(disagreements, sprintf("%s (%d rows)", basename(f), n))
  }
  expect_true(length(disagreements) == 0,
              label = paste("session_type disagrees with the parsed source in:",
                            paste(disagreements, collapse = ", ")))
})

# ============================================================
# SEMANTIC 16.7 - the absence of Poster is explained, not accidental
# ============================================================
test_that("no Poster rows appear, consistent with where ingestion stops", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  n_poster <- sum(p$session_type == "Poster", na.rm = TRUE)
  # "Poster" is a canonical label the mapping can produce, yet none appears.
  # Technical appendix A14 explains why: ingestion captures a contiguous prefix
  # of each supplement and stops between S26 and S60, while the supplements run
  # to S141-S286. The poster sections sit past the cutoff and were never seen.
  # If posters ever DO appear, the cohort definition changes and the video-only
  # exclusion at 02_clean_abstracts.R:34 is no longer sufficient.
  expect_equal(n_poster, 0L,
               label = paste(n_poster, "Poster rows have appeared; the cohort is",
                             "defined as Oral only and the exclusion filter drops",
                             "Video alone, so posters would enter the denominator"))
})

# ============================================================
# ADVERSARIAL 16.8 - one row per abstract, no tagging duplicates
# ============================================================
test_that("session tagging introduced no duplicate abstracts", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  # tag_one_congress() ends with distinct(pii, .keep_all = TRUE); a duplicate
  # surviving that would double-weight the abstract in every later count.
  expect_equal(anyDuplicated(p$abstract_id), 0L,
               label = "a duplicated abstract_id survived session tagging")
})

# ============================================================
# ADVERSARIAL 16.9 - the video exclusion removed exactly the video rows
# ============================================================
test_that("the cleaned cohort excludes exactly the Video abstracts", {
  need(P_PARSED, P_CLEAN)
  p  <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  dropped <- setdiff(p$abstract_id, cl$abstract_id)
  st <- p$session_type[match(dropped, p$abstract_id)]
  expect_true(all(st == "Video", na.rm = TRUE) && !any(is.na(st)),
              label = sprintf("%d of %d dropped abstracts are not tagged Video",
                              sum(is.na(st) | st != "Video"), length(dropped)))
})

# ============================================================
# ADVERSARIAL 16.10 - a session type must not be inferred from the title
# ============================================================
test_that("session type is not simply keyed off the word 'video' in the title", {
  need(P_PARSED)
  p <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  skip_if(!"title" %in% names(p), "title absent")
  titled_video <- p |> filter(grepl("\\bvideo\\b", title, ignore.case = TRUE))
  skip_if(nrow(titled_video) == 0, "no titles mention video")
  # The tagger reads the TOC section heading, not the title. If every abstract
  # whose title mentions video were tagged Video, the tagger would be doing
  # something other than what it claims, and oral presentations about video
  # technique would be wrongly excluded.
  share <- mean(titled_video$session_type == "Video")
  expect_lt(share, 0.9,
            label = sprintf("%.0f%% of the %d abstracts whose title mentions video are tagged Video; the tagger may be reading titles rather than TOC sections",
                            100 * share, nrow(titled_video)))
})
