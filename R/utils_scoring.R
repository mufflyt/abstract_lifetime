# utils_scoring.R — Match scoring algorithm for abstract-to-publication matching

library(stringdist)
library(dplyr)
library(stringr)

source(here::here("R", "utils_text.R"))

#' Score a candidate publication against an abstract
#' Returns a named list with total score and component scores
score_match <- function(abstract, candidate, cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))
  sc <- cfg$scoring

  scores <- list(
    title_sim = 0,
    title_points = 0,
    abstract_semantic = 0,
    abstract_points = 0,
    first_author_points = 0,
    last_author_points = 0,
    coauthor_points = 0,
    author_team_bonus = 0,
    journal_points = 0,
    keyword_points = 0,
    date_points = 0,
    total = 0
  )

  # --- Title similarity ---
  title_sim <- jaccard_similarity(abstract$title, candidate$pub_title)
  scores$title_sim <- round(title_sim, 3)
  if (title_sim >= sc$title_jaccard_high) {
    scores$title_points <- 3
  } else if (title_sim >= sc$title_jaccard_mid) {
    scores$title_points <- 2
  } else if (title_sim >= sc$title_jaccard_low) {
    scores$title_points <- 1
  }

  # --- Abstract semantic similarity ---
  if (!is.na(abstract$abstract_text) && !is.na(candidate$pub_abstract) &&
      nchar(abstract$abstract_text) > 20 && nchar(candidate$pub_abstract) > 20) {
    abs_sim <- compute_text_similarity(abstract$abstract_text, candidate$pub_abstract)
    scores$abstract_semantic <- round(abs_sim, 3)
    if (abs_sim >= sc$abstract_semantic_high) {
      scores$abstract_points <- 2
    } else if (abs_sim >= sc$abstract_semantic_mid) {
      scores$abstract_points <- 1
    }
  }

  # --- First author ---
  if (!is.na(abstract$first_author_normalized) && !is.na(candidate$pub_first_author)) {
    cand_first_norm <- normalize_author(candidate$pub_first_author)
    if (!is.na(cand_first_norm) && !is.na(abstract$first_author_normalized)) {
      if (tolower(abstract$first_author_normalized) == tolower(cand_first_norm)) {
        scores$first_author_points <- 2
      } else {
        jw <- 1 - stringdist::stringdist(
          tolower(abstract$first_author_normalized),
          tolower(cand_first_norm),
          method = "jw"
        )
        if (jw >= sc$author_fuzzy_threshold) {
          scores$first_author_points <- 1
        }
      }
    }
  }

  # --- Last author ---
  if (!is.na(abstract$last_author_normalized) && !is.na(candidate$pub_last_author)) {
    cand_last_norm <- normalize_author(candidate$pub_last_author)
    if (!is.na(cand_last_norm) && !is.na(abstract$last_author_normalized)) {
      if (tolower(abstract$last_author_normalized) == tolower(cand_last_norm)) {
        scores$last_author_points <- 2
      } else {
        jw <- 1 - stringdist::stringdist(
          tolower(abstract$last_author_normalized),
          tolower(cand_last_norm),
          method = "jw"
        )
        if (jw >= sc$author_fuzzy_threshold) {
          scores$last_author_points <- 1
        }
      }
    }
  }

  # --- Coauthor overlap ---
  if (!is.null(abstract$all_authors_normalized) && !is.na(candidate$pub_all_authors)) {
    abs_authors <- abstract$all_authors_normalized
    cand_authors <- str_split(candidate$pub_all_authors, ";\\s*")[[1]]
    cand_authors_norm <- normalize_authors(cand_authors)
    cand_authors_norm <- cand_authors_norm[!is.na(cand_authors_norm)]

    if (length(abs_authors) > 0 && length(cand_authors_norm) > 0) {
      overlap <- sum(tolower(abs_authors) %in% tolower(cand_authors_norm))
      if (overlap >= 2) {
        scores$coauthor_points <- 1
        # Author team bonus: upgrade first author if team matches
        if (scores$first_author_points >= 1) {
          scores$author_team_bonus <- 1  # bumps first_author effective to 3
        }
      }
    }
  }

  # --- Journal similarity ---
  if (!is.na(candidate$pub_journal)) {
    obgyn_journals <- c(
      "journal of minimally invasive gynecology", "fertility and sterility",
      "american journal of obstetrics and gynecology",
      "obstetrics and gynecology", "human reproduction",
      "gynecologic oncology", "european journal of obstetrics",
      "bjog", "international journal of gynecology",
      "journal of gynecologic surgery", "reproductive sciences",
      "journal of reproductive medicine"
    )
    journal_lower <- tolower(candidate$pub_journal)
    journal_sims <- vapply(obgyn_journals, function(j) {
      1 - stringdist::stringdist(journal_lower, j, method = "jw")
    }, numeric(1))
    scores$journal_points <- round(max(journal_sims), 2)
  }

  # --- Keyword overlap ---
  if (!is.null(abstract$keywords) && !is.na(candidate$pub_keywords)) {
    abs_kw <- tolower(abstract$keywords)
    cand_kw <- tolower(str_split(candidate$pub_keywords, ";\\s*")[[1]])
    kw_overlap <- length(intersect(abs_kw, cand_kw))
    if (kw_overlap >= 3) {
      scores$keyword_points <- 1
    }
  }

  # --- Publication date ---
  # Prefer the per-abstract congress_year when scoring multi-cohort data.
  conference_date <- if (!is.null(abstract$congress_year) &&
                         !is.na(abstract$congress_year[1])) {
    source(here::here("R", "utils_congresses.R"), local = TRUE)
    conference_date_for(abstract$congress_year[1], cfg)
  } else {
    as.Date(cfg$conference$date)
  }
  pub_date <- tryCatch({
    month_num <- match(candidate$pub_month, month.abb)
    if (is.na(month_num)) month_num <- as.integer(candidate$pub_month)
    if (is.na(month_num)) month_num <- 1
    day <- candidate$pub_day
    if (is.na(day) || day == "") day <- "01"
    as.Date(sprintf("%s-%02d-%s", candidate$pub_year, month_num, day))
  }, error = function(e) NA)

  if (!is.na(pub_date)) {
    months_diff <- as.numeric(difftime(pub_date, conference_date, units = "days")) / 30.44

    if (months_diff < 0) {
      # Published BEFORE conference — hard penalty
      scores$date_points <- sc$pre_conference_penalty
    } else if (months_diff <= sc$pub_date_early_months) {
      scores$date_points <- 1
    } else if (months_diff <= sc$pub_date_late_months) {
      scores$date_points <- 0.5
    }
  }

  # --- No-title-evidence penalty ---
  # If there is zero title/abstract textual overlap, author + journal + date

  # alone are not sufficient evidence — likely a coincidental name match.
  scores$no_text_penalty <- 0
  has_title_evidence <- scores$title_points >= 1 || scores$title_sim >= 0.20
  has_abstract_evidence <- scores$abstract_points >= 1
  if (!has_title_evidence && !has_abstract_evidence) {
    scores$no_text_penalty <- -2
  }

  # --- Total ---
  scores$total <- scores$title_points + scores$abstract_points +
    scores$first_author_points + scores$last_author_points +
    scores$coauthor_points + scores$author_team_bonus +
    scores$journal_points + scores$keyword_points + scores$date_points +
    scores$no_text_penalty

  scores
}

#' Compute text similarity using TF-IDF + cosine
#' Lightweight alternative to full text2vec embeddings
compute_text_similarity <- function(text_a, text_b) {
  if (is.na(text_a) || is.na(text_b)) return(0)

  # Tokenize
  words_a <- normalize_title(text_a) |> str_split("\\s+") |> unlist()
  words_b <- normalize_title(text_b) |> str_split("\\s+") |> unlist()
  words_a <- words_a[nchar(words_a) >= 3]
  words_b <- words_b[nchar(words_b) >= 3]

  if (length(words_a) == 0 || length(words_b) == 0) return(0)

  # Build vocabulary
  vocab <- unique(c(words_a, words_b))

  # Term frequency vectors
  tf_a <- vapply(vocab, function(w) sum(words_a == w), integer(1))
  tf_b <- vapply(vocab, function(w) sum(words_b == w), integer(1))

  # Cosine similarity
  dot <- sum(tf_a * tf_b)
  norm_a <- sqrt(sum(tf_a^2))
  norm_b <- sqrt(sum(tf_b^2))

  if (norm_a == 0 || norm_b == 0) return(0)
  dot / (norm_a * norm_b)
}

#' Classify match using Cochrane-aligned vocabulary
#' Returns "definite", "probable", "possible", "no_match", or "excluded"
#' @param has_text_evidence Logical — TRUE if title_pts >= 1 or abstract_pts >= 1
#' @param pre_conference Logical — TRUE if best candidate published before conference
classify_match <- function(score, cfg = NULL, has_text_evidence = TRUE,
                           pre_conference = FALSE) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))
  sc <- cfg$scoring

  if (pre_conference) return("excluded")
  if (score >= sc$auto_accept && has_text_evidence) return("definite")
  if (score >= sc$manual_review && has_text_evidence) return("probable")
  if (score >= sc$manual_review) return("possible")
  "no_match"
}

#' Score all candidates for one abstract and return best match + classification
score_abstract_candidates <- function(abstract_row, candidates_df, cfg = NULL) {
  if (nrow(candidates_df) == 0) {
    return(tibble::tibble(
      abstract_id = abstract_row$abstract_id,
      best_pmid = NA_character_,
      best_score = 0,
      classification = "no_candidates",
      has_tie = FALSE,
      n_candidates = 0L,
      score_details = list(NULL)
    ))
  }

  all_scores <- purrr::map_dfr(seq_len(nrow(candidates_df)), function(i) {
    cand <- candidates_df[i, ]
    sc <- score_match(abstract_row, cand, cfg)
    tibble::tibble(
      pmid = cand$pmid,
      total_score = sc$total,
      title_sim = sc$title_sim,
      title_pts = sc$title_points,
      abstract_pts = sc$abstract_points,
      first_au_pts = sc$first_author_points,
      last_au_pts = sc$last_author_points,
      coauthor_pts = sc$coauthor_points,
      team_bonus = sc$author_team_bonus,
      journal_pts = sc$journal_points,
      keyword_pts = sc$keyword_points,
      date_pts = sc$date_points,
      no_text_penalty = sc$no_text_penalty
    )
  })

  # Sort by score

  all_scores <- all_scores |> arrange(desc(total_score))
  best <- all_scores[1, ]

  # Check for ties
  has_tie <- nrow(all_scores) > 1 && all_scores$total_score[1] == all_scores$total_score[2]

  has_text_evidence <- best$title_pts >= 1 || best$abstract_pts >= 1
  pre_conference <- !is.na(best$date_pts) && best$date_pts < 0

  classification <- classify_match(best$total_score, cfg,
                                    has_text_evidence = has_text_evidence,
                                    pre_conference = pre_conference)

  # Ties demote definite → probable (human must pick between candidates)
  if (has_tie && classification == "definite") {
    classification <- "probable"
  }

  tibble::tibble(
    abstract_id = abstract_row$abstract_id,
    best_pmid = as.character(best$pmid),
    best_score = best$total_score,
    classification = classification,
    has_tie = has_tie,
    n_candidates = nrow(candidates_df),
    score_details = list(all_scores)
  )
}
