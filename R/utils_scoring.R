# utils_scoring.R — Match scoring algorithm for abstract-to-publication matching

library(stringdist)
library(dplyr)
library(stringr)

source(here::here("R", "utils_text.R"))
source(here::here("R", "utils_congresses.R"))

#' @title Score a Candidate Publication Against a Conference Abstract
#'
#' @description
#' Computes a multi-component match score comparing a candidate PubMed
#' publication to a conference abstract. The score combines title similarity,
#' abstract semantic similarity, author matching, journal fit, keyword overlap,
#' and publication timing relative to the conference.
#'
#' @param abstract Named list or single-row data frame. The conference abstract,
#'   with fields: \code{title}, \code{abstract_text},
#'   \code{first_author_normalized}, \code{last_author_normalized},
#'   \code{all_authors_str} (semicolon-separated), \code{keywords} (character
#'   vector), \code{congress_year} (integer or \code{NULL}).
#' @param candidate Named list or single-row data frame. A PubMed candidate
#'   article, with fields: \code{pub_title}, \code{pub_abstract},
#'   \code{pub_first_author}, \code{pub_last_author}, \code{pub_all_authors},
#'   \code{pub_journal}, \code{pub_keywords}, \code{pub_year},
#'   \code{pub_month}, \code{pub_day}.
#' @param cfg List or \code{NULL}. Parsed config object (auto-loaded when
#'   \code{NULL}). Must contain a \code{scoring} sub-list with thresholds:
#'   \code{title_jaccard_high}, \code{title_jaccard_mid},
#'   \code{title_jaccard_low}, \code{abstract_semantic_high},
#'   \code{abstract_semantic_mid}, \code{author_fuzzy_threshold},
#'   \code{pub_date_early_months}, \code{pub_date_late_months},
#'   \code{pre_conference_penalty}.
#'
#' @return Named list with component scores and a \code{total} field:
#' \describe{
#'   \item{title_sim}{Raw Jaccard similarity (0–1).}
#'   \item{title_points}{0, 1, 2, or 3.}
#'   \item{abstract_semantic}{Raw cosine-TF similarity (0–1).}
#'   \item{abstract_points}{0, 1, or 2.}
#'   \item{first_author_points}{0, 1, or 2.}
#'   \item{last_author_points}{0, 1, or 2.}
#'   \item{coauthor_points}{0 or 1.}
#'   \item{author_team_bonus}{0 or 1 (awarded when first-author matches and
#'     coauthor overlap >= 2).}
#'   \item{journal_points}{0–1 Jaro-Winkler similarity to OB/GYN journals.}
#'   \item{keyword_points}{0 or 1.}
#'   \item{date_points}{Positive, zero, or \code{pre_conference_penalty}.}
#'   \item{no_text_penalty}{-2 if neither title nor abstract evidence present.}
#'   \item{total}{Numeric sum of all component scores.}
#' }
#'
#' @details
#' The no-text-evidence penalty (\code{-2}) prevents purely author-based
#' coincidental matches from being accepted when title/abstract similarity
#' is below the detection threshold. Pre-conference publications receive a
#' configurable negative penalty because abstracts cannot be published before
#' their conference presentation. Coauthor overlap uses semicolon-split
#' \code{all_authors_str} (the CSV-serializable form) rather than the list
#' column dropped at save time.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' score_match(abstracts[1, ], candidates[3, ], cfg)
#' }
#'
#' @seealso \code{\link{score_abstract_candidates}}, \code{\link{classify_match}},
#'   \code{\link{jaccard_similarity}}, \code{\link{compute_text_similarity}}
#' @export
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
  # all_authors_normalized is a list column dropped at CSV save time;
  # use all_authors_str (semicolon-separated) which is what the CSV retains.
  abs_auth_str <- abstract$all_authors_str %||% NA_character_
  if (!is.na(abs_auth_str) && nchar(abs_auth_str) > 0 &&
      !is.na(candidate$pub_all_authors)) {
    abs_authors <- str_split(abs_auth_str, ";\\s*")[[1]]
    abs_authors <- abs_authors[nchar(abs_authors) > 0]
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

#' @title Compute Text Similarity Using TF-Weighted Cosine Similarity
#'
#' @description
#' Computes a cosine similarity score between two text strings using term-
#' frequency vectors over a shared vocabulary, after removing stopwords and
#' short tokens. Serves as a lightweight alternative to full embedding models
#' for abstract-to-publication semantic matching.
#'
#' @param text_a Character scalar. First text (abstract or title).
#' @param text_b Character scalar. Second text (candidate publication).
#'
#' @return Numeric scalar in \code{[0, 1]}. Returns \code{0} when either
#'   input is \code{NA}, when all tokens are removed by the stopword filter,
#'   or when either TF vector has zero norm.
#'
#' @details
#' Preprocessing applies \code{\link{normalize_title}()} (lowercase, remove
#' punctuation, collapse whitespace), removes tokens shorter than 3 characters,
#' and strips an internal list of ~60 common medical and structural stopwords
#' (e.g., "patients", "results", "methods", "background"). The cosine
#' similarity is computed as \eqn{\frac{\mathbf{a} \cdot \mathbf{b}}
#' {|\mathbf{a}||\mathbf{b}|}} over the union vocabulary. No IDF weighting
#' is applied (the corpus is too small to estimate reliable document
#' frequencies).
#'
#' @examples
#' \dontrun{
#' compute_text_similarity(
#'   "Laparoscopic hysterectomy reduces blood loss and operative time.",
#'   "Minimally invasive hysterectomy is associated with decreased blood loss."
#' )
#' # ~ 0.35
#' }
#'
#' @seealso \code{\link{score_match}}, \code{\link{jaccard_similarity}},
#'   \code{\link{normalize_title}}
#' @export
compute_text_similarity <- function(text_a, text_b) {
  if (is.na(text_a) || is.na(text_b)) return(0)

  .stop_words <- c(
    "the", "and", "for", "are", "was", "were", "has", "had", "have",
    "been", "with", "this", "that", "from", "not", "but", "its",
    "they", "their", "our", "which", "who", "also", "more", "than",
    "study", "patients", "results", "methods", "conclusion", "objective",
    "background", "design", "setting", "intervention", "measurements",
    "compared", "group", "groups", "using", "used", "total", "mean",
    "median", "data", "analysis", "performed", "included", "significant",
    "significantly", "associated", "respectively", "between", "after",
    "before", "during", "about", "into", "through", "over", "under"
  )

  # Tokenize and strip stop words so common structural terms don't inflate scores
  words_a <- normalize_title(text_a) |> str_split("\\s+") |> unlist()
  words_b <- normalize_title(text_b) |> str_split("\\s+") |> unlist()
  words_a <- words_a[nchar(words_a) >= 3 & !words_a %in% .stop_words]
  words_b <- words_b[nchar(words_b) >= 3 & !words_b %in% .stop_words]

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

#' @title Classify a Match Score Using Cochrane-Aligned Vocabulary
#'
#' @description
#' Maps a numeric match score to a Cochrane-aligned match-confidence category.
#' Categories follow the vocabulary recommended in Cochrane Methodology Review
#' MR000005 for tracking abstract-to-publication linkage certainty.
#'
#' @param score Numeric scalar. The \code{total} field from
#'   \code{\link{score_match}()}.
#' @param cfg List or \code{NULL}. Parsed config object (auto-loaded when
#'   \code{NULL}). Must contain \code{scoring$auto_accept} and
#'   \code{scoring$manual_review} thresholds.
#' @param has_text_evidence Logical scalar. \code{TRUE} if the candidate has
#'   at least one point from title or abstract similarity (\code{title_pts >= 1}
#'   or \code{abstract_pts >= 1}). Prevents author-only matches from reaching
#'   \code{"definite"} or \code{"probable"}. Defaults to \code{TRUE}.
#' @param pre_conference Logical scalar. \code{TRUE} if the best candidate was
#'   published before the conference date. Forces \code{"excluded"} regardless
#'   of score. Defaults to \code{FALSE}.
#'
#' @return Character scalar. One of \code{"definite"}, \code{"probable"},
#'   \code{"possible"}, \code{"no_match"}, or \code{"excluded"}.
#'
#' @details
#' Classification logic (in order):
#' \enumerate{
#'   \item \code{pre_conference} TRUE → \code{"excluded"}.
#'   \item \code{score >= auto_accept} AND \code{has_text_evidence} →
#'     \code{"definite"}.
#'   \item \code{score >= manual_review} AND \code{has_text_evidence} →
#'     \code{"probable"}.
#'   \item \code{score >= manual_review} (no text evidence) →
#'     \code{"possible"}.
#'   \item Otherwise → \code{"no_match"}.
#' }
#' Ties between candidates further demote \code{"definite"} to
#' \code{"probable"} in \code{\link{score_abstract_candidates}()}.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' classify_match(8.5, cfg, has_text_evidence = TRUE)   # "definite"
#' classify_match(5.0, cfg, has_text_evidence = FALSE)  # "possible"
#' classify_match(2.0, cfg)                             # "no_match"
#' classify_match(9.0, cfg, pre_conference = TRUE)      # "excluded"
#' }
#'
#' @seealso \code{\link{score_match}}, \code{\link{score_abstract_candidates}}
#' @export
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

#' @title Score All Candidates for One Abstract and Return the Best Match
#'
#' @description
#' Applies \code{\link{score_match}()} to every candidate publication in
#' \code{candidates_df}, selects the highest-scoring candidate, and returns
#' its classification alongside score details for all candidates.
#'
#' @param abstract_row Single-row data frame or named list. The conference
#'   abstract, compatible with \code{\link{score_match}()}.
#' @param candidates_df Data frame. Candidate PubMed publications to score
#'   against \code{abstract_row}. Each row is passed to
#'   \code{\link{score_match}()} individually. An empty data frame causes an
#'   immediate return with classification \code{"no_candidates"}.
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row containing:
#' \describe{
#'   \item{abstract_id}{The abstract's ID from \code{abstract_row}.}
#'   \item{best_pmid}{PMID of the top-scoring candidate (character).}
#'   \item{best_score}{Total score of the best candidate (numeric).}
#'   \item{classification}{Cochrane-aligned label from
#'     \code{\link{classify_match}()}.}
#'   \item{has_tie}{Logical; \code{TRUE} if two candidates share the top
#'     score (causes \code{"definite"} to demote to \code{"probable"}).}
#'   \item{n_candidates}{Integer; total number of candidates scored.}
#'   \item{score_details}{List-column containing a tibble of all candidate
#'     scores, sorted descending by \code{total_score}.}
#' }
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' result <- score_abstract_candidates(abstracts[1, ], candidate_details, cfg)
#' result$classification
#' result$score_details[[1]]
#' }
#'
#' @seealso \code{\link{score_match}}, \code{\link{classify_match}}
#' @export
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

  all_scores <- purrr::map(seq_len(nrow(candidates_df)), function(i) {
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
  }) |> purrr::list_rbind()

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
