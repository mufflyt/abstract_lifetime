# utils_text.R — Text normalization and keyword extraction utilities

library(stringr)
library(stringdist)
library(dplyr)

#' Normalize a title for comparison
#' Lowercase, strip punctuation, collapse whitespace
normalize_title <- function(title) {
  title |>
    tolower() |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

#' Normalize an author name to "LastName FI" format
#' Handles hyphens, diacritics, particles (van, de, von, etc.)
normalize_author <- function(name) {
  if (is.na(name) || name == "") return(NA_character_)

  name <- stringi::stri_trans_general(name, "Latin-ASCII")
  name <- str_squish(name)

  # Try "Last, First" format
  if (str_detect(name, ",")) {
    parts <- str_split(name, ",\\s*", n = 2)[[1]]
    last <- parts[1]
    first_initial <- toupper(substr(str_squish(parts[2]), 1, 1))
  } else {
    tokens <- str_split(name, "\\s+")[[1]]
    if (length(tokens) == 1) return(tolower(tokens[1]))

    # Detect "FI LastName" format (e.g., "J Hayden", "DA Lum")
    # First token is 1-3 uppercase chars = likely initials
    first_is_initials <- nchar(tokens[1]) <= 3 &&
      str_detect(tokens[1], "^[A-Z]+$")

    # Detect "LastName FI" format (e.g., "Johannesson U", "Smith AB")
    # Last token is 1-3 uppercase chars = likely initials
    last_is_initials <- nchar(tokens[length(tokens)]) <= 3 &&
      str_detect(tokens[length(tokens)], "^[A-Z]+$")

    if (first_is_initials && length(tokens) == 2) {
      # "J Hayden" -> last = "hayden", initial = "J"
      last <- tokens[2]
      first_initial <- substr(tokens[1], 1, 1)
    } else if (last_is_initials && length(tokens) >= 2) {
      # "Johannesson U" -> last = "johannesson", initial = "U"
      # Also handles "van der Berg AB" -> last = "van der Berg", initial = "A"
      last <- paste(tokens[1:(length(tokens) - 1)], collapse = " ")
      first_initial <- substr(tokens[length(tokens)], 1, 1)
    } else {
      # Handle particles: "van der Berg" -> last = "van der Berg"
      particles <- c("van", "de", "von", "del", "la", "le", "di", "el", "al")
      particle_start <- which(tolower(tokens) %in% particles)
      if (length(particle_start) > 0 && particle_start[1] > 1) {
        last <- paste(tokens[particle_start[1]:length(tokens)], collapse = " ")
        first_initial <- toupper(substr(tokens[1], 1, 1))
      } else {
        last <- tokens[length(tokens)]
        first_initial <- toupper(substr(tokens[1], 1, 1))
      }
    }
  }

  paste0(tolower(last), " ", toupper(first_initial))
}

#' Normalize a vector of author names
normalize_authors <- function(names_vec) {
  vapply(names_vec, normalize_author, character(1), USE.NAMES = FALSE)
}

#' Extract TF-IDF keywords from abstract text
#' Returns top_n keywords per document
extract_keywords <- function(texts, top_n = 10) {
  stopwords <- c(
    "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
    "of", "with", "by", "from", "is", "was", "were", "are", "been", "be",
    "have", "has", "had", "do", "does", "did", "will", "would", "could",
    "should", "may", "might", "shall", "can", "this", "that", "these",
    "those", "it", "its", "we", "our", "they", "their", "not", "no",
    "than", "as", "if", "when", "which", "who", "whom", "there", "each",
    "all", "both", "more", "most", "other", "some", "such", "only",
    "same", "so", "also", "very", "just", "because", "between", "after",
    "before", "during", "about", "into", "through", "over", "under",
    # Common medical/abstract words
    "study", "patients", "results", "methods", "conclusion", "objective",
    "background", "design", "setting", "intervention", "measurements",
    "compared", "group", "groups", "using", "used", "total", "mean",
    "median", "data", "analysis", "performed", "included", "significant",
    "significantly", "associated", "respectively", "p", "ci", "vs"
  )

  # Tokenize and compute TF-IDF
  lapply(texts, function(text) {
    if (is.na(text) || text == "") return(character(0))
    words <- text |>
      tolower() |>
      str_replace_all("[^a-z0-9\\s]", " ") |>
      str_squish() |>
      str_split("\\s+") |>
      unlist()
    words <- words[nchar(words) >= 3 & !words %in% stopwords]
    # Simple TF approach (IDF would need corpus)
    freq <- sort(table(words), decreasing = TRUE)
    head(names(freq), top_n)
  })
}

#' Compute Jaccard similarity on word tokens
jaccard_similarity <- function(a, b) {
  if (is.na(a) || is.na(b)) return(0)
  tokens_a <- unique(str_split(normalize_title(a), "\\s+")[[1]])
  tokens_b <- unique(str_split(normalize_title(b), "\\s+")[[1]])
  tokens_a <- tokens_a[nchar(tokens_a) > 0]
  tokens_b <- tokens_b[nchar(tokens_b) > 0]
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0)
  intersection <- length(intersect(tokens_a, tokens_b))
  union <- length(union(tokens_a, tokens_b))
  intersection / union
}

#' Find the most distinctive n-gram in a title (for PubMed search)
#' Returns a phrase of 3-5 words that is least common
distinctive_phrase <- function(title, n_words = 4) {
  words <- normalize_title(title) |>
    str_split("\\s+") |>
    unlist()
  words <- words[nchar(words) >= 3]
  if (length(words) <= n_words) return(paste(words, collapse = " "))

  # Skip very common leading words, prefer middle/end of title
  common_starts <- c("effect", "effects", "impact", "role", "comparison",
                     "evaluation", "assessment", "outcomes", "incidence",
                     "prevalence", "retrospective", "prospective")
  # Find the best window
  best_start <- 1
  for (i in seq_len(length(words) - n_words + 1)) {
    window <- words[i:(i + n_words - 1)]
    if (!any(window[1] == common_starts) && all(nchar(window) >= 3)) {
      best_start <- i
      break
    }
  }
  paste(words[best_start:(best_start + n_words - 1)], collapse = " ")
}

#' Compute MD5 hash of title + first author for dedup tracking
abstract_hash <- function(title, first_author) {
  digest::digest(paste0(normalize_title(title), "|",
                        normalize_author(first_author)),
                 algo = "md5")
}
