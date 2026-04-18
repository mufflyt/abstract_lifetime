# utils_text.R — Text normalization and keyword extraction utilities

library(stringr)
library(stringdist)
library(dplyr)

#' @title Normalize a Title String for Text Comparison
#'
#' @description
#' Converts a title to lowercase, removes all non-alphanumeric characters
#' (replacing them with spaces), and collapses runs of whitespace. Produces
#' a clean token-comparable string for use in Jaccard and cosine similarity
#' computations.
#'
#' @param title Character scalar. Raw or pre-processed title text.
#'
#' @return Character scalar. Lowercase, punctuation-free, whitespace-normalized
#'   title string.
#'
#' @examples
#' \dontrun{
#' normalize_title("Robot-Assisted Hysterectomy: A Retrospective Review")
#' # "robot assisted hysterectomy a retrospective review"
#' }
#'
#' @seealso \code{\link{jaccard_similarity}}, \code{\link{compute_text_similarity}},
#'   \code{\link{normalize_author}}
#' @export
normalize_title <- function(title) {
  title |>
    tolower() |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

#' @title Normalize an Author Name to "LastName FI" Format
#'
#' @description
#' Converts a wide variety of author name formats (Last-First, First-Last,
#' initials-last, particles) into the canonical \code{"lastname fi"} form
#' (lowercase last name + uppercase first initial). Strips diacritics via
#' Latin-ASCII transliteration.
#'
#' @param name Character scalar. A raw author name string in any common
#'   bibliographic format, including:
#'   \itemize{
#'     \item \code{"Smith JA"} (last name + initials)
#'     \item \code{"Smith, Jane A."} (Last, First)
#'     \item \code{"JA Smith"} (initials + last)
#'     \item \code{"van der Berg AB"} (name particles)
#'   }
#'
#' @return Character scalar in \code{"lastname fi"} format (e.g.,
#'   \code{"smith J"}, \code{"van der berg A"}), or \code{NA_character_} for
#'   \code{NA} or empty input. Single-word names (no initials detectable) are
#'   returned as lowercase.
#'
#' @details
#' Detection heuristics (in order):
#' \enumerate{
#'   \item Comma present → split as "Last, First" format.
#'   \item First token is 1–3 uppercase chars → "FI LastName" format.
#'   \item Last token is 1–3 uppercase chars → "LastName FI" format (also
#'     handles "van der Berg AB").
#'   \item Particles (\code{van, de, von, del, la, le, di, el, al}) in
#'     the middle → everything from the particle onwards is the last name.
#'   \item Default: last token is last name, first token provides the initial.
#' }
#' Diacritics are transliterated using \code{stringi::stri_trans_general(,
#' "Latin-ASCII")} before parsing.
#'
#' @examples
#' \dontrun{
#' normalize_author("Smith JA")         # "smith J"
#' normalize_author("van der Berg, AB") # "van der berg A"
#' normalize_author("Müller, Hans")     # "muller H"
#' normalize_author(NA_character_)      # NA_character_
#' }
#'
#' @seealso \code{\link{normalize_authors}}, \code{\link{normalize_title}},
#'   \code{\link{abstract_hash}}
#' @export
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

#' @title Normalize a Vector of Author Names
#'
#' @description
#' Vectorized wrapper around \code{\link{normalize_author}()} that applies
#' name normalization to each element of a character vector.
#'
#' @param names_vec Character vector. Author names in any supported format.
#'
#' @return Character vector of the same length as \code{names_vec}, with each
#'   element normalized to \code{"lastname fi"} format (or \code{NA_character_}
#'   for missing/empty elements).
#'
#' @examples
#' \dontrun{
#' normalize_authors(c("Smith JA", "Jones B", "van der Berg M"))
#' # c("smith J", "jones B", "van der berg M")
#' }
#'
#' @seealso \code{\link{normalize_author}}
#' @export
normalize_authors <- function(names_vec) {
  vapply(names_vec, normalize_author, character(1), USE.NAMES = FALSE)
}

#' @title Extract TF-Based Keywords from Abstract Text
#'
#' @description
#' Tokenizes a list of abstract texts, removes stopwords, and returns the top
#' \code{top_n} highest-frequency (term-frequency) keywords for each document.
#' Used to populate the \code{keywords} field on each abstract row for
#' downstream PubMed keyword-strategy queries.
#'
#' @param texts List or character vector. One element per abstract. \code{NULL},
#'   \code{NA}, or empty-string elements return \code{character(0)}.
#' @param top_n Integer scalar. Maximum number of keywords to return per
#'   document. Defaults to \code{10}.
#'
#' @return A list of character vectors, one per input element. Each vector
#'   contains the top \code{top_n} unique terms (3+ characters, not in the
#'   internal stopword list) ordered by descending term frequency.
#'
#' @details
#' The internal stopword list combines ~55 common English function words with
#' ~25 structural medical/abstract terms (e.g., "patients", "methods",
#' "results", "conclusion") that appear in nearly every abstract but carry no
#' discriminative value. No IDF weighting is applied because the corpus is
#' too small for stable document-frequency estimates. For cross-source keyword
#' overlap in scoring, use the returned vectors directly.
#'
#' @examples
#' \dontrun{
#' texts <- list(
#'   "Laparoscopic hysterectomy was associated with reduced blood loss.",
#'   "Robot-assisted myomectomy for symptomatic fibroids: a multicenter study."
#' )
#' extract_keywords(texts, top_n = 5)
#' }
#'
#' @seealso \code{\link{normalize_title}}, \code{\link{distinctive_phrase}}
#' @export
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
    if (is.null(text) || is.na(text) || text == "") return(character(0))
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

#' @title Compute Jaccard Similarity on Word Tokens
#'
#' @description
#' Computes the Jaccard index (intersection over union) between the unique
#' word-token sets of two strings after applying \code{\link{normalize_title}()}.
#' Used as the primary title-similarity metric in the match-scoring pipeline.
#'
#' @param a Character scalar. First string (e.g., conference abstract title).
#' @param b Character scalar. Second string (e.g., PubMed publication title).
#'
#' @return Numeric scalar in \code{[0, 1]}. Returns \code{0} if either input
#'   is \code{NA} or if either normalized token set is empty.
#'
#' @details
#' Tokens are unique (set-based); repeated words do not inflate the score.
#' Empty tokens produced by the normalization regex are excluded. Thresholds
#' for scoring points (\code{title_jaccard_high}, \code{title_jaccard_mid},
#' \code{title_jaccard_low}) are defined in \code{config.yml}.
#'
#' @examples
#' \dontrun{
#' jaccard_similarity(
#'   "Laparoscopic hysterectomy blood loss outcomes",
#'   "Blood loss in laparoscopic hysterectomy: a review"
#' )
#' # ~0.44 (4 shared tokens / 9 union tokens)
#' }
#'
#' @seealso \code{\link{compute_text_similarity}}, \code{\link{normalize_title}},
#'   \code{\link{score_match}}
#' @export
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

#' @title Find the Most Distinctive N-Gram in a Title for PubMed Search
#'
#' @description
#' Extracts a consecutive window of \code{n_words} tokens from a normalized
#' title, preferring windows that avoid generic leading terms. The resulting
#' phrase is used as Strategy 5 in \code{\link{build_search_strategies}()}
#' (\code{[TIAB]} PubMed query) to maximize search specificity.
#'
#' @param title Character scalar. The raw conference abstract title.
#' @param n_words Integer scalar. Desired phrase length in words. Defaults to
#'   \code{4}.
#'
#' @return Character scalar. A space-separated phrase of up to \code{n_words}
#'   tokens. Returns all available tokens if the title has fewer than
#'   \code{n_words} meaningful words.
#'
#' @details
#' The function skips the first window that starts with a common generic verb
#' or noun (e.g., "effect", "impact", "outcomes", "retrospective",
#' "prospective", "evaluation") and selects the next valid window. All tokens
#' must be at least 3 characters long after normalization. The heuristic
#' favors middle/end portions of the title which typically carry more specific
#' procedural or anatomical terms.
#'
#' @examples
#' \dontrun{
#' distinctive_phrase("Outcomes of Robotic Sacrocolpopexy in Obese Patients")
#' # "robotic sacrocolpopexy obese patients"  (skips generic "outcomes")
#'
#' distinctive_phrase("Prospective Cohort Study of Laparoscopic Myomectomy")
#' # "cohort study laparoscopic myomectomy"
#' }
#'
#' @seealso \code{\link{build_search_strategies}}, \code{\link{normalize_title}}
#' @export
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

#' @title Compute an MD5 Hash of Title and First Author for Deduplication
#'
#' @description
#' Generates a stable MD5 fingerprint by concatenating the normalized title
#' and normalized first-author name. Used to detect duplicate abstract
#' submissions across congress years or data sources.
#'
#' @param title Character scalar. Raw abstract title (normalized internally).
#' @param first_author Character scalar. Raw first-author name (normalized
#'   internally via \code{\link{normalize_author}()}).
#'
#' @return Character scalar. A 32-character lowercase hexadecimal MD5 digest.
#'
#' @details
#' The hash is computed on the pipe-delimited string
#' \code{normalize_title(title)|normalize_author(first_author)}. Normalizing
#' both fields before hashing ensures that minor formatting differences
#' (capitalization, punctuation, diacritics) between data sources do not
#' produce spurious non-duplicates.
#'
#' @examples
#' \dontrun{
#' abstract_hash(
#'   "Robot-Assisted Sacrocolpopexy: 5-Year Follow-Up",
#'   "Smith JA"
#' )
#' # "d41d8cd98f00b204e9800998ecf8427e"  (example only)
#' }
#'
#' @seealso \code{\link{normalize_title}}, \code{\link{normalize_author}}
#' @export
abstract_hash <- function(title, first_author) {
  digest::digest(paste0(normalize_title(title), "|",
                        normalize_author(first_author)),
                 algo = "md5")
}
