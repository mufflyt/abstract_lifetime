# elsevier_api_probe.R — what can this Elsevier API key actually reach?
#
# A23 records that the cohort cannot be completed because ScienceDirect returns
# HTTP 403. That 403 came from scraping the HTML listing with rvest, so the
# obvious next move was Elsevier's official API. This script is the result of
# actually trying it, and it exists so the attempt can be repeated from a
# different network without guessing.
#
# The finding on a personal network, 2026-09-06, with a self-service key:
#
#   serial/title                200  journal metadata only, no article list
#   search/scopus               200  works, but SUPPLEMENT ABSTRACTS ARE NOT
#                                    INDEXED -- both known 2022 supplement DOIs
#                                    return 0 results, and a volume query
#                                    returns only regular articles on numbered
#                                    pages. Scopus cannot answer this question.
#   search/sciencedirect        401  AUTHORIZATION_ERROR
#   metadata/article            401  AUTHORIZATION_ERROR
#   article retrieval full text 403  AUTHENTICATION_ERROR, "requestor
#                                    configuration settings insufficient"
#
# So the key authenticates and is Scopus-entitled, but has no ScienceDirect
# entitlement. Elsevier entitlement is commonly IP-bound: the same key can
# succeed inside an institutional network and fail outside it. Before
# concluding the route is closed, re-run this on the institutional network or
# VPN, and if it still fails, ask the library for a ScienceDirect API
# entitlement or an institutional token (sent as the X-ELS-Insttoken header).
#
# Set ELSEVIER_API_KEY in .Renviron (gitignored). Never commit the key.
# Usage: Rscript scripts/elsevier_api_probe.R

suppressPackageStartupMessages({library(httr); library(jsonlite)})

key <- Sys.getenv("ELSEVIER_API_KEY", "")
if (!nzchar(key)) {
  stop("ELSEVIER_API_KEY is not set. Add it to .Renviron (which is gitignored).",
       call. = FALSE)
}

# Ground truth: 2022 is one of only two congresses where the oral/video
# boundary is known, because the capture ran past the end of the oral block.
ORAL_DOI  <- "10.1016/j.jmig.2022.09.016"   # AAGL2022_001, Page S1,  Oral
VIDEO_DOI <- "10.1016/j.jmig.2022.09.120"   # AAGL2022_091, Page S37, Video

hit <- function(url, query = NULL, insttoken = Sys.getenv("ELSEVIER_INSTTOKEN", "")) {
  hdr <- c(`X-ELS-APIKey` = key, Accept = "application/json")
  if (nzchar(insttoken)) hdr <- c(hdr, `X-ELS-Insttoken` = insttoken)
  tryCatch(GET(url, query = query, do.call(add_headers, as.list(hdr)), timeout(40)),
           error = function(e) NULL)
}

report <- function(r, label) {
  if (is.null(r)) { cat(sprintf("  %-34s NETWORK ERROR\n", label)); return(invisible(NA)) }
  code <- status_code(r)
  cat(sprintf("  %-34s HTTP %s%s\n", label, code,
              if (code == 200) "" else paste0("  ", .err_of(r))))
  invisible(code)
}

.err_of <- function(r) {
  txt <- content(r, "text", encoding = "UTF-8")
  m <- regmatches(txt, regexpr('"statusCode"\\s*:\\s*"[^"]+"', txt))
  if (length(m)) sub('.*:\\s*"', "", sub('"$', "", m)) else substr(txt, 1, 60)
}

cat("Endpoints reachable with this key:\n")
report(hit("https://api.elsevier.com/content/serial/title",
           list(issn = "1553-4650")), "serial/title")
sd_search <- report(hit("https://api.elsevier.com/content/search/sciencedirect",
                        list(query = "ISSN(1553-4650)", count = 2)), "search/sciencedirect")
report(hit("https://api.elsevier.com/content/metadata/article",
           list(query = "ISSN(1553-4650)", count = 2)), "metadata/article")
report(hit("https://api.elsevier.com/content/search/scopus",
           list(query = "ISSN(1553-4650)", count = 2)), "search/scopus")
sd_full <- report(hit(paste0("https://api.elsevier.com/content/article/doi/", ORAL_DOI)),
                  "article retrieval (full text)")

# The question this was built to answer.
cat("\nCan any reachable endpoint separate an Oral from a Video?\n")
scopus_indexed <- function(doi) {
  r <- hit("https://api.elsevier.com/content/search/scopus",
           list(query = sprintf('DOI("%s")', doi), count = 1))
  if (is.null(r) || status_code(r) != 200) return(NA_integer_)
  as.integer(fromJSON(content(r, "text", encoding = "UTF-8"),
                      simplifyVector = FALSE)$`search-results`$`opensearch:totalResults`)
}
n_oral <- scopus_indexed(ORAL_DOI); n_video <- scopus_indexed(VIDEO_DOI)
cat(sprintf("  Scopus records for the known oral DOI:  %s\n", n_oral))
cat(sprintf("  Scopus records for the known video DOI: %s\n", n_video))
if (identical(n_oral, 0L) && identical(n_video, 0L))
  cat("  -> Scopus does not index the congress supplement. It cannot help here.\n")

if (identical(sd_full, 200L) || identical(sd_search, 200L)) {
  cat("\n  ScienceDirect IS reachable from this network. The cohort-completion\n",
      "  route in appendix A23 is open: retrieve the two ground-truth DOIs, diff\n",
      "  the records for a field that separates Oral from Video, and if one\n",
      "  exists apply it to the ten truncated congresses.\n", sep = "")
} else {
  cat("\n  ScienceDirect is NOT reachable with this key from this network.\n",
      "  Entitlement is commonly IP-bound. Re-run on the institutional network\n",
      "  or VPN before concluding the route is closed; failing that, request a\n",
      "  ScienceDirect entitlement or an institutional token from the library\n",
      "  and set ELSEVIER_INSTTOKEN alongside the key.\n", sep = "")
}
