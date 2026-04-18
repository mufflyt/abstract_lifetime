# utils_states.R — US state name/abbreviation parsing from affiliation text.

library(stringr)

# Full state name → 2-letter abbreviation
STATE_NAMES <- c(
  "alabama" = "AL", "alaska" = "AK", "arizona" = "AZ", "arkansas" = "AR",
  "california" = "CA", "colorado" = "CO", "connecticut" = "CT", "delaware" = "DE",
  "district of columbia" = "DC", "florida" = "FL", "georgia" = "GA", "hawaii" = "HI",
  "idaho" = "ID", "illinois" = "IL", "indiana" = "IN", "iowa" = "IA",
  "kansas" = "KS", "kentucky" = "KY", "louisiana" = "LA", "maine" = "ME",
  "maryland" = "MD", "massachusetts" = "MA", "michigan" = "MI", "minnesota" = "MN",
  "mississippi" = "MS", "missouri" = "MO", "montana" = "MT", "nebraska" = "NE",
  "nevada" = "NV", "new hampshire" = "NH", "new jersey" = "NJ", "new mexico" = "NM",
  "new york" = "NY", "north carolina" = "NC", "north dakota" = "ND", "ohio" = "OH",
  "oklahoma" = "OK", "oregon" = "OR", "pennsylvania" = "PA", "puerto rico" = "PR",
  "rhode island" = "RI", "south carolina" = "SC", "south dakota" = "SD",
  "tennessee" = "TN", "texas" = "TX", "utah" = "UT", "vermont" = "VT",
  "virginia" = "VA", "washington" = "WA", "west virginia" = "WV",
  "wisconsin" = "WI", "wyoming" = "WY"
)

# Common US city → state mapping for disambiguation (top academic medical centers)
CITY_STATE <- c(
  "new york" = "NY", "los angeles" = "CA", "chicago" = "IL", "houston" = "TX",
  "phoenix" = "AZ", "philadelphia" = "PA", "san antonio" = "TX", "san diego" = "CA",
  "dallas" = "TX", "san jose" = "CA", "austin" = "TX", "jacksonville" = "FL",
  "san francisco" = "CA", "columbus" = "OH", "indianapolis" = "IN",
  "charlotte" = "NC", "seattle" = "WA", "denver" = "CO", "boston" = "MA",
  "nashville" = "TN", "detroit" = "MI", "portland" = "OR", "memphis" = "TN",
  "louisville" = "KY", "baltimore" = "MD", "milwaukee" = "WI", "atlanta" = "GA",
  "miami" = "FL", "pittsburgh" = "PA", "cleveland" = "OH", "st. louis" = "MO",
  "saint louis" = "MO", "st louis" = "MO", "minneapolis" = "MN",
  "cincinnati" = "OH", "kansas city" = "MO", "tampa" = "FL", "aurora" = "CO",
  "new haven" = "CT", "ann arbor" = "MI", "rochester" = "NY", "birmingham" = "AL",
  "madison" = "WI", "chapel hill" = "NC", "durham" = "NC", "palo alto" = "CA",
  "stanford" = "CA", "gainesville" = "FL", "charlottesville" = "VA",
  "salt lake city" = "UT", "richmond" = "VA", "west bloomfield" = "MI",
  "springfield" = "IL", "hanover" = "NH", "burlington" = "VT", "providence" = "RI",
  "new orleans" = "LA", "oklahoma city" = "OK", "omaha" = "NE",
  "little rock" = "AR", "jackson" = "MS", "honolulu" = "HI",
  "albuquerque" = "NM", "tucson" = "AZ", "boise" = "ID", "kalispell" = "MT",
  "anchorage" = "AK", "birmingham" = "AL", "hershey" = "PA",
  "sacramento" = "CA", "irvine" = "CA", "la jolla" = "CA",
  "bethesda" = "MD", "silver spring" = "MD", "rockville" = "MD",
  "bronx" = "NY", "brooklyn" = "NY", "manhasset" = "NY", "valhalla" = "NY",
  "camden" = "NJ", "newark" = "NJ", "hackensack" = "NJ",
  "danville" = "PA", "maywood" = "IL", "iowa city" = "IA",
  "winston-salem" = "NC", "galveston" = "TX", "augusta" = "GA",
  "morgantown" = "WV", "lexington" = "KY", "columbia" = "MO",
  "scottsdale" = "AZ", "toledo" = "OH", "dayton" = "OH"
)

# Well-known institution → state (catches abbreviated names)
INSTITUTION_STATE <- c(
  "mayo clinic" = "MN", "cleveland clinic" = "OH",
  "johns hopkins" = "MD", "cedars-sinai" = "CA", "cedars sinai" = "CA",
  "mount sinai" = "NY", "mt sinai" = "NY", "nyu" = "NY",
  "columbia university" = "NY", "cornell" = "NY", "weill cornell" = "NY",
  "yale" = "CT", "harvard" = "MA", "stanford" = "CA", "duke" = "NC",
  "emory" = "GA", "vanderbilt" = "TN", "ucsf" = "CA", "ucla" = "CA",
  "usc" = "CA", "uw madison" = "WI", "upenn" = "PA",
  "magee-womens" = "PA", "magee womens" = "PA", "magee-women" = "PA",
  "brigham and women" = "MA", "mass general" = "MA", "mgh" = "MA",
  "beth israel" = "MA", "tufts" = "MA", "boston university" = "MA",
  "northwestern" = "IL", "rush" = "IL", "loyola" = "IL",
  "advocate aurora" = "IL", "northshore university" = "IL",
  "beaumont" = "MI", "henry ford" = "MI", "michigan medicine" = "MI",
  "nih" = "MD", "national institutes of health" = "MD",
  "walter reed" = "MD", "bethesda" = "MD",
  "winthrop" = "NY", "lenox hill" = "NY", "montefiore" = "NY",
  "northwell" = "NY", "long island jewish" = "NY",
  "sloan kettering" = "NY", "memorial sloan" = "NY",
  "nyu langone" = "NY", "new york presbyterian" = "NY",
  "albert einstein" = "NY", "stony brook" = "NY",
  "thomas jefferson" = "PA", "fox chase" = "PA",
  "geisinger" = "PA", "lehigh valley" = "PA",
  "scripps" = "CA", "kaiser" = "CA", "loma linda" = "CA",
  "baylor" = "TX", "ut southwestern" = "TX", "md anderson" = "TX",
  "methodist houston" = "TX", "parkland" = "TX",
  "ochsner" = "LA", "tulane" = "LA",
  "dartmouth" = "NH", "lahey" = "MA",
  "st elizabeth" = "MA", "st. elizabeth" = "MA",
  "penn medicine" = "PA", "university of pennsylvania" = "PA",
  "drexel" = "PA", "temple university" = "PA",
  "george washington" = "DC", "georgetown" = "DC", "medstar" = "DC",
  "wake forest" = "NC", "carolinas medical" = "NC",
  "university of florida" = "FL", "moffitt" = "FL",
  "university of miami" = "FL", "jackson memorial" = "FL",
  "ohio state" = "OH", "case western" = "OH",
  "university of cincinnati" = "OH",
  "indiana university" = "IN", "iu health" = "IN",
  "university of iowa" = "IA",
  "university of kansas" = "KS",
  "university of nebraska" = "NE", "creighton" = "NE",
  "university of colorado" = "CO",
  "intermountain" = "UT", "university of utah" = "UT",
  "university of washington" = "WA", "swedish" = "WA",
  "ohsu" = "OR", "oregon health" = "OR",
  "university of arizona" = "AZ", "banner" = "AZ",
  "university of new mexico" = "NM",
  "university of vermont" = "VT",
  "university of connecticut" = "CT", "hartford" = "CT",
  "university of alabama" = "AL", "uab" = "AL",
  "university of mississippi" = "MS",
  "university of arkansas" = "AR",
  "university of oklahoma" = "OK",
  "university of tennessee" = "TN",
  "university of south carolina" = "SC", "musc" = "SC",
  "university of virginia" = "VA", "virginia commonwealth" = "VA", "inova" = "VA"
)

#' @title Parse a US State Abbreviation from a PubMed Affiliation String
#'
#' @description
#' Extracts a 2-letter US state or territory abbreviation from a raw PubMed
#' affiliation string using a four-strategy waterfall: institution name lookup,
#' spelled-out state name, 2-letter code with optional ZIP, and city name
#' lookup.
#'
#' @param aff Character scalar. Raw affiliation text, typically from a PubMed
#'   author record. Returns \code{NA_character_} for \code{NA} or empty input.
#'
#' @return Character scalar. A 2-letter uppercase state abbreviation (e.g.,
#'   \code{"TX"}, \code{"CA"}, \code{"DC"}) or \code{NA_character_} if no
#'   US state can be identified.
#'
#' @details
#' Strategy priority (highest to lowest):
#' \enumerate{
#'   \item \strong{Institution lookup} — matches against \code{INSTITUTION_STATE},
#'     a curated table of well-known US academic medical centers and hospitals
#'     (e.g., "Johns Hopkins" → "MD"). Names are tested longest-first to avoid
#'     partial matches. This prevents "George Washington University" → "WA"
#'     (should be "DC") and "Mayo Clinic, Rochester" → "NY" (should be "MN").
#'   \item \strong{Spelled-out state name} — tests each state name from
#'     \code{STATE_NAMES} as a word-bounded regex, longest-first.
#'   \item \strong{2-letter code + optional ZIP} — scans comma-split tokens
#'     for a trailing \code{[A-Z]\{2\}} optionally followed by a ZIP code.
#'     Also handles period-separated abbreviations (\code{N.Y.}).
#'   \item \strong{City lookup} — matches against \code{CITY_STATE}, a
#'     curated table of top US academic medical center cities,
#'     longest-first to avoid short-name ambiguity.
#' }
#' Email addresses and "Electronic address:" suffixes are stripped before
#' matching.
#'
#' @examples
#' \dontrun{
#' parse_us_state("Department of OB/GYN, Mayo Clinic, Rochester, MN 55905")
#' # "MN"
#'
#' parse_us_state("Johns Hopkins Hospital, Baltimore, MD")
#' # "MD"
#'
#' parse_us_state("Hôpital Lariboisière, Paris, France")
#' # NA_character_
#' }
#'
#' @seealso \code{\link{parse_country}}, \code{\link{is_us_affiliation}},
#'   \code{\link{acog_district_for_state}}
#' @export
parse_us_state <- function(aff) {
  if (is.na(aff) || nchar(aff) == 0) return(NA_character_)
  # Strip email addresses, electronic address tags, and trailing periods
  aff_clean <- str_replace_all(aff, "\\S+@\\S+", "")
  aff_clean <- str_replace(aff_clean, "Electronic address:.*$", "")
  aff_clean <- str_squish(aff_clean)
  lc <- tolower(aff_clean)

  # Strategy 1: Institution name lookup (highest priority — "George Washington
  # University" should resolve to DC, not state "Washington" = WA; "Mayo Clinic,
  # Rochester" should resolve to MN, not city Rochester = NY)
  inst_order <- names(INSTITUTION_STATE)[order(-nchar(names(INSTITUTION_STATE)))]
  for (inst in inst_order) {
    if (str_detect(lc, fixed(inst))) return(unname(INSTITUTION_STATE[inst]))
  }

  # Strategy 2: Spelled-out state names (longest-first to avoid partial matches)
  state_order <- names(STATE_NAMES)[order(-nchar(names(STATE_NAMES)))]
  for (nm in state_order) {
    if (str_detect(lc, paste0("\\b", nm, "\\b"))) return(unname(STATE_NAMES[nm]))
  }

  # Strategy 3: 2-letter uppercase code (possibly with ZIP or trailing period)
  parts <- str_trim(str_split(aff_clean, ",")[[1]])
  for (p in parts) {
    m <- str_match(p, "\\b([A-Z]{2})(?:\\s+\\d{5}(?:-\\d{4})?)?[.;]?\\s*$")
    if (!is.na(m[1, 2]) && m[1, 2] %in% STATE_NAMES) return(m[1, 2])
    m2 <- str_match(p, "\\b([A-Z])\\.([A-Z])\\.?\\s*$")
    if (!is.na(m2[1, 2])) {
      code <- paste0(m2[1, 2], m2[1, 3])
      if (code %in% STATE_NAMES) return(code)
    }
  }

  # Strategy 4: City name lookup
  city_order <- names(CITY_STATE)[order(-nchar(names(CITY_STATE)))]
  for (city in city_order) {
    if (str_detect(lc, paste0("\\b", city, "\\b"))) return(unname(CITY_STATE[city]))
  }

  NA_character_
}

# Canonical country name lookup — maps common variants and abbreviations.
.COUNTRY_CANONICAL <- c(
  "japan" = "Japan", "nippon" = "Japan",
  "china" = "China", "p.r. china" = "China", "people's republic of china" = "China",
  "hong kong" = "Hong Kong",
  "south korea" = "South Korea", "korea" = "South Korea", "republic of korea" = "South Korea",
  "taiwan" = "Taiwan", "republic of china" = "Taiwan",
  "india" = "India",
  "singapore" = "Singapore",
  "thailand" = "Thailand",
  "malaysia" = "Malaysia",
  "indonesia" = "Indonesia",
  "philippines" = "Philippines",
  "vietnam" = "Vietnam",
  "pakistan" = "Pakistan",
  "bangladesh" = "Bangladesh",
  "sri lanka" = "Sri Lanka",
  "nepal" = "Nepal",
  "iran" = "Iran",
  "israel" = "Israel",
  "turkey" = "Turkey",
  "saudi arabia" = "Saudi Arabia",
  "united arab emirates" = "UAE", "uae" = "UAE",
  "qatar" = "Qatar",
  "egypt" = "Egypt",
  "jordan" = "Jordan",
  "lebanon" = "Lebanon",
  "germany" = "Germany",
  "france" = "France",
  "italy" = "Italy",
  "spain" = "Spain",
  "united kingdom" = "United Kingdom", "uk" = "United Kingdom",
  "england" = "United Kingdom", "scotland" = "United Kingdom",
  "wales" = "United Kingdom", "northern ireland" = "United Kingdom",
  "netherlands" = "Netherlands", "the netherlands" = "Netherlands",
  "belgium" = "Belgium",
  "sweden" = "Sweden",
  "norway" = "Norway",
  "denmark" = "Denmark",
  "finland" = "Finland",
  "switzerland" = "Switzerland",
  "austria" = "Austria",
  "portugal" = "Portugal",
  "ireland" = "Ireland",
  "poland" = "Poland",
  "czech republic" = "Czech Republic", "czechia" = "Czech Republic",
  "hungary" = "Hungary",
  "romania" = "Romania",
  "greece" = "Greece",
  "russia" = "Russia",
  "canada" = "Canada",
  "australia" = "Australia",
  "new zealand" = "New Zealand",
  "brazil" = "Brazil",
  "mexico" = "Mexico",
  "colombia" = "Colombia",
  "argentina" = "Argentina",
  "chile" = "Chile",
  "south africa" = "South Africa",
  "nigeria" = "Nigeria",
  "kenya" = "Kenya"
)

#' @title Extract the Country from an Affiliation String
#'
#' @description
#' Identifies the country of origin for a PubMed affiliation using two
#' strategies: checking the last comma-delimited token (where country typically
#' appears) and scanning the full string for known country names.
#'
#' @param aff Character scalar. Raw affiliation text. Returns
#'   \code{NA_character_} for \code{NA} or strings shorter than 3 characters.
#'
#' @return Character scalar. A canonical country name from
#'   \code{.COUNTRY_CANONICAL} (e.g., \code{"USA"}, \code{"Japan"},
#'   \code{"United Kingdom"}) or \code{NA_character_} if no country is
#'   identified.
#'
#' @details
#' US affiliations are detected first (via explicit keywords and
#' \code{\link{parse_us_state}()}) and return \code{"USA"} without further
#' scanning. For non-US affiliations, the last comma-delimited token is
#' checked against \code{.COUNTRY_CANONICAL}; if not matched, the full string
#' is scanned with word-bounded patterns in longest-match-first order to
#' reduce false positives (e.g., "Korea" matched before "South Korea" would
#' be wrong).
#'
#' @examples
#' \dontrun{
#' parse_country("Department of Surgery, Keio University, Tokyo, Japan")
#' # "Japan"
#'
#' parse_country("University of Texas MD Anderson Cancer Center, Houston, TX")
#' # "USA"
#'
#' parse_country("Charité Universitätsmedizin Berlin, Germany")
#' # "Germany"
#' }
#'
#' @seealso \code{\link{parse_us_state}}, \code{\link{is_us_affiliation}},
#'   \code{.COUNTRY_CANONICAL}
#' @export
parse_country <- function(aff) {
  if (is.na(aff) || nchar(aff) < 3) return(NA_character_)

  # US signals → return "USA" without further scanning
  lc <- tolower(aff)
  if (str_detect(lc, "\\b(usa|united states|u\\.s\\.a\\.)\\b") ||
      !is.na(parse_us_state(aff))) return("USA")

  # Strategy 1: last comma-delimited token is typically the country
  parts <- str_trim(str_split(aff, ",")[[1]])
  last_token <- tolower(parts[length(parts)])
  last_token <- str_remove(last_token, "\\.\\s*$")
  last_token <- str_squish(last_token)
  if (last_token %in% names(.COUNTRY_CANONICAL)) {
    return(unname(.COUNTRY_CANONICAL[last_token]))
  }

  # Strategy 2: scan full string for any known country name (longest match first)
  country_order <- names(.COUNTRY_CANONICAL)[order(-nchar(names(.COUNTRY_CANONICAL)))]
  for (cn in country_order) {
    if (str_detect(lc, paste0("\\b", cn, "\\b"))) {
      return(unname(.COUNTRY_CANONICAL[cn]))
    }
  }

  NA_character_
}

#' @title Detect Whether an Affiliation Is US-Based
#'
#' @description
#' Returns a logical indicator of whether a PubMed affiliation string belongs
#' to a US-based institution. More robust than \code{\link{parse_us_state}()}
#' for ambiguous affiliations because it checks for explicit non-US country
#' keywords before attempting state detection.
#'
#' @param aff Character scalar. Raw affiliation text. Returns \code{NA} for
#'   \code{NA} or empty strings.
#'
#' @return Logical scalar. \code{TRUE} if the affiliation contains US signals
#'   ("USA", "United States", a parseable US state, or a trailing "US" token).
#'   \code{FALSE} if a non-US country keyword is found. \code{NA} if neither
#'   US nor non-US signals are present (ambiguous, typically for bare department
#'   names with no geography).
#'
#' @details
#' Non-US country list covers ~40 countries commonly appearing in AAGL
#' conference abstract submissions. The check is applied in word-boundary
#' context to avoid false positives (e.g., "turkey" as a food item is unlikely
#' in a medical affiliation, but the function relies on context). When both US
#' and non-US signals appear (unusual), US signals take precedence because
#' explicit "USA" or a ZIP code is more specific than a matching country name.
#'
#' @examples
#' \dontrun{
#' is_us_affiliation("Mayo Clinic, Rochester, MN 55905, USA")   # TRUE
#' is_us_affiliation("University of Tokyo, Tokyo, Japan")       # FALSE
#' is_us_affiliation("Department of Surgery")                   # NA
#' }
#'
#' @seealso \code{\link{parse_us_state}}, \code{\link{parse_country}}
#' @export
is_us_affiliation <- function(aff) {
  if (is.na(aff) || nchar(aff) == 0) return(NA)
  lc <- tolower(aff)

  # Explicit non-US countries
  non_us <- c("china", "japan", "korea", "india", "germany", "france", "italy",
              "spain", "brazil", "canada", "australia", "united kingdom", "uk",
              "taiwan", "turkey", "iran", "israel", "egypt", "saudi arabia",
              "netherlands", "belgium", "sweden", "norway", "denmark", "finland",
              "switzerland", "austria", "south africa", "mexico", "colombia",
              "argentina", "chile", "portugal", "ireland", "scotland", "wales",
              "singapore", "thailand", "malaysia", "indonesia", "vietnam",
              "pakistan", "bangladesh", "sri lanka", "nepal", "philippines",
              "czech republic", "poland", "hungary", "romania", "greece",
              "lebanon", "jordan", "qatar", "uae", "united arab emirates",
              "new zealand", "nigeria")
  for (c in non_us) {
    if (str_detect(lc, paste0("\\b", c, "\\b"))) return(FALSE)
  }

  # US signals
  if (str_detect(lc, "\\busa\\b|\\bu\\.s\\.a\\b|united states|\\bus\\b$")) return(TRUE)
  if (!is.na(parse_us_state(aff))) return(TRUE)

  NA
}
