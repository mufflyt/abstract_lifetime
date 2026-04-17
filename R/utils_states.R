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
  "beth israel" = "MA", "northwestern" = "IL", "rush" = "IL",
  "beaumont" = "MI", "henry ford" = "MI", "michigan medicine" = "MI",
  "nih" = "MD", "walter reed" = "MD", "bethesda" = "MD"
)

#' Parse a US state from a PubMed affiliation string.
#' Tries: (1) spelled-out state name, (2) 2-letter code + ZIP, (3) city lookup,
#' (4) institution lookup. Returns 2-letter state code or NA.
parse_us_state <- function(aff) {
  if (is.na(aff) || nchar(aff) == 0) return(NA_character_)
  lc <- tolower(aff)

  # Strategy 1: Spelled-out state names (longest-first to avoid "New" matching "Nevada")
  state_order <- names(STATE_NAMES)[order(-nchar(names(STATE_NAMES)))]
  for (nm in state_order) {
    if (str_detect(lc, paste0("\\b", nm, "\\b"))) return(unname(STATE_NAMES[nm]))
  }

  # Strategy 2: 2-letter uppercase code (possibly with ZIP)
  # Look in comma-separated parts
  parts <- str_trim(str_split(aff, ",")[[1]])
  for (p in parts) {
    m <- str_match(p, "\\b([A-Z]{2})(?:\\s+\\d{5}(?:-\\d{4})?)?$")
    if (!is.na(m[1, 2]) && m[1, 2] %in% STATE_NAMES) return(m[1, 2])
  }

  # Strategy 3: Institution name lookup (before city — "Mayo Clinic, Rochester"
  # should match institution MN, not city Rochester NY)
  inst_order <- names(INSTITUTION_STATE)[order(-nchar(names(INSTITUTION_STATE)))]
  for (inst in inst_order) {
    if (str_detect(lc, fixed(inst))) return(unname(INSTITUTION_STATE[inst]))
  }

  # Strategy 4: City name lookup
  city_order <- names(CITY_STATE)[order(-nchar(names(CITY_STATE)))]
  for (city in city_order) {
    if (str_detect(lc, paste0("\\b", city, "\\b"))) return(unname(CITY_STATE[city]))
  }

  NA_character_
}

#' Detect whether an affiliation is US-based (broader than state parsing).
#' Returns TRUE if any US signal found, FALSE if non-US country detected, NA if ambiguous.
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
