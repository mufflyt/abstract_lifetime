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

#' Parse a US state from a PubMed affiliation string.
#' Tries: (1) spelled-out state name, (2) 2-letter code + ZIP, (3) city lookup,
#' (4) institution lookup. Returns 2-letter state code or NA.
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

#' Extract the country from an affiliation string.
#' Strategy: (1) check last comma-delimited token (most affiliations end "City, Country"),
#' (2) scan full string for known country names.
#' Returns canonical country name or NA_character_.
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
