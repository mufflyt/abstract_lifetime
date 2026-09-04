# 06c_session_snapshot.R — Record the environment a run was produced in.
#
# Why this exists
# ---------------
# docs/REPRODUCIBILITY.md names the absence of dependency pinning as this
# project's largest reproducibility gap: there is no renv.lock, no DESCRIPTION
# and no version constraint anywhere, so "R >= 4.4 plus these packages" is the
# whole specification. That cannot be tightened retrospectively, but it can at
# least be RECORDED, so a future run that disagrees with the committed outputs
# has something to diff against.
#
# mysterycall::mysterycall_session_snapshot() writes R version, platform, the
# seed and every loaded package version. It is base-R only.
#
# This is not a substitute for renv.lock. It records what was used; it does not
# let anyone reinstall it. Adding renv remains the recommendation.
#
# Output: output/session_snapshot.txt

suppressPackageStartupMessages({
  library(here); library(cli); library(config)
})

cli_h2("Session snapshot")

snapshot_path <- here("output", "session_snapshot.txt")
cfg <- config::get(file = here("config.yml"))

if (requireNamespace("mysterycall", quietly = TRUE)) {
  mysterycall::mysterycall_session_snapshot(
    file  = snapshot_path,
    seeds = cfg$pipeline$seed,
    notes = paste0(
      "abstract_lifetime pipeline. Cohort and outputs under output/. ",
      "mysterycall pinned at 42d66d92 (see docs/REPRODUCIBILITY.md)."
    ),
    quiet = TRUE
  )
  cli_alert_success("Wrote {snapshot_path}")
} else {
  # A plain fallback so a machine without the package still leaves a record.
  cli_alert_warning("mysterycall not installed - writing a reduced snapshot")
  si <- utils::sessionInfo()
  writeLines(c(
    "=== REPRODUCIBILITY SNAPSHOT (reduced) ===",
    paste("Date/Time:", format(Sys.time())),
    paste("R Version:", si$R.version$version.string),
    paste("Platform: ", si$platform),
    paste("Seed:     ", cfg$pipeline$seed),
    "",
    "=== LOADED PACKAGES ===",
    vapply(c(si$otherPkgs, si$loadedOnly),
           function(p) sprintf("%-30s %s", p$Package, p$Version), character(1))
  ), snapshot_path)
  cli_alert_success("Wrote {snapshot_path} (reduced)")
}
