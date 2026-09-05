# utils_external_paths.R — resolve paths to data that lives outside the repo.
#
# These files (the ABOG NPI export, the NPPES/Open Payments DuckDB mirror) are
# large, licensed, or both, so they sit on the operator's machine rather than in
# version control. The paths to them are therefore machine-specific, and a
# machine-specific path embedded in a script is two problems at once: it makes
# the script non-portable, and it publishes the operator's username, home
# directory layout and external volume names to anyone who reads the repository.
#
# Resolution order, highest first:
#   1. the named environment variable (set it in .Renviron, which is gitignored)
#   2. config.yml: external_data$<key>
#   3. "" — treated as absent
#
# Every caller must degrade gracefully when the result is "" or does not exist.
# None of these inputs is required to reproduce the headline results; they
# enrich author demographics only. See docs/REPRODUCIBILITY.md.

#' Resolve an external data path without hard-coding a personal location.
#'
#' @param key Name under `external_data:` in config.yml.
#' @param env_var Environment variable that overrides it.
#' @param cfg Optional pre-read config list; read from config.yml when NULL.
#' @return An absolute path with `~` expanded, or "" when unset.
external_path <- function(key, env_var, cfg = NULL) {
  if (is.null(cfg)) {
    cfg <- tryCatch(config::get(file = here::here("config.yml")),
                    error = function(e) NULL)
  }
  from_cfg <- tryCatch(cfg$external_data[[key]], error = function(e) NULL)
  if (is.null(from_cfg) || length(from_cfg) == 0 || is.na(from_cfg[1])) {
    from_cfg <- ""
  }
  p <- Sys.getenv(env_var, unset = from_cfg)
  if (!nzchar(p)) return("")
  path.expand(p)
}

#' Is an external input actually available on this machine?
external_available <- function(path) nzchar(path) && file.exists(path)
