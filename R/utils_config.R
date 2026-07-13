#' @title Get App Config File Path
#' @description Returns the absolute path to the local configuration file.
#' @noRd
get_config_path <- function() {
  file.path(path.expand("~"), ".gembank_config.rds")
}

#' @title Load Saved Database Path
#' @description Checks if a database path has been saved and is valid.
#' @return A character path to the SQLite database, or NULL if not configured.
#' @export
load_db_config <- function() {
  cfg_path <- get_config_path()
  if (file.exists(cfg_path)) {
    tryCatch(
      {
        cfg <- readRDS(cfg_path)
        if (!is.null(cfg$db_path) && file.exists(cfg$db_path)) {
          return(cfg$db_path)
        }
      },
      error = function(e) NULL
    )
  }
  return(NULL)
}

#' @title Save Database Path
#' @description Persists the database path to the local configuration file.
#' @param db_path Path to the SQLite database file.
#' @return Logical indicating success.
#' @export
save_db_config <- function(db_path) {
  cfg_path <- get_config_path()
  cfg <- list(db_path = unname(normalizePath(db_path, mustWork = FALSE)))
  tryCatch(
    {
      saveRDS(cfg, cfg_path)
      return(TRUE)
    },
    error = function(e) FALSE
  )
}