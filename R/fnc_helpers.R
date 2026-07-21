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





#' Disconnect Database Session
#'
#' Safely disconnects the current database session by clearing the application's
#' state, resetting the UI, and notifying the user. This function centralizes
#' the disconnect logic for consistency and maintainability.
#'
#' @param db_state A `reactiveValues` object holding the application's global state,
#'   including `db_state$path`.
#' @param session The Shiny session object, used for UI manipulations like
#'   switching tabs and showing alerts.
#'
#' @return Nothing; this function is called for its side effects.
#' @export
#' @importFrom bslib nav_select
#' @importFrom shinyWidgets show_alert
disconnect_db_session <- function(db_state, session) {
  # 1. Clear the database path from the reactive state
  db_state$path <- NULL

  # 2. Clear the saved configuration file to prevent auto-reconnection.
  save_db_config("")

  # 4. Provide feedback to the user
  shinyWidgets::show_alert(
    title = "Disconnected",
    text = "You have been disconnected from the database session.",
    type = "info",
    session = session
  )
}