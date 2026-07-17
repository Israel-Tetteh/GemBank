#' Setup Database Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_column_wrap
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets show_alert
#' @noRd
mod_setup_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    # Modern full-viewport container centered using Bootstrap 5 Flex utilities
    style = "background: linear-gradient(135deg, #F8FAFC 0%, #E2E8F0 100%); min-height: 90vh; display: flex; align-items: center; justify-content: center;",

    div(
      style = "width: 100%; max-width: 950px; padding: 20px;",
      bslib::card(
        class = "shadow-lg border-0",
        style = "border-radius: 16px; overflow: hidden; background: rgba(255, 255, 255, 0.85); backdrop-filter: blur(12px); -webkit-backdrop-filter: blur(12px);",

        # Sleek branding header block with a gradient background
        bslib::card_header(
          class = "text-white text-center py-4 border-0",
          style = "background: linear-gradient(90deg, #042F2E 0%, #0F766E 100%);",
          h4(
            class = "mb-1 fw-bold",
            style = "font-family: 'Outfit', sans-serif;",
            "Connect an existing breeding repository or initialize a fresh seed database session."
          ),
        ),

        bslib::card_body(
          class = "p-5",
          bslib::layout_column_wrap(
            width = 1 / 2, # Clean 50/50 responsive split using CSS grid
            fixed_width = FALSE,
            gap = "30px",

            # Left Segment: Connect Existing Storehouse
            bslib::card(
              class = "border-0 shadow-sm h-100",
              style = "border-radius: 12px; background: #FFFFFF; transition: transform 0.2s;",
              bslib::card_body(
                class = "text-center d-flex flex-column align-items-center justify-content-between p-4",
                div(
                  bsicons::bs_icon(
                    "database-fill-check",
                    size = "3.5rem",
                    class = "mb-3",
                    style = "color: #0284C7;"
                  ),
                  h4(
                    class = "fw-bold mb-2",
                    style = "color: #1E293B;",
                    "Link Existing Storage"
                  ),
                  p(
                    class = "small mb-4",
                    style = "color: #64748B;",
                    "Mount an active historical breeding record registry database file (.sqlite / .db)."
                  )
                ),
                div(
                  class = "w-100 mt-auto",
                  shinyFiles::shinyFilesButton(
                    ns("file_btn"),
                    label = "Locate SQLite File",
                    title = "Choose an existing breeding storage cluster",
                    multiple = FALSE,
                    class = "btn btn-outline-primary fw-bold w-100 mb-3 rounded-pill"
                  ),
                )
              )
            ),

            # Right Segment: Instantiate Fresh Node Repository
            bslib::card(
              class = "border-0 shadow-sm h-100",
              style = "border-radius: 12px; background: #FFFFFF; transition: transform 0.2s;",
              bslib::card_body(
                class = "text-center d-flex flex-column align-items-center justify-content-between p-4",
                div(
                  bsicons::bs_icon(
                    "database-fill-add",
                    size = "3.5rem",
                    class = "mb-3",
                    style = "color: #059669;"
                  ),
                  h4(
                    class = "fw-bold mb-2",
                    style = "color: #1E293B;",
                    "Create Clean Database"
                  ),
                  p(
                    class = "small mb-3",
                    style = "color: #64748B;",
                    "Establish a fresh working sector workspace root target folder configuration."
                  )
                ),
                div(
                  class = "w-100 mt-auto",
                  shinyFiles::shinyDirButton(
                    ns("dir_btn"),
                    label = "Target Directory Path",
                    title = "Select working path cluster container",
                    class = "btn btn-outline-secondary fw-bold w-100 mb-3 rounded-pill"
                  ),
                  actionButton(
                    ns("create_btn"),
                    label = "Initialize Schema Architecture",
                    class = "btn btn-success fw-bold w-100 mt-2 rounded-pill"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}



#' Setup Database Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from the main `app_server` that
#'   holds the application's global state, including the database path.
#' @noRd
mod_setup_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define robust, cross-platform volumes for shinyFiles navigation
    volumes <- {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        user_profile <- Sys.getenv("USERPROFILE")
        # Combine user-friendly locations with all available drive letters
        c(
          Home = user_profile,
          Desktop = file.path(user_profile, "Desktop"),
          Documents = file.path(user_profile, "Documents"),
          shinyFiles::getVolumes()()
        )
      } else {
        # For macOS and Linux, provide standard home and root directories
        c(
          Home = path.expand("~"),
          Root = "/"
        )
      }
    }

    # Initialize the shinyFiles handlers for file and directory selection.
    shinyFiles::shinyFileChoose(input, "file_btn", roots = volumes, filetypes = c("sqlite", "db"))
    shinyFiles::shinyDirChoose(input, "dir_btn", roots = volumes, allowDirCreate = FALSE)

    # Reactive expression to parse the selected file path.
    # This logic handles cases where no file is selected or the dialog is cancelled.
    selected_file_path <- reactive({
      if (is.integer(input$file_btn) || is.null(input$file_btn)) return(NULL)
      parsed <- shinyFiles::parseFilePaths(volumes, input$file_btn)$datapath
      if (length(parsed) == 0 || parsed == "") return(NULL)
      return(parsed)
    })

    # Reactive expression to parse the selected directory path.
    selected_dir_path <- reactive({
      if (is.integer(input$dir_btn) || is.null(input$dir_btn)) return(NULL)
      parsed <- shinyFiles::parseDirPath(volumes, input$dir_btn)
      if (length(parsed) == 0 || parsed == "") return(NULL)
      return(parsed)
    })

    # Observer for handling an existing database file selection.
    observeEvent(selected_file_path(), {
      path <- selected_file_path()
      req(path)

      if (file.exists(path)) {
        # Before connecting, perform a sanity check on the file.
        is_valid_db <- tryCatch({
          con <- DBI::dbConnect(RSQLite::SQLite(), path)
          # Quick query check to verify it contains your expected database structural tables
          tables <- DBI::dbListTables(con)
          DBI::dbDisconnect(con)
          "germplasm" %in% tables
        }, error = function(e) FALSE)

        if (is_valid_db) {
          shinyWidgets::show_alert("Success", "Secure cluster mounted successfully!", type = "success")
          db_state$path <- path  # Update global state, triggers UI changes in app_server
        } else {
          shinyWidgets::show_alert("Error", "Aborted: File path is valid but lacks schema configurations.", type = "error")
        }
      }
    })

    # Observer for handling the creation of a new database.
    observeEvent(input$create_btn, {
      dir_path <- selected_dir_path()
      if (is.null(dir_path)) {
        shinyWidgets::show_alert("Warning", "A root directory must be selected first.", type = "warning")
        return()
      }

      filename <- "Gembank_db.sqlite"

      target_db_path <- file.path(dir_path, filename)

      # Call the internal function to initialize the database schema.
      tryCatch({
        init_db(db_path = target_db_path)
        shinyWidgets::show_alert("Success", "Schema framework architecture generated successfully!", type = "success")
        db_state$path <- target_db_path
      }, error = function(e) {
        shinyWidgets::show_alert("Error", paste("Critical Generation Interruption Error:", e$message), type = "error")
      })
    })
  })
}
