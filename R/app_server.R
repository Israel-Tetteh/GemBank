#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize a reactive value for the database path.
  db_state <- shiny::reactiveValues(path = load_db_config())

  # Initialize the server logic for the database setup module.
  mod_setup_server("setup_panel", db_state)
  
  # Initialize the server logic for the data entry module.
  mod_data_entry_server("data_entry_panel", db_state)

  # Dynamically render the sidebar connection status UI.
  output$sidebar_status_box <- shiny::renderUI({
    if (is.null(db_state$path)) {
      div(
          class = "card border-danger-subtle bg-white shadow-sm",
          div(
            class = "card-header bg-danger-subtle fw-bold text-danger py-1 small",
            "Status: Disconnected"
          ),
          div(
            class = "card-body p-2 small text-muted",
            "Specify an SQLite workspace path to unlock the ledger database panels."
          )
        )
    } else {
      # When a valid database path is set, save it for future sessions.
      save_db_config(db_state$path)

      div(
          class = "card border-success-subtle bg-white shadow-sm",
          div(
            class = "card-header bg-success-subtle fw-bold text-success py-1 small",
            "Status: Connected"
          ),
          div(
            class = "card-body p-2 font-monospace small text-truncate text-muted",
            bsicons::bs_icon("hdd-fill", class = "text-success me-1"),
            basename(db_state$path)
          )
        )
    }
  })


  shiny::observe({
    # If a database is connected, switch to the inventory view.
    if (!is.null(db_state$path)) {
      bslib::nav_select("main_tabs", selected = "Inventory Master Ledger")
    } else {
      # Otherwise, ensure the user is on the setup page.
      bslib::nav_select("main_tabs", selected = "Database Connection")
    }
  })


  output$conditional_dashboard_ui <- shiny::renderUI({
    if (is.null(db_state$path)) {
      div(
          class = "p-5 text-center text-muted",
          bsicons::bs_icon(
            "lock-fill",
            size = "3rem",
            class = "text-warning mb-3"
          ),
          shiny::h4("Ledger Locked"),
          shiny::p(
            "Please mount or initialize an SQLite database file in the first tab to view records."
          )
        )
    } else {
      # Show the data entry UI only when the database is available.
      mod_data_entry_ui("data_entry_panel")
    }
  })
    
  # Conditionally render the inventory dashboard UI.
  output$conditional_inventory_ui <- shiny::renderUI({
    if (is.null(db_state$path)) {
      div(
          class = "p-5 text-center text-muted",
          bsicons::bs_icon(
            "lock-fill",
            size = "3rem",
            class = "text-warning mb-3"
          ),
          shiny::h4("Vault Locked"),
          shiny::p(
            "Please mount or initialize an SQLite database file in the first tab to access the inventory dashboard."
          )
        )
    } else {
      # Show the inventory UI only when the database is available.
      mod_inventory_ledger_ui("inventory_panel")
    }
  })
    
  # Initialize the server logic for the inventory ledger module.
  mod_inventory_ledger_server("inventory_panel", db_state)
}
