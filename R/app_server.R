#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib nav_hide nav_show
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
    # This observer controls which tabs are visible based on DB connection state.
    is_connected <- !is.null(db_state$path) && db_state$path != ""

    if (is_connected) {
      # DB is connected: Hide setup, show data tabs.
      # When a valid database path is set, save it for future sessions.
      save_db_config(db_state$path)

      bslib::nav_hide(id = "main_tabs", target = "db_connection_tab", session = session)
      bslib::nav_show(id = "main_tabs", target = "new_entry_tab", session = session)
      bslib::nav_show(id = "main_tabs", target = "transaction_log_tab", session = session)
      bslib::nav_show(id = "main_tabs", target = "passport_explorer_tab", session = session)
      bslib::nav_show(id = "main_tabs", target = "species_explorer_tab", session = session)
      bslib::nav_show(id = "main_tabs", target = "germplasm_query_tab", session = session)

      # Select a default data tab to show the user.
      bslib::nav_select("main_tabs", selected = "new_entry_tab", session = session)
    } else {
      # DB is disconnected: Show setup, hide data tabs.
      bslib::nav_show(id = "main_tabs", target = "db_connection_tab", session = session)
      bslib::nav_hide(id = "main_tabs", target = "new_entry_tab", session = session)
      bslib::nav_hide(id = "main_tabs", target = "transaction_log_tab", session = session)
      bslib::nav_hide(id = "main_tabs", target = "passport_explorer_tab", session = session)
      bslib::nav_hide(id = "main_tabs", target = "species_explorer_tab", session = session)
      bslib::nav_hide(id = "main_tabs", target = "germplasm_query_tab", session = session)

      # Ensure the connection tab is selected.
      bslib::nav_select("main_tabs", selected = "db_connection_tab", session = session)
    }
  }, label = "Tab Visibility Controller")

  # Initialize the server logic for the inventory ledger module.
  mod_transaction_log_server("inventory_panel", db_state)

  # --- Cross-Module Communication Setup ---
  # Create a reactive value to allow other modules to trigger a search in the passport explorer.
  passport_search_term <- reactiveVal()

  # Initialize the server logic for the passport explorer module.
  mod_passport_explorer_server("passport_explorer", db_state, passport_search_term)

  # Initialize the server logic for the species explorer module.
  # It returns a reactive that fires when an accession is clicked.
  clicked_from_species <- mod_species_explorer_server("species_explorer", db_state)

  # When an accession is clicked in the species explorer, update the passport search term
  # and navigate the user to the passport explorer tab.
  observeEvent(clicked_from_species(), {
    req(clicked_from_species())
    # Set the search term for the passport module
    passport_search_term(clicked_from_species())
    # Switch the user to the passport explorer tab
    bslib::nav_select("main_tabs", selected = "passport_explorer_tab")
  })
  
  # Initialize the server logic for the germplasm query module.
  mod_germplasm_query_server("germplasm_query_1", db_state)

  # Initialize the server logic for the inventory query module.
  mod_inventory_query_server("inventory_query_1", db_state)

  # Initialize the server logic for the trial query module.
  mod_trial_query_server("trial_query_1", db_state)

  # Initialize the server logic for the plot query module.
  mod_plot_query_server("plot_query_1", db_state)

  # Render the disconnect button conditionally in the sidebar
  output$disconnect_button_ui <- renderUI({
    # Only show the button if a database path is set
    req(db_state$path)

    shiny::actionButton(
      "disconnect_btn",
      "Disconnect Session",
      icon = bsicons::bs_icon("power"),
      class = "btn-danger w-100 mt-3" # Added margin-top for spacing
    )
  })

  # Observer to handle the disconnect button click
  observeEvent(input$disconnect_btn, {
    # Call the centralized utility function to handle the disconnect logic.
    disconnect_db_session(db_state, session)
  })
  
}
