#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Look for a previously saved database path right at startup
  saved_path <- load_db_config()

  # Determine the initial layout view state
  has_db <- !is.null(saved_path)

  bslib::page_sidebar(
    title = "GemBank",

    # Modern, sleek, deep teal/blue scientific theme
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = 'flatly',
      bg = "#F8FAFC",
      fg = "#1E293B",
      primary = "#0F766E", # Deep Teal
      secondary = "#64748B", # Slate Gray
      success = "#059669",
      info = "#0284C7",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Outfit"),
      "navbar-bg" = "#042F2E" # Very dark teal for the sidebar/navbar
    ),

    # Left Control Sidebar mapping local machine context
    sidebar = bslib::sidebar(
      title = "Menu",
      bg = "#f8f9fa",

      # Live indicator box tracking the active local file connection
      shiny::uiOutput("sidebar_status_box"),

      # Placeholder for the conditional disconnect button
      shiny::uiOutput("disconnect_button_ui"),

      shiny::hr(),
      div(
        style = "font-size: 0.8rem; color: #6c757d;",
        "Owner: CARNASA-KNUST"
      ),
      div(
        style = "font-size: 0.8rem; color: #6c757d;",
        "Environment: Local Host Desktop"
      )
    ),

    # Central Content Workspace: Controlled by a conditional tab panel matrix
    bslib::navset_card_pill(
      id = "main_tabs",
      title = " ",
      full_screen = TRUE,

      # Tab A: Storage Connection Node (Always visible)
      bslib::nav_panel(
        title = "Database Connection",
        value = "db_connection_tab",
        icon = bsicons::bs_icon("gear-fill"),
        # We explicitly call the onboarding setup UI module right here inside app_ui!
        mod_setup_ui("setup_panel")
      ),

      # Tab B: Functional Data Dashboard (Conditionally unlocked/shown by server)
      bslib::nav_panel(
        title = "New Entry",
        value = "new_entry_tab",
        icon = bsicons::bs_icon("table"),
        mod_data_entry_ui("data_entry_panel")
      ),

      # Tab C: Inventory Dashboard (Conditionally unlocked/shown by server)
      bslib::nav_panel(
        title = "Inventory Management",
        value = "transaction_log_tab",
        icon = bsicons::bs_icon("boxes"),
        mod_transaction_log_ui("inventory_panel")
      ),
      # Tab D: Passport Explorer
      bslib::nav_panel(
        title = "Passport Explorer",
        value = "passport_explorer_tab",
        icon = bsicons::bs_icon("person-vcard-fill"),
        mod_passport_explorer_ui("passport_explorer")
      ),
      # Tab E: Species Explorer
      bslib::nav_panel(
        title = "Species Explorer",
        value = "species_explorer_tab",
        icon = bsicons::bs_icon("diagram-3-fill"),
        mod_species_explorer_ui("species_explorer")
      ),
      # Tab F: Germplasm Query Builder
      bslib::nav_menu(
        title = "Query Database",
        icon = bsicons::bs_icon("funnel-fill"),
        bslib::nav_panel(
          title = "Germplasm Query",
          value = "germplasm_query_panel",
          mod_germplasm_query_ui("germplasm_query_1")
        ),
        bslib::nav_panel(
          title = "Inventory Query",
          value = "inventory_query_panel",
          mod_inventory_query_ui("inventory_query_1")
        ),
        bslib::nav_panel(
          title = "Trial Query",
          value = "trial_query_panel",
          mod_trial_query_ui("trial_query_1")
        ),
        bslib::nav_panel(
          title = "Plot Query",
          mod_plot_query_ui("plot_query_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "GemBank"
		),
		# Add here other external resources
		# for example, you can add shinyalert::useShinyalert(),
		shinyWidgets::useShinyWidgets()
	)
}
