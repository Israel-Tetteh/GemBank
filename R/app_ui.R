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

      shiny::hr(),
      div(
        style = "font-size: 0.8rem; color: #6c757d;",
        "Operator: Israel Tetteh"
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
        icon = bsicons::bs_icon("gear-fill"),
        # We explicitly call the onboarding setup UI module right here inside app_ui!
        mod_setup_ui("setup_panel")
      ),

      # Tab B: Functional Data Dashboard (Conditionally unlocked/shown by server)
      bslib::nav_panel(
        title = "New Entry",
        icon = bsicons::bs_icon("table"),
        shiny::uiOutput("conditional_dashboard_ui")
      ),

      # Tab C: Inventory Dashboard (Conditionally unlocked/shown by server)
      bslib::nav_panel(
        title = "Transaction Log Dashboard",
        icon = bsicons::bs_icon("boxes"),
        shiny::uiOutput("conditional_inventory_ui")
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
