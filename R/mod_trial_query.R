#' Trial Query Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @noRd
mod_trial_query_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-4 text-center",
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Trial Query Builder"
      ),
      p(
        class = "text-muted",
        "Search and filter all historical and active trials."
      )
    ),

    # Filter Card
    bslib::card(
      height = '800px',
      bslib::card_header(
        class = "d-flex align-items-center",
        bsicons::bs_icon("funnel-fill", class = "me-2"),
        "Filter Criteria"
      ),
      bslib::card_body(
        div(
          class = "mb-3",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            selectizeInput(ns("filter_trial"), "Trial Name", choices = NULL),
            selectizeInput(ns("filter_year"), "Year", choices = NULL),
            selectizeInput(ns("filter_season"), "Season", choices = NULL)
          )
        ),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          selectizeInput(ns("filter_status"), "Trial Status", choices = NULL),
          selectizeInput(ns("filter_type"), "Trial Type", choices = NULL),
          selectizeInput(
            ns("filter_pi"),
            "Principal Investigator",
            choices = NULL
          )
        ),
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(
            ns("btn_query"),
            "Run Query",
            icon = bs_icon("search"),
            class = "btn-primary rounded-pill fw-bold px-4"
          )
        )
      )
    ),

    # Results Card
    bslib::card(
      class = "shadow-sm border-0",
      bslib::card_header(tagList(bsicons::bs_icon("table"), " Query Results")),
      bslib::card_body(
        DT::DTOutput(ns("results_table"))
      )
    )
  )
}

#' Trial Query Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @param global_refresh_trigger A reactiveVal to signal data changes.
#' @noRd
mod_trial_query_server <- function(id, db_state, global_refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate filter dropdowns on load
    observeEvent(
      list(db_state$path, global_refresh_trigger()),
      {
        req(db_state$path)
        tryCatch(
          {
            updateSelectizeInput(
              session,
              "filter_trial",
              choices = c("All", get_all_trials(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_year",
              choices = c("All", get_all_trial_years(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_season",
              choices = c("All", get_all_trial_seasons(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_status",
              choices = c("All", get_all_trial_statuses(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_type",
              choices = c("All", get_all_trial_types(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_pi",
              choices = c(
                "All",
                get_all_principal_investigators(db_state$path)
              ),
              server = TRUE
            )
          },
          error = function(e) {
            # Fail silently if DB is empty
          }
        )
      },
      ignoreNULL = TRUE
    )

    # Reactive to store query results, triggered by the button
    query_results <- eventReactive(input$btn_query, {
      req(db_state$path)

      id <- showNotification(
        "Running query...",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)

      search_trials(
        db_path = db_state$path,
        target_trial = input$filter_trial,
        trial_year = input$filter_year,
        trial_season = input$filter_season,
        t_status = input$filter_status,
        t_type = input$filter_type,
        pi_name = input$filter_pi
      )
    })

    # Render the results table
    output$results_table <- DT::renderDT({
      df <- query_results()
      req(df)

      DT::datatable(
        df,
        extensions = 'Buttons',
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    })
  })
}