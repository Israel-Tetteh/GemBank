#' Plot Query Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @noRd
mod_plot_query_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-4 text-center",
      h2(class = "fw-bold", style = "color: #0F766E; font-family: 'Outfit', sans-serif;", "Plot Query Builder"),
      p(class = "text-muted", "Search and filter all field plots across all trials.")
    ),
    
    # Filter Card
      bslib::card(
      height = "500px",
      #class = "shadow-sm border-0 mb-4",
      bslib::card_header("Query Filters"),
      bslib::card_body(
          div(
          class = "mb-3",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          textInput(ns("filter_trial"), "Trial Name (contains)", placeholder = "e.g., BDT-2026"),
          textInput(ns("filter_accession"), "Accession Name (contains)", placeholder = "e.g., SC-2026-001"),
          selectizeInput(ns("filter_rep"), "Replication Number", choices = NULL)
        )),
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(
            ns("btn_query"), "Run Query", 
            icon = bs_icon("search"), 
            class = "btn-primary rounded-pill px-4"
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

#' Plot Query Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_plot_query_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Populate filter dropdowns on load
    observeEvent(db_state$path, {
      req(db_state$path)
      tryCatch({
        updateSelectizeInput(session, "filter_rep", choices = c("All", get_all_replications(db_state$path)), server = TRUE)
      }, error = function(e) {
        # Fail silently if DB is empty
      })
    }, ignoreNULL = TRUE)
    
    # Reactive to store query results, triggered by the button
    query_results <- eventReactive(input$btn_query, {
      req(db_state$path)
      
      id <- showNotification("Running query...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      search_plots(
        db_path = db_state$path,
        search_trial = input$filter_trial,
        search_acc = input$filter_accession,
        rep_num = input$filter_rep
      )
    })
    
    # Render the results table
    output$results_table <- DT::renderDT({
      df <- query_results()
      req(df)
      
      DT::datatable(
        df,
        extensions = 'Buttons',
        options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    })
    
  })
}