#' Inventory Query Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @noRd
mod_inventory_query_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-4 text-center",
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Inventory Query Builder"
      ),
      p(
        class = "text-muted",
        "Search and filter all physical seed lots in the inventory."
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
            selectizeInput(
              ns("filter_accession"),
              "Accession Name",
              choices = NULL
            ),
            selectizeInput(
              ns("filter_location"),
              "Storage Location",
              choices = NULL
            ),
            selectizeInput(ns("filter_status"), "Seed Status", choices = NULL)
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          selectizeInput(ns("filter_source"), "Seed Source", choices = NULL),
          numericInput(
            ns("filter_low_stock"),
            "Show Lots with Quantity < (g)",
            value = NA,
            min = 0
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

#' Inventory Query Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_inventory_query_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate filter dropdowns on load
    observeEvent(
      db_state$path,
      {
        req(db_state$path)
        tryCatch(
          {
            updateSelectizeInput(
              session,
              "filter_accession",
              choices = c("All", get_all_accessions(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_location",
              choices = c("All", get_all_storage_locations(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_status",
              choices = c("All", get_all_seed_statuses(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_source",
              choices = c("All", get_all_seed_sources(db_state$path)),
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

      # Use NA for threshold if input is empty
      threshold <- if (is.na(input$filter_low_stock)) {
        NULL
      } else {
        input$filter_low_stock
      }

      search_inventory(
        db_path = db_state$path,
        target_accession = input$filter_accession,
        storage_loc = input$filter_location,
        seed_stat = input$filter_status,
        seed_src = input$filter_source,
        low_stock_threshold = threshold
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