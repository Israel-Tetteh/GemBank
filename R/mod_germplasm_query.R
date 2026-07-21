#' Germplasm Query Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @noRd
mod_germplasm_query_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-4 text-center mt-3",
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Germplasm Query Builder"
      ),
      p(
        class = "text-muted",
        "Search and filter the entire germplasm collection to find specific accessions."
      )
    ),

    # Filter Card
    bslib::card(
      height = "800px",
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
              ns("filter_bio_status"),
              "Biological Status",
              choices = NULL
            ),
            selectizeInput(
              ns("filter_acc_type"),
              "Accession Type",
              choices = NULL
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          selectizeInput(ns("filter_source"), "Seed Source", choices = NULL),
          selectizeInput(
            ns("filter_country"),
            "Country of Origin",
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

#' Germplasm Query Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_germplasm_query_server <- function(id, db_state) {
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
              "filter_bio_status",
              choices = c("All", get_all_biological_statuses(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_acc_type",
              choices = c("All", get_all_accession_types(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_source",
              choices = c("All", get_all_seed_sources(db_state$path)),
              server = TRUE
            )
            updateSelectizeInput(
              session,
              "filter_country",
              choices = c("All", get_all_countries_of_origin(db_state$path)),
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

      # Show a loading notification while the query runs
      id <- showNotification(
        "Running query...",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)

      search_germplasm(
        db_path = db_state$path,
        target_accession = input$filter_accession,
        bio_status = input$filter_bio_status,
        acc_type = input$filter_acc_type,
        source_cat = input$filter_source,
        origin_country = input$filter_country
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