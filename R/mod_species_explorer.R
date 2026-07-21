#' Species Explorer Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header value_box layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput renderDT datatable
#' @noRd
mod_species_explorer_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    style = "background-color: #F8FAFC; padding: 20px;",
    div(
      class = "mb-4 text-center",
      h2(class = "fw-bold", style = "color: #0F766E; font-family: 'Outfit', sans-serif;", "Species Explorer"),
      p(class = "text-muted", "Get a high-level overview of all accessions within a specific species.")
    ),
    
    # Species Selector
    bslib::card(
      class = "shadow-sm border-0 mb-4",
      bslib::card_body(
        selectizeInput(
          ns("species_selector"),
          "Select a Species to Explore",
          choices = NULL,
          width = "100%"
        )
      )
    ),
    
    # Summary Value Boxes (dynamically rendered)
    uiOutput(ns("summary_cards_ui")),
    
    # Data Tables (static layout, dynamic content)
    bslib::layout_columns(
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header("Accession List"),
        bslib::card_body(DT::DTOutput(ns("accessions_table")))
      ),
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header("Inventory Summary"),
        bslib::card_body(DT::DTOutput(ns("inventory_table")))
      )
    )
  )
}

#' Species Explorer Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @param global_refresh_trigger A reactiveVal to signal data changes.
#' @return A `reactive` that triggers when an accession is clicked.
#' @noRd
mod_species_explorer_server <- function(id, db_state, global_refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to communicate the clicked accession to the parent server
    clicked_accession <- reactiveVal()
    
    # Populate the species dropdown when the database is connected
    observeEvent(list(db_state$path, global_refresh_trigger()), {
      req(db_state$path)
      species_list <- get_all_species(db_state$path)
      updateSelectizeInput(session, "species_selector", choices = species_list, server = TRUE)
    }, ignoreNULL = TRUE)
    
    # Reactive to fetch all data when a species is selected
    species_data <- eventReactive(input$species_selector, {
      req(db_state$path, input$species_selector)
      
      species <- input$species_selector
      
      list(
        accessions = get_accessions_by_species(db_state$path, species),
        inventory = get_inventory_by_species(db_state$path, species)
      )
    })
    
    # Render the summary value boxes
    output$summary_cards_ui <- renderUI({
      data <- species_data()
      req(data)
      
      inv_summary <- data$inventory
      total_g <- sum(inv_summary$total_grams, na.rm = TRUE)
      total_kg_in_g <- sum(inv_summary$total_kg, na.rm = TRUE) * 1000
      total_inventory_grams <- total_g + total_kg_in_g
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::value_box(
          title = "Total Accessions",
          value = nrow(data$accessions),
          showcase = bs_icon("tree-fill"),
          theme = "success"
        ),
        bslib::value_box(
          title = "Total Seed Weight (grams)",
          value = format(total_inventory_grams, big.mark = ","),
          showcase = bs_icon("box-seam-fill"),
          theme = "info"
        )
      )
    })
    
    # Render the accessions table
    output$accessions_table <- DT::renderDT({
      req(species_data()$accessions)
      DT::datatable(species_data()$accessions, rownames = FALSE, selection = 'single', options = list(dom = 'ftp'))
    })
    
    # Render the inventory summary table
    output$inventory_table <- DT::renderDT({
      req(species_data()$inventory)
      DT::datatable(species_data()$inventory, rownames = FALSE, selection = 'none', options = list(dom = 'ftp'))
    })
    
    # When a row is clicked in the accessions table, capture the accession name
    observeEvent(input$accessions_table_rows_selected, {
      selected_row <- input$accessions_table_rows_selected
      req(selected_row)
      accession_name <- species_data()$accessions[selected_row, "accession_name"]
      clicked_accession(accession_name)
    })
    
    return(clicked_accession)
  })
}