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
    
    # Results Area
    uiOutput(ns("species_results_ui"))
  )
}

#' Species Explorer Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_species_explorer_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Populate the species dropdown when the database is connected
    observeEvent(db_state$path, {
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
    
    # Render the dynamic UI for the selected species
    output$species_results_ui <- renderUI({
      data <- species_data()
      req(data)
      
      # Calculate summary stats
      inv_summary <- data$inventory
      total_accessions <- nrow(data$accessions)
      
      total_g <- sum(inv_summary$total_grams, na.rm = TRUE)
      total_kg_in_g <- sum(inv_summary$total_kg, na.rm = TRUE) * 1000
      total_inventory_grams <- total_g + total_kg_in_g
      
      tagList(
        # Summary Value Boxes
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::value_box(
            title = "Total Accessions",
            value = total_accessions,
            showcase = bs_icon("tree-fill"),
            theme = "success"
          ),
          bslib::value_box(
            title = "Total Seed Weight (grams)",
            value = format(total_inventory_grams, big.mark = ","),
            showcase = bs_icon("box-seam-fill"),
            theme = "info"
          )
        ),
        
        # Data Tables
        bslib::layout_columns(
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header("Accession List"),
            bslib::card_body(DT::renderDT(DT::datatable(data$accessions, rownames = FALSE)))
          ),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header("Inventory Summary"),
            bslib::card_body(DT::renderDT(DT::datatable(data$inventory, rownames = FALSE)))
          )
        )
      )
    })
    
  })
}