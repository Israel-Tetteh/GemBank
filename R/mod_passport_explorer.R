#' Passport Explorer Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_sidebar layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput renderDT datatable
#' @noRd
mod_passport_explorer_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    style = "background-color: #F8FAFC; padding: 20px;",
    div(
      class = "mb-4 text-center",
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Passport Explorer"
      ),
      p(
        class = "text-muted",
        "Search for an accession to view its complete passport, inventory status, and trial history."
      )
    ),

    # Search Bar
    bslib::card(
      class = "shadow-sm border-0 mb-4",
      bslib::card_header("Search Accession"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(4, 6, 2),
          textInput(
            ns("user_name"),
            "Breeder's Name (for edits)",
            value = "Israel Tetteh"
          ),
          textInput(
            ns("search_accession"),
            "Enter Accession Name",
            placeholder = "e.g., sc-2026-001"
          ),
          div(
            class = "d-flex align-items-end h-100",
          actionButton(
            ns("btn_search"),
            "Search",
            icon = bs_icon("search"),
            class = "btn-primary w-100"
          )
          )
        )
      )
    ),

    # Results Area
    uiOutput(ns("results_ui"))
  )
}

#' Passport Explorer Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @param search_term A `reactiveVal` that can be set by other modules to trigger a search.
#' @noRd
mod_passport_explorer_server <- function(id, db_state, search_term = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to hold search results and UI state
    search_result <- reactiveVal(NULL)
    is_editing <- reactiveVal(FALSE)

    # Helper function to fetch all data for a given accession
    # This centralizes the data fetching logic and avoids code duplication
    fetch_passport_data <- function(accession_name) {
      req(db_state$path, accession_name)
      accession <- toupper(trimws(accession_name))
      
      id <- showNotification(paste("Fetching data for", accession, "..."), duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id))
      
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), db_state$path)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        basic_info <- DBI::dbGetQuery(con, "SELECT * FROM germplasm WHERE accession_name = ?", params = list(accession))
        
        if (nrow(basic_info) == 0) stop("Accession not found in the database.")
        
        list(
          basic_info = basic_info,
          passport = get_germplasm_passport(db_state$path, accession),
          inventory = get_inventory_status(db_state$path, accession),
          raw_data = get_raw_accession_data(db_state$path, accession),
          audit_log = get_accession_audit_log(db_state$path, accession)
        )
      }, error = function(e) {
        shinyWidgets::show_alert("Error", e$message, type = "error")
        NULL # Return NULL on error
      })
    }

    # Trigger for search
    observeEvent(input$btn_search, {
      req(db_state$path, input$search_accession)
      is_editing(FALSE) # Reset state for new search
      search_result(fetch_passport_data(input$search_accession))
    })
    
    # Observer to handle searches triggered from other modules
    observeEvent(search_term(), {
      req(search_term())
      # Update the UI's text input so the user sees what's being searched
      updateTextInput(session, "search_accession", value = search_term())
      # Trigger the search logic
      is_editing(FALSE)
      search_result(fetch_passport_data(search_term()))
      search_term(NULL) # Reset the trigger
    })

    # Dynamic UI for results
    output$results_ui <- renderUI({
      result <- search_result(); req(result)

      if (is_editing()) {
        # UI for editing mode
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(bs_icon("pencil-square"), " Editing Passport Details"),
          bslib::card_body(p(strong("Accession: "), result$basic_info$accession_name),
            bslib::layout_column_wrap(width = 1/3,
              textInput(ns("edit_preferred_name"), "Preferred/Local Name", value = result$basic_info$preferred_name),
              textInput(ns("edit_species"), "Species", value = result$basic_info$species),
              selectInput(ns("edit_biological_status"), "Biological Status",
                          choices = c("Unknown", "Landrace", "Breeding Line", "Cultivar", "Wild Relative", "Population"),
                          selected = result$basic_info$biological_status),
              selectInput(ns("edit_accession_type"), "Accession Type",
                          choices = c("", "Released Variety", "Advanced Line", "Experimental Line", "Parent", "Check", "Elite Line", "Population"),
                          selected = result$basic_info$accession_type),
              selectInput(ns("edit_status"), "Status",
                          choices = c("Available", "Inactive", "Archived", "Exhausted", "Regenerating"),
                          selected = result$basic_info$status),
              selectInput(ns("edit_seed_source"), "Seed Source",
                          choices = c("", "Harvest", "Research Institution", "Farmer", "Gene Bank", "Purchase", "Donation", "Exchange", "Company", "Unknown"),
                          selected = result$basic_info$seed_source),
              textInput(ns("edit_source_name"), "Source Name", value = result$basic_info$source_name),
              textInput(ns("edit_country_of_origin"), "Country of Origin", value = result$basic_info$country_of_origin),
              textInput(ns("edit_collection_site"), "Collection Site", value = result$basic_info$collection_site),
              textInput(ns("edit_collector_name"), "Collector Name", value = result$basic_info$collector_name),
              dateInput(ns("edit_acquisition_date"), "Acquisition Date", value = result$basic_info$acquisition_date),
              textInput(ns("edit_pedigree"), "Pedigree", value = result$basic_info$pedigree),
              textInput(ns("edit_generation"), "Generation", value = result$basic_info$generation)
            ),
            textAreaInput(ns("edit_remarks"), "Remarks", value = result$basic_info$remarks, width = "100%", rows = 3),
            hr(),
            actionButton(ns("btn_save"), "Save Changes", class = "btn-success"),
            actionButton(ns("btn_cancel"), "Cancel", class = "btn-secondary")
          )
        )
      } else {
        # Helper function to render a detail item, hiding if value is NULL or empty
        render_detail <- function(label, value) {
          if (!is.null(value) && !is.na(value) && value != "") {
            p(strong(paste0(label, ": ")), value)
          }
        }
        
        # UI for viewing mode
        bslib::layout_columns(
          col_widths = c(6, 6, 12, 12, 12),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              span(bs_icon("person-vcard-fill"), " Passport Details"),
              actionButton(ns("btn_edit"), "Edit", icon = bs_icon("pencil-square"), class = "btn-sm btn-outline-secondary")
            ),
            bslib::card_body(
              render_detail("Accession", result$basic_info$accession_name),
              render_detail("Preferred Name", result$basic_info$preferred_name),
              render_detail("Species", result$basic_info$species),
              render_detail("Pedigree", result$basic_info$pedigree),
              render_detail("Generation", result$basic_info$generation),
              render_detail("Biological Status", result$basic_info$biological_status),
              render_detail("Accession Type", result$basic_info$accession_type),
              render_detail("Status", result$basic_info$status),
              render_detail("Seed Source", result$basic_info$seed_source),
              render_detail("Source Name", result$basic_info$source_name),
              render_detail("Country of Origin", result$basic_info$country_of_origin),
              render_detail("Collection Site", result$basic_info$collection_site),
              render_detail("Collector Name", result$basic_info$collector_name),
              render_detail("Acquisition Date", result$basic_info$acquisition_date),
              render_detail("Remarks", result$basic_info$remarks)
            )
          ),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header(bs_icon("box-seam-fill"), " Inventory Status"),
            bslib::card_body(DT::renderDT(DT::datatable(result$inventory, options = list(dom = 't'), rownames = FALSE)))
          ),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header(bs_icon("clipboard-data-fill"), " Trial Performance History"),
            bslib::card_body(
              if (nrow(result$passport) > 0) {
                DT::renderDT(DT::datatable(result$passport, options = list(scrollX = TRUE), rownames = FALSE))
              } else {
                p("No trial performance data available for this accession.")
              }
            )
          ),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header(bs_icon("table"), " Raw Field Data"),
            bslib::card_body(
              if (nrow(result$raw_data) > 0) {
                DT::renderDT(DT::datatable(result$raw_data, options = list(scrollX = TRUE), rownames = FALSE))
              } else {
                p("No raw field data available for this accession.")
              }
            )
          ),
          bslib::card(
            class = "shadow-sm border-0",
            bslib::card_header(bs_icon("journal-check"), " Accession Audit Trail"),
            bslib::card_body(
              if (nrow(result$audit_log) > 0) {
                DT::renderDT(DT::datatable(result$audit_log, options = list(scrollX = TRUE), rownames = FALSE))
              } else {
                p("No audit trail available for this accession.")
              }
            )
          )
        )
      }
    })

    # Event handlers for edit/save/cancel
    observeEvent(input$btn_edit, { is_editing(TRUE) })
    observeEvent(input$btn_cancel, { is_editing(FALSE) })

    observeEvent(input$btn_save, {
      result <- search_result(); req(result, input$user_name)

      # Collect all updated values into a named list
      updates <- list(
        preferred_name = input$edit_preferred_name,
        species = input$edit_species,
        pedigree = input$edit_pedigree,
        biological_status = input$edit_biological_status,
        accession_type = input$edit_accession_type,
        status = input$edit_status,
        seed_source = input$edit_seed_source,
        source_name = input$edit_source_name,
        country_of_origin = input$edit_country_of_origin,
        collection_site = input$edit_collection_site,
        collector_name = input$edit_collector_name,
        acquisition_date = as.character(input$edit_acquisition_date),
        generation = input$edit_generation,
        remarks = input$edit_remarks
      )

      tryCatch({
        update_germplasm(
          db_path = db_state$path,
          accession_name = result$basic_info$accession_name,
          updates = updates,
          user_name = input$user_name
        )
        shinyWidgets::show_alert("Success", "Germplasm details updated successfully!", type = "success")
        is_editing(FALSE)

        # Refresh data by calling the centralized helper function
        search_result(fetch_passport_data(input$search_accession))

      }, error = function(e) {
        shinyWidgets::show_alert("Error", e$message, type = "error")
      })
    })
  })
}
