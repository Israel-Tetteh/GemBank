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
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(-1, 4, 5, 1, -1), # Perfectly scales across the dashboard

        textInput(
          ns("user_name"),
          "Breeder's Name (for edits)",
          value = "Israel Tetteh",
          width = "100%"
        ),

        textInput(
          ns("search_accession"),
          "Enter Accession Name",
          placeholder = "e.g., sc-2026-001",
          width = "100%"
        ),

        # Flex wrapper pushes the button down to perfectly line up with the inputs
        div(
          class = "d-flex align-items-end",
          style = "height: 100%;",
          actionButton(
            ns("btn_search"),
            "Search",
            icon = bs_icon("search"),
            class = "text-white w-100",
            style = "background-color: #0F766E; border-color: #0F766E; height: 38px; font-weight: 500;"
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
#' @noRd
mod_passport_explorer_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to hold search results and UI state
    search_result <- reactiveVal(NULL)
    is_editing <- reactiveVal(FALSE)

    # Trigger for search
    observeEvent(input$btn_search, {
      req(db_state$path, input$search_accession)
      is_editing(FALSE) # Reset state for new search
      accession <- tolower(trimws(input$search_accession))

      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), db_state$path)
        on.exit(DBI::dbDisconnect(con))
        basic_info <- DBI::dbGetQuery(con, "SELECT * FROM germplasm WHERE accession_name = ?", params = list(accession))

        if (nrow(basic_info) == 0) stop("Accession not found in the database.")

        search_result(list(
          basic_info = basic_info,
          passport = get_germplasm_passport(db_state$path, accession),
          inventory = get_inventory_status(db_state$path, accession),
          raw_data = get_raw_accession_data(db_state$path, accession),
          audit_log = get_accession_ledger(db_state$path, accession)
        ))
      }, error = function(e) {
        shinyWidgets::show_alert("Error", e$message, type = "error")
        search_result(NULL)
      })
    })

    # Dynamic UI for results
    output$results_ui <- renderUI({
      result <- search_result(); req(result)

      if (is_editing()) {
        # UI for editing mode
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(bs_icon("pencil-square"), " Editing Passport Details"),
          bslib::card_body(
            p(strong("Accession: "), result$basic_info$accession_name),
            textInput(ns("edit_species"), "Species", value = result$basic_info$species),
            textInput(ns("edit_pedigree"), "Pedigree", value = result$basic_info$pedigree),
            hr(),
            actionButton(ns("btn_save"), "Save Changes", class = "btn-success"),
            actionButton(ns("btn_cancel"), "Cancel", class = "btn-secondary")
          )
        )
      } else {
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
              p(strong("Accession: "), result$basic_info$accession_name),
              p(strong("Species: "), result$basic_info$species),
              p(strong("Pedigree: "), result$basic_info$pedigree)
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

      tryCatch({
        update_germplasm(
          db_path = db_state$path,
          accession_name = result$basic_info$accession_name,
          new_species = input$edit_species,
          new_pedigree = input$edit_pedigree,
          user_name = input$user_name
        )
        shinyWidgets::show_alert("Success", "Germplasm details updated successfully!", type = "success")
        is_editing(FALSE)

        # Refresh data
        accession <- tolower(trimws(input$search_accession))
        con <- DBI::dbConnect(RSQLite::SQLite(), db_state$path)
        on.exit(DBI::dbDisconnect(con))
        basic_info <- DBI::dbGetQuery(con, "SELECT * FROM germplasm WHERE accession_name = ?", params = list(accession))
        search_result(list(
          basic_info = basic_info,
          passport = get_germplasm_passport(db_state$path, accession),
          inventory = get_inventory_status(db_state$path, accession),
          raw_data = get_raw_accession_data(db_state$path, accession),
          audit_log = get_accession_ledger(db_state$path, accession)
        ))
      }, error = function(e) {
        shinyWidgets::show_alert("Error", e$message, type = "error")
      })
    })
  })
}
