#' Transaction Log Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_column_wrap layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT renderDT datatable formatStyle styleInterval DTOutput
#' @importFrom shinyWidgets show_alert
#' @noRd
mod_transaction_log_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    style = "background-color: #F8FAFC; padding: 20px;",
    
    div(
      class = "mb-4 text-center",
      h2(class = "fw-bold", style = "color: #0F766E; font-family: 'Outfit', sans-serif;", "Transaction Log"),
      p(class = "text-muted", "Monitor real-time seed balances and execute inventory withdrawals.")
    ),
    
    bslib::layout_columns(
      col_widths = c(8, 4),
      
      # LEFT: Live Inventory Table
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header(
          class = "bg-white fw-bold",
          tagList(bsicons::bs_icon("table"), " Live Vault Balances")
        ),
        bslib::card_body(
          DT::DTOutput(ns("inventory_table"))
        )
      ),
      
      # RIGHT: Withdrawal Form
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header(
          class = "bg-white fw-bold text-danger",
          tagList(bsicons::bs_icon("box-arrow-up-right"), " Withdraw Seed")
        ),
        bslib::card_body(
          class = "p-4",
          p(class = "text-muted small mb-4", "Remove seeds from the cold room for planting, sharing, or testing."),
          
          selectizeInput(
            ns("w_accession"),
            "Target Accession",
            choices = NULL,
            width = "100%"
          ),
          
          selectizeInput(
            ns("w_location"),
            "Storage Location",
            choices = NULL, # Will be populated dynamically
            width = "100%"
          ),
          # Dynamic text to show available stock
          shiny::div(
            class = "alert alert-info py-2 px-3 mt-2 small",
            style = "background-color: #E0F2FE !important; color: #075985 !important; border-color: #BAE6FD !important;",
            shiny::textOutput(ns("available_stock_text"))
          ),
          
          bslib::layout_column_wrap(
            width = 1/2,
            numericInput(
              ns("w_amount"),
              "Amount",
              value = 10,
              min = 0.1,
              width = "100%"
            ),
            selectInput(
              ns("w_unit"),
              "Unit",
              choices = c("grams", "kg"),
              selected = "grams",
              width = "100%"
            )
          ),
          
          textInput(
            ns("w_operator"),
            "Breeder's Name:",
            placeholder = "e.g., Dr. Kena",
            width = "100%"
          ),
          
          textInput(
            ns("w_reason"),
            "Reason for Withdrawal",
            placeholder = "e.g., Planting 2026 Trial",
            width = "100%"
          ),
          
          hr(),
          
          actionButton(
            ns("btn_withdraw"),
            "Execute Withdrawal",
            class = "btn btn-danger rounded-pill fw-bold w-100"
          )
        )
      )
    )
  )
}

#' Transaction Log Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_transaction_log_server <- function(id, db_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactive trigger to manually refresh data after a database write (e.g., withdrawal).
    refresh_trigger <- reactiveVal(0)
    
    # Reactive value to hold inventory data for the currently selected accession.
    selected_accession_inventory <- reactiveVal(data.frame())
    
    # Render the main data table showing current inventory levels.
    output$inventory_table <- DT::renderDT({
      path <- db_state$path
      req(path)
      refresh_trigger() # Establish a dependency on the refresh trigger.
      
      tryCatch({
        df <- get_inventory_status(path)
        
        # Apply conditional styling to highlight low-stock accessions.
        DT::datatable(
          df,
          extensions = 'Buttons',
          options = list(
            pageLength = 10,
            dom = 'Bfrtip', # 'B' for Buttons
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) |>
          DT::formatStyle(
            'quantity',
            color = DT::styleInterval(c(50, 500), c('red', 'orange', 'black')),
            fontWeight = DT::styleInterval(50, c('bold', 'normal'))
          )
      }, error = function(e) {
        data.frame(Message = "No inventory data found or error loading table.")
      })
    })
    
    # Observer to update accession choices when the database path or data changes.
    observeEvent(list(db_state$path, refresh_trigger()), {
      path <- db_state$path
      req(path)
      
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), path)
        on.exit(DBI::dbDisconnect(con))
        
        # Directly and efficiently query for only those accessions that have
        # at least one entry in the inventory table. This is more robust than
        # fetching all germplasm and filtering in R.
        acc_choices <- DBI::dbGetQuery(con, 
          "SELECT DISTINCT g.accession_name 
           FROM germplasm g 
           JOIN inventory i ON g.germplasm_id = i.germplasm_id
           ORDER BY g.accession_name"
        )$accession_name
        updateSelectizeInput(session, "w_accession", choices = acc_choices, selected = character(0), server = TRUE)
      }, error = function(e) {
        # Fail silently if the database is empty or an error occurs during the fetch.
      })
    })
    
    # Observer to dynamically update storage locations based on selected accession.
    observeEvent(input$w_accession, {
      path <- db_state$path
      accession <- input$w_accession
      
      # Ensure inputs are available before proceeding.
      req(path, accession, accession != "")
      
      tryCatch({
        # Directly query for the locations and stock of the selected accession.
        # This is more robust and efficient than the previous implementation.
        con <- DBI::dbConnect(RSQLite::SQLite(), path)
        on.exit(DBI::dbDisconnect(con))
        
        query <- "
          SELECT i.storage_location, i.quantity, i.unit
          FROM inventory i
          JOIN germplasm g ON i.germplasm_id = g.germplasm_id
          WHERE g.accession_name = ?
        "
        acc_inventory <- DBI::dbGetQuery(con, query, params = list(accession))
        
        # Store this data for use in other observers
        selected_accession_inventory(acc_inventory)
        
        # Update the location dropdown with valid choices.
        updateSelectizeInput(session, "w_location", choices = acc_inventory$storage_location, selected = character(0), server = TRUE)
        
      }, error = function(e) {
        updateSelectizeInput(session, "w_location", choices = character(0), server = TRUE)
      })
    }, ignoreInit = TRUE) # ignoreInit prevents running on startup before w_accession is set.
    
    # Observer to handle the 'Execute Withdrawal' button click.
    # This now shows a confirmation modal instead of withdrawing directly.
    observeEvent(input$btn_withdraw, {
      req(input$w_accession, input$w_location, input$w_amount, input$w_operator, input$w_reason)
      path <- db_state$path
      req(path)
      
      showModal(modalDialog(
        title = tagList(bsicons::bs_icon("exclamation-triangle-fill", class = "text-warning"), " Confirm Withdrawal"),
        p(
          "Please confirm you want to withdraw",
          strong(paste(input$w_amount, input$w_unit)), "of",
          strong(input$w_accession), "from",
          strong(input$w_location), "."
        ),
        p("This action will be recorded in the transaction log and cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_confirm_withdraw"), "Confirm Withdrawal", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    
    # Observer for the final confirmation button inside the modal.
    observeEvent(input$btn_confirm_withdraw, {
      # Remove the modal dialog
      removeModal()
      
      tryCatch({
        withdraw_seed(
          db_path = db_state$path,
          accession_name = input$w_accession,
          storage_location = trimws(input$w_location),
          withdraw_amount = input$w_amount,
          withdraw_unit = input$w_unit,
          user_name = trimws(input$w_operator),
          reason = trimws(input$w_reason)
        )

        shinyWidgets::show_alert(
          title = "Success",
          text = "Seed withdrawn successfully!",
          type = "success"
        )

        # Reset form inputs for the next transaction.
        updateSelectizeInput(session, "w_accession", selected = character(0))
        updateSelectizeInput(session, "w_location", choices = character(0), selected = character(0))
        updateNumericInput(session, "w_amount", value = 10)
        updateTextInput(session, "w_reason", value = "")

        # Increment the trigger to refresh the data table and dropdowns.
        refresh_trigger(refresh_trigger() + 1)

      }, error = function(e) {
        shinyWidgets::show_alert(
          title = "Withdrawal Error",
          text = e$message,
          type = "error"
        )
      })
    })
    
    # Render the available stock text based on selections
    output$available_stock_text <- renderText({
      inv_data <- selected_accession_inventory()
      location <- input$w_location
      
      req(nrow(inv_data) > 0, location != "")
      
      stock_info <- inv_data[inv_data$storage_location == location, ]
      
      if (nrow(stock_info) == 1) {
        paste("Available at this location:", stock_info$quantity, stock_info$unit)
      } else {
        "Select a location to see available stock."
      }
    })
    
  })
}
