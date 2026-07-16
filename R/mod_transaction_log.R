#' Transaction Log Module UI
#' @param id Module id
#' @import shiny
#' @importFrom bslib card card_body card_header layout_column_wrap layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom DT renderDT 
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
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
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
        df <- get_inventory_status(path)
        if (nrow(df) > 0) {
          # Only show accessions that have been physically deposited.
          acc_choices <- unique(df$accession_name[df$storage_location != "Not Deposited"])
          updateSelectizeInput(session, "w_accession", choices = acc_choices, server = TRUE)
        }
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
        # Fetch inventory specifically for the selected accession.
        acc_inventory <- get_inventory_status(
          db_path = path,
          target_accession = accession
        )
        
        # Filter for actual, physical storage locations.
        valid_locations <- acc_inventory$storage_location[acc_inventory$storage_location != "Not Deposited"]
        
        # Update the location dropdown with valid choices.
        updateSelectizeInput(session, "w_location", choices = valid_locations, server = TRUE)
        
      }, error = function(e) {
        updateSelectizeInput(session, "w_location", choices = character(0), server = TRUE)
      })
    }, ignoreInit = TRUE) # ignoreInit prevents running on startup before w_accession is set.
    
    # Observer to handle the 'Execute Withdrawal' button click.
    observeEvent(input$btn_withdraw, {
      req(input$w_accession, input$w_location, input$w_amount, input$w_operator, input$w_reason)
      path <- db_state$path
      req(path)
      
      tryCatch({
        withdraw_seed(
          db_path = path,
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
  })
}
