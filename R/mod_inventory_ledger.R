#' Inventory Ledger Module UI
#' @param id Module id
#' @import shiny bslib bsicons DT
#' @noRd
mod_inventory_ledger_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    style = "background-color: #F8FAFC; padding: 20px;",
    
    shiny::div(
      class = "mb-4 text-center",
      shiny::h2(class = "fw-bold", style = "color: #0F766E; font-family: 'Outfit', sans-serif;", "Inventory Dashboard"),
      shiny::p(class = "text-muted", "Monitor real-time seed balances and execute inventory withdrawals.")
    ),
    
    bslib::layout_columns(
      col_widths = c(8, 4),
      
      # LEFT: Live Inventory Table
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header(
          class = "bg-white fw-bold",
          shiny::tagList(bsicons::bs_icon("table"), " Live Vault Balances")
        ),
        bslib::card_body(
          DT::dataTableOutput(ns("inventory_table"))
        )
      ),
      
      # RIGHT: Withdrawal Form
      bslib::card(
        class = "shadow-sm border-0",
        bslib::card_header(
          class = "bg-white fw-bold text-danger",
          shiny::tagList(bsicons::bs_icon("box-arrow-up-right"), " Withdraw Seed")
        ),
        bslib::card_body(
          class = "p-4",
          shiny::p(class = "text-muted small mb-4", "Remove seeds from the cold room for planting, sharing, or testing."),
          
          shiny::selectizeInput(
            ns("w_accession"),
            "Target Accession",
            choices = NULL,
            width = "100%"
          ),
          
          bslib::layout_column_wrap(
            width = 1/2,
            shiny::numericInput(
              ns("w_amount"),
              "Amount",
              value = 10,
              min = 0.1,
              width = "100%"
            ),
            shiny::selectInput(
              ns("w_unit"),
              "Unit",
              choices = c("grams", "kg", "seeds"),
              selected = "grams",
              width = "100%"
            )
          ),
          
          shiny::textInput(
            ns("w_operator"),
            "Operator Name",
            placeholder = "e.g., Dr. Kena",
            width = "100%"
          ),
          
          shiny::textInput(
            ns("w_reason"),
            "Reason for Withdrawal",
            placeholder = "e.g., Planting 2026 Trial",
            width = "100%"
          ),
          
          shiny::hr(),
          
          shiny::actionButton(
            ns("btn_withdraw"),
            "Execute Withdrawal",
            class = "btn btn-danger rounded-pill fw-bold w-100"
          )
        )
      )
    )
  )
}

#' Inventory Ledger Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @noRd
mod_inventory_ledger_server <- function(id, db_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactive trigger to manually refresh data after a database write (e.g., withdrawal).
    refresh_trigger <- shiny::reactiveVal(0)
    
    # Render the main data table showing current inventory levels.
    output$inventory_table <- DT::renderDataTable({
      path <- db_state$path
      shiny::req(path)
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
            'total_balance',
            color = DT::styleInterval(50, c('red', 'black')),
            fontWeight = DT::styleInterval(50, c('bold', 'normal'))
          )
      }, error = function(e) {
        data.frame(Message = "No inventory data found or error loading table.")
      })
    })
    
    # Observer to update accession choices when the database path or data changes.
    shiny::observeEvent(list(db_state$path, refresh_trigger()), {
      path <- db_state$path
      shiny::req(path)
      
      tryCatch({
        df <- get_inventory_status(path)
        # Populate the dropdown with accessions currently in the inventory.
        if(nrow(df) > 0) {
          acc_choices <- unique(df$accession_name)
          shiny::updateSelectizeInput(session, "w_accession", choices = acc_choices, server = TRUE)
        }
      }, error = function(e) {
        # Fail silently if the database is empty or an error occurs during the fetch.
      })
    })
    
    # Observer to handle the 'Execute Withdrawal' button click.
    shiny::observeEvent(input$btn_withdraw, {
      shiny::req(input$w_accession, input$w_amount, input$w_operator, input$w_reason)
      path <- db_state$path
      shiny::req(path)
      
      tryCatch({
        withdraw_seed(
          db_path = path,
          accession_name = input$w_accession,
          withdraw_amount = input$w_amount,
          withdraw_unit = input$w_unit,
          user_name = trimws(input$w_operator),
          reason = trimws(input$w_reason)
        )
        
        shiny::showNotification("Seed withdrawn successfully!", type = "message")
        
        # Reset form inputs for the next transaction.
        shiny::updateNumericInput(session, "w_amount", value = 10)
        shiny::updateTextInput(session, "w_reason", value = "")
        
        # Increment the trigger to refresh the data table and dropdowns.
        refresh_trigger(refresh_trigger() + 1)
        
      }, error = function(e) {
        shiny::showNotification(paste("Withdrawal Error:", e$message), type = "error", duration = 8)
      })
    })
  })
}
