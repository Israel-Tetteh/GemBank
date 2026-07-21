#' Inventory Management Module UI
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
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Inventory Management"
      ),
      p(
        class = "text-muted",
        "Manage seed lots, register new inventory, perform withdrawals, and view history."
      )
    ),

    bslib::card(
      class = "shadow-sm border-0 mb-3",
      bslib::card_body(
        textInput(
          ns("user_name"),
          tagList(
            bsicons::bs_icon("person-badge-fill"),
            " Breeder's Name (for logging):"
          ),
          value = "Israel Tetteh"
        )
      )
    ),

    bslib::navset_card_pill(
      
      # TAB 2: REGISTER SEED LOT
      bslib::nav_panel(
        "Register Seed Lot",
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(tagList(
            bsicons::bs_icon("box-seam"),
            " Register New Seed Lot"
          )),
          bslib::card_body(
            p(
              class = "text-muted small mb-4",
              "Add a new physical seed lot to a storage location. The breeder's name above will be used for logging."
            ),
            h6("Core Information", class = "text-primary fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 3,
              selectizeInput(
                ns("reg_accession"),
                "Select Accession (Required)",
                choices = NULL
              ),
              textInput(
                ns("reg_location"),
                "Storage Location (Required)",
                placeholder = "e.g., Cold Room A"
              ),
              textInput(
                ns("reg_container"),
                "Container",
                placeholder = "e.g., Bottle 2"
              )
            ),
            h6("Quantity & Condition", class = "text-primary fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 3,
              numericInput(
                ns("reg_quantity"),
                "Quantity (Required)",
                value = 100,
                min = 0.1
              ),
              selectInput(
                ns("reg_unit"),
                "Unit",
                choices = c("g", "kg", "Seeds", "Packets"),
                selected = "g"
              ),
              selectInput(
                ns("reg_seed_status"),
                "Seed Status",
                choices = c("Available", "Reserved", "Testing", "Regeneration"),
                selected = "Available"
              )
            ),
            h6("Provenance (Source)", class = "text-primary fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 3,
              selectInput(
                ns("reg_source_type"),
                "Source Type",
                choices = c(
                  "",
                  "Harvest",
                  "Research Institution",
                  "Gene Bank",
                  "Farmer",
                  "Purchase",
                  "Donation",
                  "Exchange",
                  "Regeneration",
                  "Unknown"
                )
              ),
              textInput(
                ns("reg_source_reference"),
                "Source Reference",
                placeholder = "e.g., 2026 Bird Trial, ICRISAT"
              ),
              textInput(
                ns("reg_deposit_reason"),
                "Reason for Deposit",
                placeholder = "e.g., Post Harvest, Backup"
              )
            ),
            h6("Quality & Storage", class = "text-primary fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 3,
              numericInput(
                ns("reg_viability"),
                "Viability (%)",
                value = NA,
                min = 0,
                max = 100
              ),
              numericInput(
                ns("reg_moisture"),
                "Moisture (%)",
                value = NA,
                min = 0,
                max = 100
              ),
              textInput(
                ns("reg_storage_condition"),
                "Storage Condition",
                placeholder = "e.g., 4°C, -20°C"
              )
            ),
            h6("Dates & Remarks", class = "text-primary fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 2,
              dateInput(ns("reg_deposit_date"), "Deposit Date"),
              textAreaInput(
                ns("reg_remarks"),
                "Remarks",
                placeholder = "Any other notes about this lot...",
                width = "100%",
                rows = 2
              )
            ),
            hr(),
            actionButton(
              ns("btn_register_lot"),
              "Register Seed Lot",
              class = "btn btn-success rounded-pill fw-bold float-end px-4"
            )
          )
        )
      ),

      # TAB 1: SEED INVENTORY DASHBOARD
      bslib::nav_panel(
        "Seed Inventory",
        # Summary Value Boxes
        uiOutput(ns("summary_cards_ui")),

        # Filters
        bslib::card(
          class = "shadow-sm border-0 mb-3",
          bslib::card_header("Filters"),
          bslib::card_body(
            bslib::layout_column_wrap(
              width = 1 / 4,
              selectizeInput(
                ns("filter_species"),
                "Crop Species",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                ns("filter_location"),
                "Storage Location",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                ns("filter_status"),
                "Seed Status",
                choices = NULL,
                multiple = TRUE
              ),
              numericInput(
                ns("filter_low_stock_threshold"),
                "Filter by Max Quantity (g)",
                value = NA,
                min = 0
              )
            )
          )
        ),

        # Main Inventory Table
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(tagList(
            bsicons::bs_icon("table"),
            " Live Vault Balances"
          )),
          bslib::card_body(DT::DTOutput(ns("inventory_table")))
        )
      ),

      # TAB 3: WITHDRAW SEED
      bslib::nav_panel(
        "Withdraw Seed",
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(
            class = "text-danger",
            tagList(bsicons::bs_icon("box-arrow-up-right"), " Withdraw Seed")
          ),
          bslib::card_body(
            p(
              class = "text-muted small mb-4",
              "Remove seeds from the cold room for planting, sharing, or testing. The breeder's name above will be used for logging."
            ),
            h6("Step 1: Select Accession", class = "fw-bold"),
            selectizeInput(
              ns("w_accession"),
              NULL,
              choices = NULL,
              width = "100%"
            ),

            h6("Step 2: Select Seed Lot", class = "fw-bold mt-4"),
            selectizeInput(
              ns("w_inventory_id"),
              NULL,
              choices = NULL,
              width = "100%"
            ),
            uiOutput(ns("w_lot_details_ui")),

            h6("Step 3: Specify Withdrawal Details", class = "fw-bold mt-4"),
            bslib::layout_column_wrap(
              width = 1 / 2,
              numericInput(
                ns("w_amount"),
                "Amount to Withdraw",
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
            selectizeInput(
              ns("w_reason"),
              "Purpose of Withdrawal (Select or Create)",
              choices = c(
                "",
                "Planting Trial",
                "Seed Multiplication",
                "DNA Extraction",
                "Germination Test",
                "Distribution",
                "Student Research",
                "Other"
              ),
              options = list(create = TRUE),
              width = "100%"
            ),
            textAreaInput(
              ns("w_remarks"),
              "Remarks (Optional)",
              placeholder = "e.g., For 2027 PYT",
              width = "100%"
            ),

            uiOutput(ns("w_summary_ui")),

            hr(),
            actionButton(
              ns("btn_withdraw"),
              "Execute Withdrawal",
              class = "btn btn-danger rounded-pill fw-bold w-100"
            )
          )
        )
      ),

      # TAB 4: INVENTORY HISTORY
      bslib::nav_panel(
        "Inventory History",
        bslib::card(
          class = "shadow-sm border-0",
          bslib::card_header(tagList(
            bsicons::bs_icon("clock-history"),
            " Lot Movement History"
          )),
          bslib::card_body(
            p(
              class = "text-muted small mb-4",
              "Track all deposits, withdrawals, and adjustments for every seed lot."
            ),
            DT::DTOutput(ns("history_table"))
          )
        )
      )
    )
  )
}

#' Inventory Management Module Server
#' @param id Module id
#' @param db_state A `reactiveValues` object from `app_server` holding the database path.
#' @param global_refresh_trigger A reactiveVal to signal data changes.
#' @noRd
mod_transaction_log_server <- function(id, db_state, global_refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    full_inventory_data <- reactiveVal(data.frame())

    # OBSERVE: Main data fetcher, depends on DB path and refresh trigger
    observeEvent(list(db_state$path, global_refresh_trigger()), {
      path <- db_state$path
      req(path)
      tryCatch({
        full_inventory_data(get_inventory_status(path))
        
        # Update filter choices
        inv_data <- full_inventory_data()
        updateSelectizeInput(session, "filter_species", choices = unique(inv_data$species), server = TRUE)
        updateSelectizeInput(session, "filter_location", choices = unique(inv_data$storage_location), server = TRUE)
        updateSelectizeInput(session, "filter_status", choices = unique(inv_data$seed_status), server = TRUE)
        
        # Update accession dropdowns
        acc_choices <- unique(inv_data$accession_name)
        updateSelectizeInput(session, "reg_accession", choices = acc_choices, selected = character(0), server = TRUE)
        updateSelectizeInput(session, "w_accession", choices = acc_choices, selected = character(0), server = TRUE)

      }, error = function(e) {
        full_inventory_data(data.frame())
      })
    })

    # REACTIVE: Filtered inventory data for the main dashboard table
    filtered_data <- reactive({
      df <- full_inventory_data()
      req(df)
      
      if (!is.null(input$filter_species) && length(input$filter_species) > 0) {
        df <- df[df$species %in% input$filter_species, ]
      }
      if (!is.null(input$filter_location) && length(input$filter_location) > 0) {
        df <- df[df$storage_location %in% input$filter_location, ]
      }
      if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
        df <- df[df$seed_status %in% input$filter_status, ]
      }
      # Apply quantity filter only if a value is provided
      if (!is.na(input$filter_low_stock_threshold)) {
        df <- df[!is.na(df$quantity) & df$quantity < input$filter_low_stock_threshold & df$unit == 'g', ]
      }
      df
    })

    # REACTIVE: Summary statistics for the value boxes
    summary_stats <- reactive({
      req(db_state$path, global_refresh_trigger())
      get_inventory_summary_stats(db_state$path)
    })

    # RENDER: Summary cards UI
    output$summary_cards_ui <- renderUI({
      stats <- summary_stats()
      req(stats)
      
      bslib::layout_columns(
        bslib::value_box("Total Accessions", stats$total_accessions, showcase = bs_icon("tree"), theme="success"),
        bslib::value_box("Total Seed Lots", stats$total_lots, showcase = bs_icon("collection"), theme="info"),
        bslib::value_box("Low Stock Lots", stats$low_stock_lots, showcase = bs_icon("exclamation-triangle"), theme="warning"),
        bslib::value_box("Empty Lots", stats$empty_lots, showcase = bs_icon("trash"), theme="danger")
      )
    })

    # RENDER: Main inventory table
    output$inventory_table <- DT::renderDT({
      df <- filtered_data()
      tryCatch({
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
            'quantity', 'seed_status',
            backgroundColor = DT::styleEqual(c('Exhausted', 'Archived'), c('#FECACA', '#E5E7EB')),
            color = DT::styleInterval(c(0.1, 50), c('red', 'orange', 'black')),
            fontWeight = DT::styleInterval(50, c('bold', 'normal'))
          )
      }, error = function(e) {
        data.frame(Message = "No inventory data found or error loading table.")
      })
    })
    
    # --- REGISTER SEED LOT TAB ---
    observeEvent(input$btn_register_lot, {
      # Validation
      if (is.null(input$reg_accession) || input$reg_accession == "") {
        shinyWidgets::show_alert("Error", "An accession must be selected.", type = "error"); return()
      }
      if (trimws(input$reg_location) == "") {
        shinyWidgets::show_alert("Error", "Storage location is required.", type = "error"); return()
      }
      if (input$reg_quantity <= 0) {
        shinyWidgets::show_alert("Error", "Quantity must be greater than zero.", type = "error"); return()
      }
      if (input$reg_deposit_date > Sys.Date()) {
        shinyWidgets::show_alert("Error", "Deposit date cannot be in the future.", type = "error"); return()
      }

      tryCatch({
        add_inventory_deposit(
          db_path = db_state$path,
          accession_name = input$reg_accession,
          quantity = input$reg_quantity,
          unit = input$reg_unit,
          storage_location = trimws(input$reg_location),
          user_name = trimws(input$user_name),
          container = input$reg_container,
          source_type = input$reg_source_type,
          source_reference = input$reg_source_reference,
          deposit_reason = input$reg_deposit_reason,
          seed_status = input$reg_seed_status,
          viability_percent = input$reg_viability,
          moisture_percent = input$reg_moisture,
          storage_condition = input$reg_storage_condition,
          deposit_date = input$reg_deposit_date,
          remarks = input$reg_remarks
        )
        shinyWidgets::show_alert("Success", "New seed lot registered successfully!", type = "success")
        global_refresh_trigger(global_refresh_trigger() + 1)
      }, error = function(e) {
        shinyWidgets::show_alert("Error", e$message, type = "error")
      })
    })

    # --- WITHDRAW SEED TAB ---
    w_selected_lot <- reactiveVal(NULL)

    observeEvent(input$w_accession, {
      accession <- input$w_accession
      req(accession, accession != "")
      
      acc_inventory <- full_inventory_data()
      acc_inventory <- acc_inventory[acc_inventory$accession_name == accession & acc_inventory$quantity > 0, ]
      
      if (nrow(acc_inventory) > 0) {
        lot_choices <- setNames(
          acc_inventory$inventory_id,
          sprintf("Lot #%s: %s (%s) - %.2f %s", acc_inventory$inventory_id, acc_inventory$storage_location, acc_inventory$container, acc_inventory$quantity, acc_inventory$unit)
        )
        updateSelectizeInput(session, "w_inventory_id", choices = lot_choices, selected = character(0), server = TRUE)
      } else {
        updateSelectizeInput(session, "w_inventory_id", choices = c("No available lots for this accession" = ""), selected = "", server = TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$w_inventory_id, {
      # If input is empty or NULL, clear the selection and stop.
      if (is.null(input$w_inventory_id) || input$w_inventory_id == "") {
        w_selected_lot(NULL)
        return()
      }
      
      lot_id <- as.integer(input$w_inventory_id)
      
      # Ensure we only proceed with a valid, non-NA lot_id
      if (is.na(lot_id)) {
        w_selected_lot(NULL)
        return()
      }
      
      lot_data <- full_inventory_data()[which(full_inventory_data()$inventory_id == lot_id), ]
      
      # Only set the reactive value if exactly one row is found
      if (nrow(lot_data) == 1) {
        w_selected_lot(lot_data)
      } else {
        w_selected_lot(NULL)
      }
    })

    output$w_lot_details_ui <- renderUI({
      lot <- w_selected_lot()
      req(lot)
      div(class="alert alert-info p-2 mt-2",
          p(strong("Current Qty: "), sprintf("%.2f %s", lot$quantity, lot$unit)),
          p(strong("Location: "), sprintf("%s (%s)", lot$storage_location, lot$container))
      )
    })

    output$w_summary_ui <- renderUI({
      lot <- w_selected_lot(); req(lot)
      withdraw_qty <- input$w_amount; req(withdraw_qty)
      remaining <- lot$quantity - withdraw_qty
      
      div(class="alert alert-warning p-2 mt-3",
          p(strong("Current: "), sprintf("%.2f %s", lot$quantity, lot$unit)),
          p(strong("Withdraw: "), sprintf("%.2f %s", withdraw_qty, input$w_unit)),
          p(strong("Remaining: "), sprintf("%.2f %s", remaining, lot$unit))
      )
    })

    observeEvent(input$btn_withdraw, {
      req(input$w_inventory_id, input$w_amount, input$user_name, input$w_reason != "")
      path <- db_state$path
      req(path)
      
      showModal(modalDialog(
        title = tagList(bsicons::bs_icon("exclamation-triangle-fill", class = "text-warning"), " Confirm Withdrawal"),
        p(
          "Please confirm you want to withdraw",
          strong(paste(input$w_amount, input$w_unit)), "from seed lot",
          strong(paste0("#", input$w_inventory_id)), "."
        ),
        p("This action will be recorded in the transaction log and cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_confirm_withdraw"), "Confirm Withdrawal", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$btn_confirm_withdraw, {
      removeModal()
      
      tryCatch({
        withdraw_seed(
          db_path = db_state$path,
          inventory_id = input$w_inventory_id,
          withdraw_amount = input$w_amount,
          withdraw_unit = input$w_unit,
          user_name = trimws(input$user_name),
          reason = trimws(input$w_reason) # In new design, this is 'purpose'
        )

        shinyWidgets::show_alert(
          title = "Success",
          text = "Seed withdrawn successfully!",
          type = "success"
        )

        # Reset form inputs for the next transaction.
        updateNumericInput(session, "w_amount", value = 10)
        updateSelectizeInput(session, "w_reason", selected = "")

        global_refresh_trigger(global_refresh_trigger() + 1)

      }, error = function(e) {
        shinyWidgets::show_alert(
          title = "Withdrawal Error",
          text = e$message,
          type = "error"
        )
      })
    })

    # --- INVENTORY HISTORY TAB ---
    output$history_table <- renderDT({
      req(db_state$path)
      global_refresh_trigger() # Depend on the global trigger
      
      history_data <- get_inventory_history(db_state$path)
      DT::datatable(
        history_data,
        extensions = 'Buttons',
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
  })
}
