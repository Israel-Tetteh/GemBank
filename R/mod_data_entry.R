#' Data Entry Module UI
#' @param id Module id
#' @import shiny bslib bsicons
#' @noRd
mod_data_entry_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    # Container styling for modern layout
    style = "background-color: #F8FAFC; padding: 20px;",

    shiny::div(
      class = "mb-4 text-center",
      shiny::h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Master Registry"
      ),
      shiny::p(
        class = "text-muted",
        "Populate the database sequentially. Changes are immediately saved to the active SQLite connection."
      )
    ),

    shiny::div(
      style = "max-width: 800px; margin: 0 auto;",
      bslib::navset_card_underline(
        id = ns("data_entry_tabs"),

        # ========================================================
        # TAB 1: GERMPLASM
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList("1. Germplasm", bsicons::bs_icon("tree-fill")),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(
                class = "fw-bold text-success mb-3",
                "Register New Accession"
              ),
              shiny::p(
                class = "text-muted small mb-4",
                "Add a new seed line or variety to the passport database."
              ),
              shiny::textInput(
                ns("g_name"),
                "Accession Name (Unique)",
                placeholder = "e.g., SC-2026-001",
                width = '100%'
              ),
              shiny::textInput(
                ns("g_pedigree"),
                "Pedigree / Cross",
                placeholder = "e.g., Local-Landrace-A",
                width = '100%'
              ),
              shiny::textInput(
                ns("g_species"),
                "Species",
                placeholder = "e.g., Sorghum bicolor",
                width = '100%'
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_germplasm"),
                "Register Germplasm",
                class = "btn btn-success rounded-pill fw-bold float-end px-4"
              )
            )
          )
        ),

        # ========================================================
        # TAB 2: INVENTORY
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList("2. Inventory", bsicons::bs_icon("box-seam")),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(class = "fw-bold text-info mb-3", "Inventory Deposit"),
              shiny::p(
                class = "text-muted small mb-4",
                "Add physical seeds to your storage locations."
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::selectizeInput(
                  ns("inv_accession"),
                  "Select Accession",
                  choices = NULL
                ),
                shiny::numericInput(
                  ns("inv_amount"),
                  "Amount (grams)",
                  value = 100,
                  min = 1
                ),
                shiny::textInput(
                  ns("inv_location"),
                  "Storage Location",
                  placeholder = "e.g., Cold_Room_Shelf_A"
                ),
                shiny::textInput(
                  ns("inv_reason"),
                  "Reason for Deposit",
                  placeholder = "e.g., 2026 Harvest"
                )
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_deposit"),
                "Deposit to Inventory",
                class = "btn btn-info text-white rounded-pill fw-bold float-end px-4"
              )
            )
          )
        ),

        # ========================================================
        # TAB 3: TRAITS
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList("3. Traits", bsicons::bs_icon("tag-fill")),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(
                class = "fw-bold text-warning mb-3",
                style = "color: #D97706 !important;",
                "Define Trait Vocabulary"
              ),
              shiny::p(
                class = "text-muted small mb-4",
                "Define a standard phenotypic trait before recording it in the field."
              ),
              shiny::textInput(
                ns("tr_name"),
                "Trait Name (Unique)",
                placeholder = "e.g., Awn_Length"
              ),
              shiny::textInput(
                ns("tr_unit"),
                "Unit of Measurement",
                placeholder = "e.g., cm, kg/ha, Score(1-5)"
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_trait"),
                "Register Trait",
                class = "btn btn-warning text-white rounded-pill fw-bold float-end px-4",
                style = "background-color: #F59E0B; border: none;"
              )
            )
          )
        ),

        # ========================================================
        # TAB 4: TRIALS
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList("4. Trials", bsicons::bs_icon("map")),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(
                class = "fw-bold text-primary mb-3",
                "Create Field Trial"
              ),
              shiny::p(
                class = "text-muted small mb-4",
                "Initialize an experimental field or greenhouse trial environment."
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::textInput(
                  ns("t_name"),
                  "Trial Name (Unique)",
                  placeholder = "e.g., 2026_Yield_Test"
                ),
                shiny::textInput(
                  ns("t_loc"),
                  "Trial Location",
                  placeholder = "e.g., KNUST_Agric_Field"
                ),
                shiny::dateInput(
                  ns("t_date"),
                  "Start Date",
                  format = "yyyy-mm-dd"
                )
              ),
              shiny::tags$label(
                "Trial Metadata",
                class = "control-label mt-3 fw-bold"
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::textInput(
                  ns("t_meta_design"),
                  "Experimental Design",
                  placeholder = "e.g., Alpha Lattice"
                ),
                shiny::textInput(
                  ns("t_meta_dim"),
                  "Plot Dimensions",
                  placeholder = "e.g., 3m x 0.25m"
                ),
                shiny::textInput(
                  ns("t_meta_sup"),
                  "Supervisor",
                  placeholder = "e.g., Dr. Wireko Kena"
                )
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_trial"),
                "Initialize Trial",
                class = "btn btn-primary rounded-pill fw-bold float-end px-4"
              )
            )
          )
        ),

        # ========================================================
        # TAB 5: PLOTS
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList("5. Plots", bsicons::bs_icon("grid-3x3")),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(class = "fw-bold text-secondary mb-3", "Assign Plot"),
              shiny::p(
                class = "text-muted small mb-4",
                "Map a specific seed accession to a physical field plot in a trial."
              ),
              shiny::selectizeInput(
                ns("p_trial"),
                "Select Trial",
                choices = NULL
              ),
              shiny::selectizeInput(
                ns("p_accession"),
                "Select Accession",
                choices = NULL
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::numericInput(
                  ns("p_number"),
                  "Plot Number",
                  value = 101,
                  min = 1
                ),
                shiny::numericInput(
                  ns("p_block"),
                  "Block / Rep",
                  value = 1,
                  min = 1
                )
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_plot"),
                "Create Plot Record",
                class = "btn btn-secondary rounded-pill fw-bold float-end px-4"
              )
            )
          )
        ),

        # ========================================================
        # TAB 6: OBSERVATIONS
        # ========================================================
        bslib::nav_panel(
          title = shiny::tagList(
            "6. Observations",
            bsicons::bs_icon("clipboard-data")
          ),
          bslib::card(
            class = "shadow-sm border-0 mt-3",
            bslib::card_body(
              class = "p-4",
              shiny::h5(
                class = "fw-bold text-danger mb-3",
                "Record Observation"
              ),
              shiny::p(
                class = "text-muted small mb-4",
                "Enter phenotypic data measured from a specific plot."
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::selectizeInput(
                  ns("obs_trial"),
                  "Target Trial",
                  choices = NULL
                ),
                shiny::numericInput(
                  ns("obs_plot"),
                  "Plot Number",
                  value = 101,
                  min = 1
                )
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::selectizeInput(
                  ns("obs_trait"),
                  "Target Trait",
                  choices = NULL
                ),
                shiny::numericInput(
                  ns("obs_value"),
                  "Measured Value",
                  value = 0,
                  step = 0.1
                )
              ),
              shiny::hr(),
              shiny::actionButton(
                ns("btn_add_obs"),
                "Submit Measurement",
                class = "btn btn-danger rounded-pill fw-bold float-end px-4"
              )
            )
          )
        )
      )
    )
  )
}

#' Data Entry Module Server
#' @param id Module id
#' @param db_state Reactive variable holding the current DB path
#' @noRd
mod_data_entry_server <- function(id, db_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive trigger to force dropdown updates across all components when DB changes
    db_update_trigger <- shiny::reactiveVal(0)
    
    # Hardcoded current user for the audit log (could be made dynamic later)
    current_user <- "Local Operator"
    
    # Observe changes in db_state$path or direct triggers to reload dropdowns
    shiny::observeEvent(list(db_state$path, db_update_trigger()), {
      path <- db_state$path
      shiny::req(path)
      
      if (file.exists(path)) {
        tryCatch({
          acc_list <- get_all_accessions(path)
          trial_list <- get_all_trials(path)
          trait_list <- get_all_traits(path)
          
          shiny::updateSelectizeInput(session, "inv_accession", choices = acc_list, server = TRUE)
          shiny::updateSelectizeInput(session, "p_accession", choices = acc_list, server = TRUE)
          shiny::updateSelectizeInput(session, "p_trial", choices = trial_list, server = TRUE)
          shiny::updateSelectizeInput(session, "obs_trial", choices = trial_list, server = TRUE)
          shiny::updateSelectizeInput(session, "obs_trait", choices = trait_list, server = TRUE)
        }, error = function(e) {
          # Silently ignore errors during initialization of an empty DB
        })
      }
    })
    
    # Helper to execute DB operations safely with notifications
    run_db_op <- function(success_msg, operation_expr) {
      shiny::req(db_state$path)
      tryCatch({
        operation_expr
        shiny::showNotification(success_msg, type = "message")
        # Trigger dropdown refresh globally
        db_update_trigger(db_update_trigger() + 1)
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    }
    
    # 1. Add Germplasm
    shiny::observeEvent(input$btn_add_germplasm, {
      shiny::req(input$g_name, input$g_species)
      run_db_op("Germplasm registered!", {
        add_germplasm(db_state$path, trimws(input$g_name), trimws(input$g_pedigree), trimws(input$g_species))
      })
    })
    
    # 2. Add Inventory Deposit
    shiny::observeEvent(input$btn_add_deposit, {
      shiny::req(input$inv_accession, input$inv_amount, input$inv_location)
      run_db_op("Inventory deposit successful!", {
        add_inventory_deposit(
          db_path = db_state$path,
          accession_name = input$inv_accession,
          amount_grams = input$inv_amount,
          storage_location = trimws(input$inv_location),
          user_name = current_user,
          reason = trimws(input$inv_reason)
        )
      })
    })
    
    # 3. Add Trial
    shiny::observeEvent(input$btn_add_trial, {
      shiny::req(input$t_name, input$t_loc)
      run_db_op("Trial initialized!", {
        # Construct metadata list based on user text inputs
        meta <- list()
        if (trimws(input$t_meta_design) != "") meta$experimental_design <- trimws(input$t_meta_design)
        if (trimws(input$t_meta_dim) != "") meta$plot_dimensions <- trimws(input$t_meta_dim)
        if (trimws(input$t_meta_sup) != "") meta$supervisor <- trimws(input$t_meta_sup)
        
        add_trial(
          db_path = db_state$path,
          trial_name = trimws(input$t_name),
          trial_loc = trimws(input$t_loc),
          start_date = as.character(input$t_date),
          metadata_list = meta
        )
      })
    })
    
    # 4. Add Plot
    shiny::observeEvent(input$btn_add_plot, {
      shiny::req(input$p_trial, input$p_accession, input$p_number, input$p_block)
      run_db_op("Plot record created!", {
        add_plot(
          db_path = db_state$path,
          trial_name = input$p_trial,
          accession_name = input$p_accession,
          plot_number = input$p_number,
          block = input$p_block
        )
      })
    })
    
    # 5. Add Trait
    shiny::observeEvent(input$btn_add_trait, {
      shiny::req(input$tr_name, input$tr_unit)
      run_db_op("Trait registered!", {
        add_trait(
          db_path = db_state$path,
          trait_name = trimws(input$tr_name),
          unit = trimws(input$tr_unit)
        )
      })
    })
    
    # 6. Add Observation
    shiny::observeEvent(input$btn_add_obs, {
      shiny::req(input$obs_trial, input$obs_plot, input$obs_trait)
      run_db_op("Observation submitted!", {
        add_observation(
          db_path = db_state$path,
          trial_name = input$obs_trial,
          plot_number = input$obs_plot,
          trait_name = input$obs_trait,
          value = input$obs_value,
          user_name = current_user
        )
      })
    })
    
  })
}
