#' Data Entry Module UI
#' @param id Module id
#' @import shiny
#' @importFrom shinyWidgets show_alert
#' @noRd
mod_data_entry_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Container styling for modern layout
    style = "background-color: #F8FAFC; padding: 20px;",

    div(
      class = "mb-4 text-center",
      h2(
        class = "fw-bold",
        style = "color: #0F766E; font-family: 'Outfit', sans-serif;",
        "Master Registry"
      ),
      p(
        class = "text-muted",
        "Populate the database sequentially. Changes are immediately saved to the active SQLite connection."
      )
    ),

    div(
      style = "max-width: 800px; margin: 0 auto;",
      bslib::card(
        class = "shadow-sm border-0 mb-3",
        bslib::card_body(
          textInput(
            ns("user_name"),
            tagList(bsicons::bs_icon("person-badge-fill"), " Breeder's Name:"),
            placeholder = "e.g., Israel Tetteh",
            value = "Israel Tetteh"
          )
        )
      ),
      bslib::accordion(
        id = ns("data_entry_accordion"),
        open = "germplasm",

        # ========================================================
        # TAB 1: GERMPLASM
        # ========================================================
        bslib::accordion_panel(
          title = tagList("1. Germplasm", bsicons::bs_icon("tree-fill")),
          value = "germplasm",
              h5(
                class = "fw-bold text-success mb-3",
                "Register New Accession"
              ),
              p(class = "text-muted small mb-4", "Add a new seed line or variety to the passport database."),
              textInput(
                ns("g_name"),
                "Accession Name (Unique)",
                placeholder = "e.g., SC-2026-001",
                width = '100%'
              ),
              textInput(
                ns("g_pedigree"),
                "Pedigree / Cross",
                placeholder = "e.g., Local-Landrace-A",
                width = '100%'
              ),
              textInput(
                ns("g_species"),
                "Species",
                placeholder = "e.g., Sorghum bicolor",
                width = '100%'
              ),
              hr(),
              actionButton(
                ns("btn_add_germplasm"),
                "Register Germplasm",
                class = "btn btn-success rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 2: INVENTORY
        # ========================================================
        bslib::accordion_panel(
          title = tagList("2. Inventory", bsicons::bs_icon("box-seam")),
          value = "inventory",
              h5(class = "fw-bold text-info mb-3", "Inventory Deposit"),
              p(class = "text-muted small mb-4", "Add physical seeds to a storage location."),
              bslib::layout_column_wrap(
                width = 1 / 2,
                selectizeInput(
                  ns("inv_accession"),
                  "Select Accession",
                  choices = NULL
                ),
                numericInput(
                  ns("inv_amount"),
                  "Amount (grams)",
                  value = 100,
                  min = 1
                ),
                textInput(
                  ns("inv_location"),
                  "Storage Location",
                  placeholder = "e.g., Cold_Room_Shelf_A"
                ),
                textInput(
                  ns("inv_reason"),
                  "Reason for Deposit",
                  placeholder = "e.g., 2026 Harvest"
                )
              ),
              hr(),
              actionButton(
                ns("btn_add_deposit"),
                "Deposit to Inventory",
                class = "btn btn-info text-white rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 3: TRAITS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("3. Traits", bsicons::bs_icon("rulers")),
          value = "traits",
              h5(
                class = "fw-bold text-warning mb-3",
                style = "color: #D97706 !important;",
                "Define Trait Vocabulary"
              ),
              p(class = "text-muted small mb-4", "Define a standard phenotypic trait before recording it."),
              selectizeInput(
                ns("tr_name"),
                "Trait Name (Select or Create)",
                choices = NULL,
                options = list(
                  create = TRUE,
                  placeholder = "e.g., Awn_Length"
                )
              ),
              textInput(
                ns("tr_unit"),
                "Unit of Measurement",
                placeholder = "e.g., cm, kg/ha, Score(1-5)"
              ),
              hr(),
              actionButton(
                ns("btn_add_trait"),
                "Register Trait",
                class = "btn btn-warning text-dark rounded-pill fw-bold float-end px-4",
                style = "background-color: #F59E0B; border: none;"
          )
        ),

        # ========================================================
        # TAB 4: TRIALS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("4. Trials", bsicons::bs_icon("clipboard-check")),
          value = "trials",
              h5(
                class = "fw-bold text-primary mb-3",
                "Create Field Trial"
              ),
              p(class = "text-muted small mb-4", "Initialize an experimental field or greenhouse trial."),
              bslib::layout_column_wrap(
                width = 1 / 2,
                textInput(
                  ns("t_name"),
                  "Trial Name (Unique)",
                  placeholder = "e.g., 2026_Yield_Test"
                ),
                textInput(
                  ns("t_loc"),
                  "Trial Location",
                  placeholder = "e.g., KNUST_Agric_Field"
                ),
                dateInput(
                  ns("t_date"),
                  "Start Date",
                  format = "yyyy-mm-dd"
                )
              ),
              tags$label(
                "Trial Metadata",
                class = "control-label mt-3 fw-bold"
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                textInput(
                  ns("t_meta_design"),
                  "Experimental Design",
                  placeholder = "e.g., Alpha Lattice"
                ),
                textInput(
                  ns("t_meta_dim"),
                  "Plot Dimensions",
                  placeholder = "e.g., 3m x 0.25m"
                ),
                textInput(
                  ns("t_meta_sup"),
                  "Supervisor",
                  placeholder = "e.g., Dr. Wireko Kena"
                )
              ),
              hr(),
              actionButton(
                ns("btn_add_trial"),
                "Initialize Trial",
                class = "btn btn-primary rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 5: PLOTS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("5. Plots", bsicons::bs_icon("grid-3x3-gap-fill")),
          value = "plots",
              h5(class = "fw-bold text-secondary mb-3", "Assign Plot"),
              p(
                class = "text-muted small mb-4",
                "Map a seed accession to a physical plot in a trial."
              ),
               bslib::layout_column_wrap(
                width = 1 / 2,
              selectizeInput(
                ns("p_trial"),
                "Select Trial",
                choices = NULL
              ),
              selectizeInput(
                ns("p_accession"),
                "Select Accession",
                choices = NULL
              )
            ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                numericInput(
                  ns("p_number"),
                  "Plot Number",
                  value = 101,
                  min = 1
                ),
                numericInput(
                  ns("p_block"),
                  "Block / Rep",
                  value = 1,
                  min = 1
                )
              ),
              hr(),
              actionButton(
                ns("btn_add_plot"),
                "Create Plot Record",
                class = "btn btn-secondary rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 6: OBSERVATIONS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("6. Data", bsicons::bs_icon("clipboard-data")),
          value = "observations",
              h5(
                class = "fw-bold text-danger mb-3",
                "Record Observation"
              ),
              p(class = "text-muted small mb-4", "Enter phenotypic data from a specific plot."),
              bslib::layout_column_wrap(
                width = 1 / 2,
                selectizeInput(
                  ns("obs_trial"),
                  "Target Trial",
                  choices = NULL
                ),
                numericInput(
                  ns("obs_plot"),
                  "Plot Number",
                  value = 101,
                  min = 1
                )
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                selectizeInput(
                  ns("obs_trait"),
                  "Target Trait",
                  choices = NULL
                ),
                numericInput(
                  ns("obs_value"),
                  "Measured Value",
                  value = 0,
                  step = 0.1
                )
              ),
              hr(),
              actionButton(
                ns("btn_add_obs"),
                "Submit Measurement",
                class = "btn btn-danger rounded-pill fw-bold float-end px-4"
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
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the full trait table (name + unit)
    all_traits_data <- reactiveVal(data.frame())
    
    # Reactive trigger to force dropdown updates across all components when DB changes
    db_update_trigger <- reactiveVal(0)
    
    # Observe changes in db_state$path or direct triggers to reload dropdowns
    observeEvent(list(db_state$path, db_update_trigger()), {
      path <- db_state$path
      req(path)
      
      if (file.exists(path)) {
        tryCatch({
          acc_list <- get_all_accessions(path)
          trial_list <- get_all_trials(path)

          # Fetch traits with units, store the data, and get the names for dropdowns
          traits_df <- get_all_traits_with_units(path)
          all_traits_data(traits_df)
          trait_list <- traits_df$trait_name
          
          updateSelectizeInput(session, "inv_accession", choices = acc_list, server = TRUE)
          updateSelectizeInput(session, "p_accession", choices = acc_list, server = TRUE)
          updateSelectizeInput(session, "p_trial", choices = trial_list, server = TRUE)
          updateSelectizeInput(session, "obs_trial", choices = trial_list, server = TRUE)
          updateSelectizeInput(session, "obs_trait", choices = trait_list, server = TRUE)
          updateSelectizeInput(session, "tr_name", choices = trait_list, server = TRUE)

        }, error = function(e) {
          # Silently ignore errors during initialization of an empty DB
        })
      }
    })

    # When a trait is selected or created, auto-fill its unit if it exists.
    observeEvent(input$tr_name, {
      selected_trait <- trimws(input$tr_name)
      traits_df <- all_traits_data()

      req(selected_trait != "")

      # Find the selected trait in our stored data
      trait_info <- traits_df[traits_df$trait_name == selected_trait, ]

      if (nrow(trait_info) == 1) {
        # It's an existing trait, so auto-fill the unit
        updateTextInput(session, "tr_unit", value = trait_info$unit)
      } else {
        # It's a new trait, so clear the unit field for the user to fill
        updateTextInput(session, "tr_unit", value = "")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Helper to execute DB operations safely with notifications
    run_db_op <- function(success_msg, operation_expr) {
      req(db_state$path)
      tryCatch({
        operation_expr
        shinyWidgets::show_alert(
          title = "Success",
          text = success_msg,
          type = "success"
        )
        # Trigger dropdown refresh globally
        db_update_trigger(db_update_trigger() + 1)
      }, error = function(e) {
        shinyWidgets::show_alert(
          title = "Error",
          text = e$message,
          type = "error"
        )
      })
    }
    
    # 1. Add Germplasm
    observeEvent(input$btn_add_germplasm, {
      req(input$g_name, input$g_species)
      run_db_op("Germplasm registered!", {
        add_germplasm(db_state$path, trimws(input$g_name), trimws(input$g_pedigree), trimws(input$g_species))
      })
    })
    
    # 2. Add Inventory Deposit
    observeEvent(input$btn_add_deposit, {
      req(input$inv_accession, input$inv_amount, input$inv_location, input$user_name)
      run_db_op("Inventory deposit successful!", {
        add_inventory_deposit(
          db_path = db_state$path,
          accession_name = input$inv_accession,
          amount_grams = input$inv_amount,
          storage_location = trimws(input$inv_location),
          user_name = trimws(input$user_name),
          reason = trimws(input$inv_reason)
        )
      })
    })
    
    # 3. Add Trial
    observeEvent(input$btn_add_trial, {
      req(input$t_name, input$t_loc)
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
    observeEvent(input$btn_add_plot, {
      req(input$p_trial, input$p_accession, input$p_number, input$p_block)
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
    observeEvent(input$btn_add_trait, {
      req(input$tr_name, input$tr_unit)
      run_db_op("Trait registered!", {
        add_trait(
          db_path = db_state$path,
          trait_name = trimws(input$tr_name),
          unit = trimws(input$tr_unit)
        )
      })
    })
    
    # 6. Add Observation
    observeEvent(input$btn_add_obs, {
      req(input$obs_trial, input$obs_plot, input$obs_trait, input$user_name)
      run_db_op("Observation submitted!", {
        add_observation(
          db_path = db_state$path,
          trial_name = input$obs_trial,
          plot_number = input$obs_plot,
          trait_name = input$obs_trait,
          value = input$obs_value,
          user_name = trimws(input$user_name)
        )
      })
    })
    
  })
}
