#' Data Entry Module UI
#' @param id Module id
#' @importFrom bslib layout_column_wrap
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
          h5(class = "fw-bold text-success mb-3", "Register New Accession"),
          p(class = "text-muted small mb-4", "Add a new seed line or variety to the passport database."),
          
          h6("Identification", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(
            width = 1/3,
            textInput(ns("g_name"), "Accession Name", placeholder = "e.g., SC-2026-001"),
            textInput(ns("g_preferred_name"), "Preferred/Local Name", placeholder = "e.g., Asontem"),
            textInput(ns("g_species"), "Species (Required)", placeholder = "e.g., Sorghum bicolor")
          ),
          
          h6("Classification & Status", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(
            width = 1/3,
            selectInput(ns("g_biological_status"), "Biological Status",
                        choices = c("Unknown", "Landrace", "Breeding Line", "Cultivar", "Wild Relative", "Population"),
                        selected = "Unknown"),
            selectInput(ns("g_accession_type"), "Accession Type",
                        choices = c("", "Released Variety", "Advanced Line", "Experimental Line", "Parent", "Check", "Elite Line", "Population")),
            selectInput(ns("g_status"), "Status",
                        choices = c("Available", "Inactive", "Archived", "Exhausted", "Regenerating"),
                        selected = "Available")
          ),
          
          h6("Source & Origin", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(
            width = 1/3,
            selectInput(ns("g_seed_source"), "Seed Source",
                        choices = c("", "Harvest", "Research Institution", "Farmer", "Gene Bank", "Purchase", "Donation", "Exchange", "Company", "Unknown")),
            textInput(ns("g_source_name"), "Source Name", placeholder = "e.g., CSIR-SARI, ICRISAT"),
            textInput(ns("g_country_of_origin"), "Country of Origin", placeholder = "e.g., Ghana")
          ),
          
          h6("Collection Information", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(
            width = 1/3,
            textInput(ns("g_collection_site"), "Collection Site", placeholder = "e.g., Tamale"),
            textInput(ns("g_collector_name"), "Collector Name", placeholder = "e.g., Dr. Kena"),
            dateInput(ns("g_acquisition_date"), "Acquisition Date (at KNUST)")
          ),
          
          h6("Breeding Information", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(
            width = 1/2,
            textInput(ns("g_pedigree"), "Pedigree / Cross", placeholder = "e.g., A x B"),
            textInput(ns("g_generation"), "Generation", placeholder = "e.g., F4, BC1F2")
          ),
          
          h6("Additional Remarks", class="text-primary fw-bold mt-4"),
          textAreaInput(ns("g_remarks"), "Remarks", placeholder = "Any other relevant notes...", width = "100%", rows = 3),
          
          hr(),
          actionButton(
            ns("btn_add_germplasm"),
            "Register Germplasm",
            class = "btn btn-success rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 2: TRAITS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("2. Traits", bsicons::bs_icon("rulers")),
          value = "traits",
              h5(
                class = "fw-bold text-warning mb-3",
                style = "color: #D97706 !important;",
                "Define Trait Vocabulary"
              ),
              p(class = "text-muted small mb-4", "Define a standard phenotypic trait before recording it."),
              bslib::layout_column_wrap(width = 1/2,
                selectizeInput(
                  ns("tr_name"),
                  "Trait Name (Select or Create)",
                  choices = NULL,
                  options = list(
                    create = TRUE,
                    placeholder = "e.g., Awn_Length"
                  )
                ),
                selectInput(ns("tr_data_type"), "Data Type",
                            choices = c("", "Numeric", "Text", "Score", "Boolean", "Date"))
              ),
              bslib::layout_column_wrap(width = 1/2,
                textInput(
                  ns("tr_unit"),
                  "Unit of Measurement",
                  placeholder = "e.g., cm, kg/ha, Score(1-5)"
                ),
                textInput(ns("tr_description"), "Description", placeholder = "e.g., Length of the awn in centimeters")
              ),
              textAreaInput(ns("tr_remarks"), "Remarks", placeholder = "Any other notes about this trait definition...", width = "100%", rows = 2),
              hr(),
              actionButton(
                ns("btn_add_trait"),
                "Register Trait",
                class = "btn btn-warning text-dark rounded-pill fw-bold float-end px-4",
                style = "background-color: #F59E0B; border: none;"
          )
        ),

        # ========================================================
        # TAB 3: TRIALS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("3. Trials", bsicons::bs_icon("clipboard-check")),
          value = "trials",
          h5(class = "fw-bold text-primary mb-3", "Create Field Trial"),
          p(class = "text-muted small mb-4", "Initialize an experimental field or greenhouse trial."),
          
          h6("Core Details", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(width = 1/3,
            textInput(ns("t_name"), "Trial Name (Required, Unique)", placeholder = "e.g., 2026 Bird Damage Trial"),
            textInput(ns("t_code"), "Trial Code (Unique)", placeholder = "e.g., BDT-2026"),
            selectizeInput(ns("t_type"), "Trial Type (Select or Create)", 
                         choices = c("", "Yield Trial", "Disease Screening", "Nursery", "Seed Multiplication", "Observation Trial", "Stress Screening", "Hybrid Evaluation", "Advanced Yield Trial", "Multi-location Trial", "Quality Evaluation", "Other"),
                         options = list(create = TRUE)
            )
          ),
          
          h6("Objective & Location", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(width = 1/2,
            textAreaInput(ns("t_objective"), "Scientific Objective", placeholder = "e.g., Evaluate drought tolerance.", rows = 2),
            textInput(ns("t_location"), "Location (Required)", placeholder = "e.g., KNUST Research Farm")
          ),
          bslib::layout_column_wrap(width = 1/2,
            numericInput(ns("t_lat"), "Latitude", value = NA),
            numericInput(ns("t_lon"), "Longitude", value = NA)
          ),
          
          h6("Season & Dates", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(width = 1/3,
            numericInput(ns("t_year"), "Year", value = format(Sys.Date(), "%Y")),
            selectInput(ns("t_season"), "Season", choices = c("", "Major", "Minor", "Dry Season", "Wet Season")),
            selectInput(ns("t_status"), "Trial Status", choices = c("Planned", "Active", "Completed", "Cancelled", "Archived"), selected = "Planned")
          ),
          bslib::layout_column_wrap(width = 1/3,
            dateInput(ns("t_planting_date"), "Planting Date"),
            dateInput(ns("t_expected_harvest_date"), "Expected Harvest Date"),
            dateInput(ns("t_actual_harvest_date"), "Actual Harvest Date")
          ),
          
          h6("Design & Management", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(width = 1/3,
            textInput(ns("t_exp_design"), "Experimental Design", placeholder = "e.g., RCBD, Alpha Lattice"),
            numericInput(ns("t_reps"), "Number of Replications", value = 1, min = 1),
            textInput(ns("t_pi"), "Principal Investigator", placeholder = "e.g., Dr. Kena")
          ),
          
          h6("Project & Metadata", class="text-primary fw-bold mt-4"),
          bslib::layout_column_wrap(width = 1/2,
            textInput(ns("t_project"), "Project Name", placeholder = "e.g., Green Evolution"),
            textAreaInput(ns("t_remarks"), "Remarks", placeholder = "General comments about the trial.", rows = 3)
          ),
          
          hr(),
          actionButton(
            ns("btn_add_trial"),
            "Initialize Trial",
            class = "btn btn-primary rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 4: PLOTS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("4. Plots", bsicons::bs_icon("grid-3x3-gap-fill")),
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
              h6("Plot Layout Details", class="text-primary fw-bold mt-4"),
              bslib::layout_column_wrap(
                width = 1/3,
                numericInput(ns("p_number"), "Plot Number", value = 101, min = 1),
                numericInput(ns("p_replication"), "Replication", value = 1, min = 1),
                numericInput(ns("p_block"), "Block (optional)", value = NA)
              ),
              bslib::layout_column_wrap(
                width = 1/2,
                numericInput(ns("p_row"), "Row (optional)", value = NA),
                numericInput(ns("p_column"), "Column (optional)", value = NA)
              ),
              textAreaInput(ns("p_remarks"), "Remarks", placeholder = "Any notes about this specific plot...", width = "100%", rows = 2),
              hr(),
              actionButton(
                ns("btn_add_plot"),
                "Create Plot Record",
                class = "btn btn-secondary rounded-pill fw-bold float-end px-4"
          )
        ),

        # ========================================================
        # TAB 5: OBSERVATIONS
        # ========================================================
        bslib::accordion_panel(
          title = tagList("5. Data", bsicons::bs_icon("clipboard-data")),
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
                textInput(
                  ns("obs_value"),
                  "Measured Value",
                  placeholder = "Enter value..."
                )
              ),
              textAreaInput(ns("obs_remarks"), "Remarks", placeholder = "Any notes about this observation...", width = "100%", rows = 2),
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
#' @param global_refresh_trigger A reactiveVal to signal data changes.
#' @noRd
mod_data_entry_server <- function(id, db_state, global_refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the full trait table (name + unit)
    all_traits_data <- reactiveVal(data.frame())
    
    # Observe changes in db_state$path or direct triggers to reload dropdowns
    observeEvent(list(db_state$path, global_refresh_trigger()), {
      path <- db_state$path
      req(path)
      
      if (file.exists(path)) {
        tryCatch({
          acc_list <- get_all_accessions(path)
          trial_list <- get_all_trials(path)

          # Fetch all trait details, store the data, and get the names for dropdowns
          traits_df <- get_all_traits_details(path)
          all_traits_data(traits_df)
          trait_list <- traits_df$trait_name
          
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
        # It's an existing trait, so auto-fill the fields
        updateTextInput(session, "tr_unit", value = if (is.na(trait_info$unit)) "" else trait_info$unit)
        updateTextInput(session, "tr_description", value = if (is.na(trait_info$trait_description)) "" else trait_info$trait_description)
        updateSelectInput(session, "tr_data_type", selected = if (is.na(trait_info$data_type)) "" else trait_info$data_type)
        updateTextAreaInput(session, "tr_remarks", value = if (is.na(trait_info$remarks)) "" else trait_info$remarks)
      } else {
        # It's a new trait, so clear the fields for the user to fill
        updateTextInput(session, "tr_unit", value = "")
        updateTextInput(session, "tr_description", value = "")
        updateSelectInput(session, "tr_data_type", selected = "")
        updateTextAreaInput(session, "tr_remarks", value = "")
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
        global_refresh_trigger(global_refresh_trigger() + 1)
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
      # Validation
      if (trimws(input$g_name) == "" || trimws(input$g_species) == "") {
        shinyWidgets::show_alert("Error", "Accession Name and Species are required.", type = "error")
        return()
      }
      if (!is.null(input$g_acquisition_date) && input$g_acquisition_date > Sys.Date()) {
        shinyWidgets::show_alert("Error", "Acquisition date cannot be in the future.", type = "error")
        return()
      }

      run_db_op("Germplasm registered!", {
        add_germplasm(
          db_path = db_state$path,
          accession_name = input$g_name,
          species = input$g_species,
          preferred_name = input$g_preferred_name,
          pedigree = input$g_pedigree,
          biological_status = input$g_biological_status,
          accession_type = input$g_accession_type,
          seed_source = input$g_seed_source,
          source_name = input$g_source_name,
          country_of_origin = input$g_country_of_origin,
          collection_site = input$g_collection_site,
          collector_name = input$g_collector_name,
          acquisition_date = input$g_acquisition_date,
          generation = input$g_generation,
          status = input$g_status,
          remarks = input$g_remarks,
          user_name = trimws(input$user_name)
        )
      })
    })
    
    # 3. Add Trial
    observeEvent(input$btn_add_trial, {
      # --- Validation ---
      if (trimws(input$t_name) == "" || trimws(input$t_location) == "") {
        shinyWidgets::show_alert("Error", "Trial Name and Location are required.", type = "error"); return()
      }
      if (!is.na(input$t_reps) && input$t_reps < 1) {
        shinyWidgets::show_alert("Error", "Number of Replications must be at least 1.", type = "error"); return()
      }
      if (!is.null(input$t_planting_date) && !is.null(input$t_expected_harvest_date) && input$t_expected_harvest_date < input$t_planting_date) {
        shinyWidgets::show_alert("Error", "Expected Harvest Date cannot be before Planting Date.", type = "error"); return()
      }
      if (!is.null(input$t_planting_date) && !is.null(input$t_actual_harvest_date) && input$t_actual_harvest_date < input$t_planting_date) {
        shinyWidgets::show_alert("Error", "Actual Harvest Date cannot be before Planting Date.", type = "error"); return()
      }

      run_db_op("Trial initialized!", {
        add_trial(
          db_path = db_state$path,
          trial_name = input$t_name,
          location = input$t_location,
          trial_code = input$t_code,
          trial_type = input$t_type,
          objective = input$t_objective,
          latitude = input$t_lat,
          longitude = input$t_lon,
          season = input$t_season,
          year = input$t_year,
          experimental_design = input$t_exp_design,
          number_of_replications = input$t_reps,
          principal_investigator = input$t_pi,
          project_name = input$t_project,
          planting_date = input$t_planting_date,
          expected_harvest_date = input$t_expected_harvest_date,
          actual_harvest_date = input$t_actual_harvest_date,
          trial_status = input$t_status,
          remarks = input$t_remarks,
          user_name = trimws(input$user_name)
        )
      })
    })
    
    # 4. Add Plot
    observeEvent(input$btn_add_plot, {
      req(input$p_trial, input$p_accession, input$p_number)
      run_db_op("Plot record created!", {
        add_plot(
          db_path = db_state$path,
          trial_name = input$p_trial,
          accession_name = input$p_accession,
          plot_number = input$p_number,
          replication = input$p_replication,
          block = input$p_block,
          row = input$p_row,
          column = input$p_column,
          remarks = input$p_remarks,
          user_name = trimws(input$user_name)
        )
      })
    })
    
    # 5. Add Trait
    observeEvent(input$btn_add_trait, {
      req(input$tr_name)
      run_db_op("Trait registered!", {
        add_trait(
          db_path = db_state$path,
          trait_name = trimws(input$tr_name),
          trait_description = input$tr_description,
          unit = input$tr_unit,
          data_type = input$tr_data_type,
          remarks = input$tr_remarks,
          user_name = trimws(input$user_name)
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
          user_name = trimws(input$user_name),
          remarks = input$obs_remarks
        )
      })
    })
    
  })
}
