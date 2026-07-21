################################################################################
#     DATABASE SETUP & MANAGEMENT
################################################################################

#' @title Initialize the Breeding Database
#'
#' @description
#' Creates a new SQLite database with the required schema for the breeding database.
#' If the database already exists, it connects to it and ensures the required tables
#' are present.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#'
#' @return Invisibly returns `NULL`. This function is primarily called for its
#'   side effect of creating and initializing the database schema.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbWithTransaction
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
#' \dontrun{
#' # Use case: Setting up the database for a new project
#' init_db(db_path = tempfile(fileext = ".sqlite"))
#' }
init_db <- function(db_path = "data/breeding_db.sqlite") {
  # Create directory if missing
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  # Connect to DB
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Disconnect on exit
  on.exit(DBI::dbDisconnect(con))

  # Enable foreign keys
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Create tables
  DBI::dbWithTransaction(con, {
    # Germplasm table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS germplasm (
        germplasm_id INTEGER PRIMARY KEY AUTOINCREMENT,
        accession_name TEXT NOT NULL UNIQUE,
        preferred_name TEXT,
        species TEXT NOT NULL,
        pedigree TEXT,
        biological_status TEXT DEFAULT 'Unknown',
        accession_type TEXT,
        seed_source TEXT,
        source_name TEXT,
        country_of_origin TEXT,
        collection_site TEXT,
        collector_name TEXT,
        acquisition_date DATE,
        generation TEXT,
        status TEXT DEFAULT 'Available',
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME
      );"
    )

    # Trigger to auto-update the 'updated_at' timestamp
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS germplasm_updated_at
      AFTER UPDATE ON germplasm
      FOR EACH ROW
      BEGIN
          UPDATE germplasm SET updated_at = CURRENT_TIMESTAMP WHERE germplasm_id = OLD.germplasm_id;
      END;"
    )

    # Trials table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS trials (
        trial_id INTEGER PRIMARY KEY AUTOINCREMENT,
        trial_name TEXT NOT NULL UNIQUE,
        trial_code TEXT UNIQUE,
        trial_type TEXT,
        objective TEXT,
        location TEXT NOT NULL,
        latitude REAL,
        longitude REAL,
        season TEXT,
        year INTEGER,
        experimental_design TEXT,
        number_of_replications INTEGER CHECK(number_of_replications > 0),
        principal_investigator TEXT,
        project_name TEXT,
        planting_date DATE,
        expected_harvest_date DATE,
        actual_harvest_date DATE,
        trial_status TEXT DEFAULT 'Planned',
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME
      );"
    )

    # Trigger for trials updated_at
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS trials_updated_at
      AFTER UPDATE ON trials
      FOR EACH ROW
      BEGIN
          UPDATE trials SET updated_at = CURRENT_TIMESTAMP WHERE trial_id = OLD.trial_id;
      END;")

    # Plots table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS plots (
        plot_id INTEGER PRIMARY KEY AUTOINCREMENT,
        trial_id INTEGER,
        germplasm_id INTEGER,
        plot_number INTEGER,
        replication INTEGER,
        block INTEGER,
        `row` INTEGER,
        `column` INTEGER,
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME,
        FOREIGN KEY (trial_id) REFERENCES trials(trial_id),
        FOREIGN KEY (germplasm_id) REFERENCES germplasm(germplasm_id),
        UNIQUE (trial_id, plot_number)
      );"
    )

    # Trigger for plots updated_at
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS plots_updated_at
      AFTER UPDATE ON plots
      FOR EACH ROW
      BEGIN
          UPDATE plots SET updated_at = CURRENT_TIMESTAMP WHERE plot_id = OLD.plot_id;
      END;")
      
    # Traits table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS traits (
        trait_id INTEGER PRIMARY KEY AUTOINCREMENT,
        trait_name TEXT NOT NULL UNIQUE,
        trait_description TEXT,
        unit TEXT,
        data_type TEXT,
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME
      );"
    )

    # Trigger for traits updated_at
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS traits_updated_at
      AFTER UPDATE ON traits
      FOR EACH ROW
      BEGIN
          UPDATE traits SET updated_at = CURRENT_TIMESTAMP WHERE trait_id = OLD.trait_id;
      END;")

    # Observations table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS observations (
        observation_id INTEGER PRIMARY KEY AUTOINCREMENT,
        plot_id INTEGER,
        trait_id INTEGER,
        observation_value TEXT,
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME,
        FOREIGN KEY (plot_id) REFERENCES plots(plot_id),
        FOREIGN KEY (trait_id) REFERENCES traits(trait_id),
        UNIQUE (plot_id, trait_id)
      );"
    )

    # Trigger for observations updated_at
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS observations_updated_at
      AFTER UPDATE ON observations
      FOR EACH ROW
      BEGIN
          UPDATE observations SET updated_at = CURRENT_TIMESTAMP WHERE observation_id = OLD.observation_id;
      END;")

    # Inventory table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS inventory (
        inventory_id INTEGER PRIMARY KEY AUTOINCREMENT,
        germplasm_id INTEGER NOT NULL,
        quantity REAL NOT NULL CHECK(quantity >= 0),
        unit TEXT DEFAULT 'g',
        storage_location TEXT NOT NULL,
        container TEXT,
        source_type TEXT,
        source_reference TEXT,
        deposit_reason TEXT,
        seed_status TEXT DEFAULT 'Available',
        viability_percent REAL CHECK(viability_percent >= 0 AND viability_percent <= 100),
        moisture_percent REAL,
        storage_condition TEXT,
        deposit_date DATE DEFAULT (date('now')),
        last_inventory_check DATE,
        remarks TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME,
        FOREIGN KEY (germplasm_id) REFERENCES germplasm(germplasm_id)
      );"
    )

    # Trigger for inventory updated_at
    DBI::dbExecute(con, "
      CREATE TRIGGER IF NOT EXISTS inventory_updated_at
      AFTER UPDATE ON inventory
      FOR EACH ROW
      BEGIN
          UPDATE inventory SET updated_at = CURRENT_TIMESTAMP WHERE inventory_id = OLD.inventory_id;
      END;")

    # Indexes for inventory
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_inventory_germplasm_id ON inventory(germplasm_id);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_inventory_storage_location ON inventory(storage_location);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_inventory_seed_status ON inventory(seed_status);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_inventory_source_type ON inventory(source_type);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_inventory_deposit_date ON inventory(deposit_date);")

    # Audit Log table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit_log (
        audit_id INTEGER PRIMARY KEY AUTOINCREMENT,
        table_name TEXT NOT NULL,
        record_id INTEGER NOT NULL,
        action TEXT NOT NULL,
        user_name TEXT,
        details TEXT,
        action_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
      );"
    )

    # Indexes for Audit Log
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_log_table_name ON audit_log(table_name);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_log_action ON audit_log(action);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_log_action_timestamp ON audit_log(action_timestamp);")


    # Add indexes for performance
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_germplasm_accession_name ON germplasm(accession_name);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_germplasm_species ON germplasm(species);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_germplasm_biological_status ON germplasm(biological_status);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_germplasm_accession_type ON germplasm(accession_type);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_germplasm_status ON germplasm(status);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_trial_name ON trials(trial_name);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_trial_code ON trials(trial_code);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_location ON trials(location);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_year ON trials(year);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_season ON trials(season);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_trial_type ON trials(trial_type);")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_trials_trial_status ON trials(trial_status);")
  })

  message("Database initialized successfully at: ", db_path)
}


#' @title Clear All Data from the Breeding Database
#'
#' @description
#' Deletes all records from all tables in the database while preserving the schema.
#' This is useful for resetting a test database or starting a new breeding season
#' from scratch. It also resets all autoincrement sequences.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use case: Resetting a test database before running tests
#' clear_db(db_path = tempfile(fileext = ".sqlite"))
#' }
clear_db <- function(db_path = "data/breeding_db.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Enable foreign keys
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbWithTransaction(con, {
    # Delete all data
    DBI::dbExecute(con, "DELETE FROM observations;")
    DBI::dbExecute(con, "DELETE FROM inventory;")
    DBI::dbExecute(con, "DELETE FROM plots;")
    DBI::dbExecute(con, "DELETE FROM traits;")
    DBI::dbExecute(con, "DELETE FROM trials;")
    DBI::dbExecute(con, "DELETE FROM germplasm;")
    DBI::dbExecute(con, "DELETE FROM audit_log;")

    # Reset AUTOINCREMENT
    DBI::dbExecute(con, "DELETE FROM sqlite_sequence;")
  })

  message("All data has been successfully deleted from the database.")
}


################################################################################
#     DATA ENTRY & INSERTS
################################################################################

#' @title Add Germplasm to Database
#'
#' @description
#' Registers a new seed line, variety, or genetic material (germplasm) into the
#' passport table of the database.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param accession_name Unique identifier for the seed line.
#' @param pedigree Genetic background or cross.
#' @param species Biological species (e.g., "Sorghum bicolor", "Vigna unguiculata").
#'
#' @return Invisibly returns `TRUE` on success, or throws an error on failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use case: Registering a new variety of cowpea
#' add_germplasm(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   accession_name = "Asontem",
#'   pedigree = "Local-Selection",
#'   species = "Vigna unguiculata"
#' )
#' }
add_germplasm <- function(
    db_path = "data/breeding_db.sqlite",
    accession_name,
    species,
    preferred_name = NULL,
    pedigree = NULL,
    biological_status = 'Unknown',
    accession_type = NULL,
    seed_source = NULL,
    source_name = NULL,
    country_of_origin = NULL,
    collection_site = NULL,
    collector_name = NULL,
    acquisition_date = NULL,
    generation = NULL,
    status = 'Available',
    remarks = NULL,
    user_name = "Unknown"
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Helper to clean strings, returning NULL if input is empty/NULL
  clean_text <- function(text, case = "lower") {
    if (is.null(text) || trimws(text) == "") return(NULL)
    if (case == "upper") {
      return(toupper(trimws(text)))
    }
    tolower(trimws(text))
  }

  query <- "INSERT INTO germplasm (
              accession_name, preferred_name, species, pedigree, biological_status,
              accession_type, seed_source, source_name, country_of_origin,
              collection_site, collector_name, acquisition_date, generation,
              status, remarks
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  params <- list(
    clean_text(accession_name, case = "upper"),
    clean_text(preferred_name),
    clean_text(species),
    clean_text(pedigree),
    clean_text(biological_status),
    clean_text(accession_type),
    clean_text(seed_source),
    clean_text(source_name),
    clean_text(country_of_origin),
    clean_text(collection_site),
    clean_text(collector_name),
    if (is.null(acquisition_date) || acquisition_date == "") NULL else as.character(acquisition_date),
    clean_text(generation),
    clean_text(status),
    clean_text(remarks)
  )

  tryCatch({
      DBI::dbExecute(con, query, params = params)
      
      # Log action
      record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
      details <- sprintf("Created new germplasm: %s", clean_text(accession_name, case = "upper"))
      DBI::dbExecute(
        con,
        "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
        params = list("germplasm", record_id, "INSERT", clean_text(user_name), details)
      )

      message(sprintf("Successfully added germplasm: '%s'", accession_name))
      invisible(TRUE)
    }, error = function(e) {
      if (grepl("UNIQUE constraint failed: germplasm.accession_name", e$message, ignore.case = TRUE)) {
        stop("Failed to add germplasm. The accession name is already in use.")
      } else {
        stop("Failed to add germplasm. Error: ", e$message)
      }
    })
}


#' @title Add a Trait Definition
#'
#' @description
#' Defines a new phenotypic trait and its associated unit of measurement in the
#' database. This standardizes the metrics recorded during trials.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param trait_name Name of the metric or trait being measured (e.g., "Aphid_Damage_Score").
#' @param unit Unit of measurement (e.g., "cm", "kg/ha", "1-5 Scale").
#'
#' @return Invisibly returns `TRUE` on success, or throws an error on failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use case: Defining standard traits for yield and disease resistance
#' try(add_trait(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   trait_name = "Pod_Yield",
#'   unit = "kg/ha"
#' ))
#' }
add_trait <- function(db_path = "data/breeding_db.sqlite",
                      trait_name,
                      trait_description = NULL,
                      unit = NULL,
                      data_type = NULL,
                      remarks = NULL,
                      user_name = "Unknown") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Helper to clean text inputs
  clean_text <- function(text) {
    if (is.null(text) || trimws(text) == "") return(NULL)
    trimws(text)
  }

  query <- "INSERT INTO traits (trait_name, trait_description, unit, data_type, remarks) VALUES (?, ?, ?, ?, ?)"
  
  params <- list(
    tolower(clean_text(trait_name)),
    clean_text(trait_description),
    clean_text(unit),
    clean_text(data_type),
    clean_text(remarks)
  )

  tryCatch({
    DBI::dbExecute(con, query, params = params)
    
    # Log action
    record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
    details <- sprintf("Created new trait: %s", tolower(clean_text(trait_name)))
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
      params = list("traits", record_id, "INSERT", clean_text(user_name), details)
    )

    message(sprintf("Successfully added trait: '%s'", trait_name))
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to add trait. Ensure trait_name is unique. Error: ", e$message)
  })
}


#' @title Add a Trial with Flexible Metadata
#'
#' @description
#' Creates a new field or greenhouse trial record in the database. Flexible metadata
#' can be provided as a list, which is automatically serialized into JSON format for
#' storage.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param trial_name Name of the experiment or trial.
#' @param trial_loc Location of the experiment (e.g., "Somanya_Field_Station").
#' @param start_date Start date of the trial in YYYY-MM-DD format.
#' @param metadata_list A named R list of custom trial parameters (e.g., experimental
#'   design, irrigation type). Defaults to an empty list.
#'
#' @return Invisibly returns `TRUE` on success, or throws an error on failure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use case: Setting up a new screening trial for cowpea aphids
#' add_trial(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   trial_name = "2026_Cowpea_Aphid_Screening",
#'   trial_loc = "Somanya_Field_Station",
#'   start_date = "2026-06-01",
#'   metadata_list = list(design = "RCBD", irrigation = "Rainfed")
#' )
#' }
add_trial <- function(db_path = "data/breeding_db.sqlite",
                      trial_name,
                      location,
                      trial_code = NULL,
                      trial_type = NULL,
                      objective = NULL,
                      latitude = NULL,
                      longitude = NULL,
                      season = NULL,
                      year = NULL,
                      experimental_design = NULL,
                      number_of_replications = NULL,
                      principal_investigator = NULL,
                      project_name = NULL,
                      planting_date = NULL,
                      expected_harvest_date = NULL,
                      actual_harvest_date = NULL,
                      trial_status = "Planned",
                      remarks = NULL,
                      user_name = "Unknown") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Helper to clean text inputs
  clean_text <- function(text) {
    if (is.null(text) || trimws(text) == "") return(NULL)
    trimws(text)
  }

  # Helper for dates
  clean_date <- function(date) {
    if (is.null(date) || date == "") return(NULL)
    as.character(date)
  }

  query <- "
    INSERT INTO trials (
      trial_name, trial_code, trial_type, objective, location, latitude, longitude,
      season, year, experimental_design, number_of_replications, principal_investigator,
      project_name, planting_date, expected_harvest_date, actual_harvest_date,
      trial_status, remarks
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  params <- list(
    clean_text(trial_name),
    clean_text(trial_code),
    clean_text(trial_type),
    clean_text(objective),
    clean_text(location),
    latitude,
    longitude,
    clean_text(season),
    year,
    clean_text(experimental_design),
    number_of_replications,
    clean_text(principal_investigator),
    clean_text(project_name),
    clean_date(planting_date),
    clean_date(expected_harvest_date),
    clean_date(actual_harvest_date),
    clean_text(trial_status),
    clean_text(remarks)
  )

  tryCatch({
    DBI::dbExecute(con, query, params = params)
    
    # Log action
    record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
    details <- sprintf("Created new trial: %s", clean_text(trial_name))
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
      params = list("trials", record_id, "INSERT", clean_text(user_name), details)
    )

    message(sprintf("Successfully added trial: '%s'", trial_name))
    invisible(TRUE)
  }, error = function(e) {
    if (grepl("UNIQUE constraint failed", e$message)) {
      if (grepl("trial_name", e$message)) {
        stop("Failed to add trial. The Trial Name is already in use.")
      } else if (grepl("trial_code", e$message)) {
        stop("Failed to add trial. The Trial Code is already in use.")
      }
    }
    stop("Failed to add trial. Database error: ", e$message)
  })
}


#' @title Link Germplasm to a Trial (Create a Plot)
#'
#' @description
#' Assigns a specific germplasm to a trial by creating a plot record.
#' Looks up the internal database IDs using the human-readable trial and accession names.
#'
#' @param db_path Path to the SQLite file.
#' @param trial_name The human-readable name of the trial.
#' @param accession_name The unique identifier of the seed line.
#' @param plot_number Physical or logical plot identifier (e.g., 101).
#' @param block Replicate or block number within the experimental design.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#' add_plot(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   trial_name = "2026_Cowpea_Aphid_Screening",
#'   accession_name = "Asontem",
#'   plot_number = 101,
#'   block = 1
#' )
#' }
add_plot <- function(
  db_path = "data/breeding_db.sqlite",
  trial_name,
  accession_name,
  plot_number,
  replication = NULL,
  block = NULL,
  row = NULL,
  column = NULL,
  remarks = NULL,
  user_name = "Unknown"
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Get trial ID
  t_res <- DBI::dbGetQuery(
    con,
    "SELECT trial_id FROM trials WHERE trial_name = ?",
    params = list(trimws(trial_name))
  )
  if (nrow(t_res) == 0) {
    stop("Trial name not found in the database.")
  }
  trial_id <- t_res$trial_id[1]

  # Get germplasm ID
  g_res <- DBI::dbGetQuery(
    con,
    "SELECT germplasm_id FROM germplasm WHERE accession_name = ?",
    params = list(toupper(trimws(accession_name)))
  )
  if (nrow(g_res) == 0) {
    stop("Accession name not found in the database.")
  }
  germplasm_id <- g_res$germplasm_id[1]

  query <- "INSERT INTO plots (trial_id, germplasm_id, plot_number, replication, block, `row`, `column`, remarks) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  DBI::dbExecute(
    con,
    query,
    params = list(trial_id, germplasm_id, plot_number, replication, block, row, column, remarks)
  )
  
  # Log action
  record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
  details <- sprintf("Created plot %s for accession %s in trial %s", plot_number, accession_name, trial_name)
  DBI::dbExecute(
    con,
    "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
    params = list("plots", record_id, "INSERT", user_name, details)
  )

  invisible(TRUE)
}


#' @title Record a Phenotypic Observation
#'
#' @description
#' Records a phenotypic measurement using human-readable plot numbers and trait names.
#' It automatically resolves internal IDs and writes a breeder-friendly ledger entry.
#'
#' @param db_path Path to the SQLite file.
#' @param trial_name The name of the trial this plot belongs to.
#' @param plot_number The physical plot number in the field.
#' @param trait_name The name of the trait being measured (e.g., "Awn_Length").
#' @param value Numeric value of the recorded observation.
#' @param user_name Name of the researcher entering the data.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#' add_observation(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   trial_name = "2026_Cowpea_Aphid_Screening",
#'   plot_number = 101,
#'   trait_name = "Aphid_Damage_Score",
#'   value = 4.5,
#'   user_name = "Israel Tetteh"
#' )
#' }
add_observation <- function(
  db_path = "data/breeding_db.sqlite",
  trial_name,
  plot_number,
  trait_name,
  value,
  user_name,
  remarks = NULL
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Get plot ID and accession name
  p_query <- "
    SELECT p.plot_id, g.accession_name 
    FROM plots p 
    JOIN trials tr ON p.trial_id = tr.trial_id
    JOIN germplasm g ON p.germplasm_id = g.germplasm_id
    WHERE tr.trial_name = ? AND p.plot_number = ?
  "
  p_res <- DBI::dbGetQuery(
    con,
    p_query,
    params = list(trimws(trial_name), plot_number)
  )
  if (nrow(p_res) == 0) {
    stop("Plot number not found for that trial.")
  }
  plot_id <- p_res$plot_id[1]
  acc_name <- p_res$accession_name[1]

  # Get trait ID
  t_res <- DBI::dbGetQuery(
    con,
    "SELECT trait_id FROM traits WHERE trait_name = ?",
    params = list(tolower(trimws(trait_name)))
  )
  if (nrow(t_res) == 0) {
    stop("Trait name not found in the database.")
  }
  trait_id <- t_res$trait_id[1]

  DBI::dbWithTransaction(con, {
    # Check if observation already exists for this plot and trait
    check_obs <- DBI::dbGetQuery(
      con,
      "SELECT observation_id FROM observations WHERE plot_id = ? AND trait_id = ?",
      params = list(plot_id, trait_id)
    )

    # Helper to clean text inputs
    clean_text <- function(text) {
      if (is.null(text) || trimws(text) == "") return(NULL)
      trimws(text)
    }

    if (nrow(check_obs) == 0) {
      # Insert new record
      DBI::dbExecute(
        con,
        "INSERT INTO observations (plot_id, trait_id, observation_value, remarks) VALUES (?, ?, ?, ?)",
        params = list(plot_id, trait_id, clean_text(value), clean_text(remarks))
      )
      action <- "INSERT"
      record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
    } else {
      # Update existing record (correction/overwrite)
      DBI::dbExecute(
        con,
        "UPDATE observations SET observation_value = ?, remarks = ? WHERE plot_id = ? AND trait_id = ?",
        params = list(clean_text(value), clean_text(remarks), plot_id, trait_id)
      )
      action <- "UPDATE"
      record_id <- check_obs$observation_id[1]
    }

    # Format log details
    details <- sprintf(
      "Recorded %s: %s for Plot %s (%s)",
      tolower(trimws(trait_name)),
      value,
      plot_number,
      acc_name
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
      params = list("observations", record_id, action, tolower(trimws(user_name)), details)
    )
  })

  invisible(TRUE)
}


#' @title Deposit Seeds into Inventory
#'
#' @description
#' Adds seeds to the inventory using the accession name. Updates existing stock
#' or creates a new record, logging the atomic transaction in human-readable format.
#'
#' @param db_path Path to the SQLite file.
#' @param accession_name The unique identifier of the seed line.
#' @param amount_grams Quantity of seeds to add, strictly in grams.
#' @param storage_location Identifier for the freezer or cold room.
#' @param user_name Name of the researcher making the deposit.
#' @param reason Details regarding the deposit (e.g., "2026 Harvest").
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#' add_inventory_deposit(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   accession_name = "SC-2026-001",
#'   amount_grams = 1500,
#'   storage_location = "Cold_Room_Shelf_A",
#'   user_name = "Israel Tetteh",
#'   reason = "Harvest from 2026 Trial"
#' )
#' }
add_inventory_deposit <- function(db_path = "data/breeding_db.sqlite",
                                  accession_name,
                                  quantity,
                                  unit = "g",
                                  storage_location,
                                  user_name,
                                  container = NULL,
                                  source_type = NULL,
                                  source_reference = NULL,
                                  deposit_reason = NULL,
                                  seed_status = "Available",
                                  viability_percent = NULL,
                                  moisture_percent = NULL,
                                  storage_condition = NULL,
                                  deposit_date = Sys.Date(),
                                  remarks = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # --- Validation ---
  if (quantity <= 0) stop("Quantity must be greater than zero.")
  if (trimws(storage_location) == "") stop("Storage location cannot be empty.")
  if (deposit_date > Sys.Date()) stop("Deposit date cannot be in the future.")
  if (!is.null(viability_percent) && (viability_percent < 0 || viability_percent > 100)) {
    stop("Viability must be between 0 and 100.")
  }

  # Helper to clean text inputs
  clean_text <- function(text) {
    if (is.null(text) || trimws(text) == "") return(NULL)
    trimws(text)
  }

  accession_name_clean <- toupper(clean_text(accession_name))
  
  # Get germplasm ID
  g_res <- DBI::dbGetQuery(
    con,
    "SELECT germplasm_id FROM germplasm WHERE accession_name = ?",
    params = list(accession_name_clean)
  )
  if (nrow(g_res) == 0) stop("Accession name not found in the database.")
  germplasm_id <- g_res$germplasm_id[1]

  DBI::dbWithTransaction(con, {
    # Always insert a new row for each deposit, treating it as a new lot
    insert_query <- "
      INSERT INTO inventory (
        germplasm_id, quantity, unit, storage_location, container, source_type,
        source_reference, deposit_reason, seed_status, viability_percent,
        moisture_percent, storage_condition, deposit_date, remarks
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    params <- list(
      germplasm_id,
      quantity,
      clean_text(unit),
      clean_text(storage_location),
      clean_text(container),
      clean_text(source_type),
      clean_text(source_reference),
      clean_text(deposit_reason),
      clean_text(seed_status),
      viability_percent,
      moisture_percent,
      clean_text(storage_condition),
      as.character(deposit_date),
      clean_text(remarks)
    )

    DBI::dbExecute(con, insert_query, params = params)

    # Log action
    # Create a structured JSON detail for better history tracking
    record_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid();")[[1]]
    details_list <- list(
      type = "DEPOSIT",
      accession = accession_name_clean,
      lot_id = record_id,
      quantity_before = 0,
      quantity_changed = quantity,
      quantity_after = quantity,
      unit = unit,
      reason = clean_text(deposit_reason)
    )
    details_json <- jsonlite::toJSON(details_list, auto_unbox = TRUE)
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
      params = list("inventory", record_id, "INSERT", clean_text(user_name), details_json)
    )
  })
  
  message(sprintf(
    "Successfully deposited %s %s of '%s' into inventory.",
    quantity, unit,
    accession_name
  ))
  invisible(TRUE)
}

#' @title Update Germplasm Details
#'
#' @description
#' Updates the details of an existing germplasm record and logs the change.
#'
#' @param db_path Path to the SQLite file.
#' @param accession_name The unique identifier of the seed line to update.
#' @param updates A named list of columns and their new values to update.
#' @param user_name The name of the user making the change.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
update_germplasm <- function(db_path, accession_name, updates, user_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Clean inputs
  accession_name_clean <- toupper(trimws(accession_name))
  user_name_clean <- tolower(trimws(user_name))

  # Clean the update values and remove any empty ones
  updates_clean <- lapply(updates, function(x) if (is.character(x)) trimws(x) else x)
  updates_clean <- updates_clean[sapply(updates_clean, function(x) !is.null(x) && x != "")]

  if (length(updates_clean) == 0) {
    message("No updates to perform.")
    return(invisible(TRUE))
  }

  DBI::dbWithTransaction(con, {
    # Get current state for logging
    current_data <- DBI::dbGetQuery(
      con,
      "SELECT * FROM germplasm WHERE accession_name = ?",
      params = list(accession_name_clean)
    )

    if (nrow(current_data) == 0) {
      stop("Update failed: Accession name not found.")
    }
    
    # Build update query dynamically
    set_clauses <- paste(names(updates_clean), "= ?", collapse = ", ")
    query <- sprintf("UPDATE germplasm SET %s WHERE accession_name = ?", set_clauses)
    params <- c(unname(updates_clean), list(accession_name_clean))
    
    DBI::dbExecute(con, query, params = params)

    # Create a detailed log message
    changes <- sapply(names(updates_clean), function(field) {
      old_val <- current_data[[field]]
      new_val <- updates_clean[[field]]
      # Use `identical` to handle NULLs and avoid logging non-changes
      if (!identical(as.character(old_val), as.character(new_val))) {
        sprintf("%s from '%s' to '%s'", field, ifelse(is.na(old_val), "NULL", old_val), new_val)
      } else {
        NULL
      }
    })
    changes <- Filter(Negate(is.null), changes)

    if (length(changes) > 0) {
      details <- sprintf("UPDATE %s: %s", accession_name_clean, paste(changes, collapse = "; "))

      DBI::dbExecute(
        con,
        "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
        params = list("germplasm", current_data$germplasm_id, "UPDATE", user_name_clean, details)
      )
    }
  })
  invisible(TRUE)
}


################################################################################
#    SHINY APP HELPERS
################################################################################

#' @title Get All Accession Names
#' @description Retrieves a vector of all unique germplasm accession names.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of accession names.
#' @noRd
get_all_accessions <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT accession_name FROM germplasm ORDER BY accession_name"
  )$accession_name
}

#' @title Get All Trial Names
#' @description Retrieves a vector of all unique trial names.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of trial names.
#' @noRd
get_all_trials <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT trial_name FROM trials ORDER BY trial_name"
  )$trial_name
}

#' @title Get All Trait Names
#' @description Retrieves a vector of all unique trait names.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of trait names.
#' @noRd
get_all_traits <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT trait_name FROM traits ORDER BY trait_name"
  )$trait_name
}

#' @title Get All Trait Details
#' @description Retrieves a data frame of all unique trait names and their units.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A data frame with all trait details.
#' @noRd
get_all_traits_details <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT trait_id, trait_name, trait_description, unit, data_type, remarks FROM traits ORDER BY trait_name"
  )
}

#' @title Get Raw Data For a Specific Accession
#' @description Extracts all raw, plot-level field data for a single accession across all trials.
#' @param db_path Path to the SQLite file.
#' @param accession_name Name of the specific accession to extract.
#' @importFrom dplyr bind_rows
#' @return A single data frame of all observations for the accession.
#' @importFrom dplyr bind_rows
#' @noRd
get_raw_accession_data <- function(db_path, accession_name) {
  # This function returns a list of data frames, one for each trial
  field_books <- get_field_book(db_path = db_path, accession_name = accession_name)
  
  if (length(field_books) == 0) {
    return(data.frame())
  }
  
  # Combine the list of data frames into a single data frame
  dplyr::bind_rows(field_books)
}

#' @title Get Accession-Specific Audit Log
#' @description Retrieves audit log entries relevant to a specific accession.
#' @param db_path Path to the SQLite file.
#' @param accession_name The accession name to search for in the log details.
#' @return A data frame of relevant log entries.
#' @noRd
get_accession_audit_log <- function(db_path, accession_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  query <- "
    SELECT action_timestamp, table_name, record_id, action, user_name, details
    FROM audit_log
    WHERE details LIKE ?
    ORDER BY action_timestamp DESC
  "
  
  search_pattern <- paste0("%", toupper(trimws(accession_name)), "%")
  result <- DBI::dbGetQuery(con, query, params = list(search_pattern))
  return(result)
}

#' @title Get All Species Names
#' @description Retrieves a vector of all unique species names.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of species names.
#' @noRd
get_all_species <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT species FROM germplasm ORDER BY species"
  )$species
}

#' @title Get Accessions by Species
#' @description Retrieves a data frame of all accessions for a given species.
#' @param db_path Path to the SQLite file.
#' @param species_name The species to filter by.
#' @return A data frame with accession_name and pedigree.
#' @noRd
get_accessions_by_species <- function(db_path, species_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT accession_name, pedigree FROM germplasm WHERE species = ? ORDER BY accession_name",
    params = list(tolower(trimws(species_name)))
  )
}

#' @title Get Inventory by Species
#' @description Retrieves inventory details for all accessions of a given species.
#' @param db_path Path to the SQLite file.
#' @param species_name The species to filter by.
#' @return A data frame with inventory details.
#' @noRd
get_inventory_by_species <- function(db_path, species_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  query <- "
    SELECT
      g.accession_name,
      SUM(CASE WHEN i.unit = 'g' THEN i.quantity ELSE 0 END) AS total_grams,
      SUM(CASE WHEN i.unit = 'kg' THEN i.quantity ELSE 0 END) AS total_kg,
      SUM(CASE WHEN i.unit = 'Seeds' THEN i.quantity ELSE 0 END) AS total_seeds,
      SUM(CASE WHEN i.unit = 'Packets' THEN i.quantity ELSE 0 END) AS total_packets,
      COUNT(i.inventory_id) AS lot_count
    FROM germplasm g
    LEFT JOIN inventory i ON g.germplasm_id = i.germplasm_id
    WHERE g.species = ?
    GROUP BY g.accession_name
    ORDER BY g.accession_name
  "
  
  DBI::dbGetQuery(con, query, params = list(tolower(trimws(species_name))))
}

#' @title Get Inventory Summary Statistics
#' @description Calculates key metrics for the inventory dashboard.
#' @param db_path Path to the SQLite file.
#' @return A named list of summary statistics.
#' @noRd
get_inventory_summary_stats <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  total_accessions <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT accession_name) FROM germplasm;")[[1]]
  total_lots <- DBI::dbGetQuery(con, "SELECT COUNT(inventory_id) FROM inventory;")[[1]]
  low_stock_lots <- DBI::dbGetQuery(con, "SELECT COUNT(inventory_id) FROM inventory WHERE quantity > 0 AND quantity < 50 AND unit = 'g';")[[1]]
  empty_lots <- DBI::dbGetQuery(con, "SELECT COUNT(inventory_id) FROM inventory WHERE quantity = 0;")[[1]]
  
  list(
    total_accessions = total_accessions,
    total_lots = total_lots,
    low_stock_lots = low_stock_lots,
    empty_lots = empty_lots
  )
}

#' @title Get Inventory Movement History
#' @description Retrieves and parses inventory movement from the audit log.
#' @param db_path Path to the SQLite file.
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @return A data frame of inventory movement history.
#' @noRd
get_inventory_history <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Query for inventory-related audit logs
  audit_data <- DBI::dbGetQuery(con, "SELECT action_timestamp, details FROM audit_log WHERE table_name = 'inventory' ORDER BY action_timestamp DESC")
  
  if (nrow(audit_data) == 0) return(data.frame())
  
  # Parse the JSON details column
  history_list <- lapply(audit_data$details, function(json_str) {
    tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
  })
  
  # Filter out any parsing errors and bind into a data frame
  history_df <- dplyr::bind_rows(Filter(Negate(is.null), history_list))
  history_df$action_timestamp <- audit_data$action_timestamp[sapply(history_list, Negate(is.null))]
  return(history_df)
}

#' @title Get All Biological Statuses
#' @description Retrieves a vector of all unique biological statuses.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of biological statuses.
#' @noRd
get_all_biological_statuses <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT biological_status FROM germplasm WHERE biological_status IS NOT NULL ORDER BY biological_status"
  )$biological_status
}

#' @title Get All Accession Types
#' @description Retrieves a vector of all unique accession types.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of accession types.
#' @noRd
get_all_accession_types <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT accession_type FROM germplasm WHERE accession_type IS NOT NULL ORDER BY accession_type"
  )$accession_type
}

#' @title Get All Seed Sources
#' @description Retrieves a vector of all unique seed sources.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of seed sources.
#' @noRd
get_all_seed_sources <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT seed_source FROM germplasm WHERE seed_source IS NOT NULL ORDER BY seed_source"
  )$seed_source
}

#' @title Get All Countries of Origin
#' @description Retrieves a vector of all unique countries of origin.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of countries.
#' @noRd
get_all_countries_of_origin <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT country_of_origin FROM germplasm WHERE country_of_origin IS NOT NULL ORDER BY country_of_origin"
  )$country_of_origin
}

#' @title Get All Trial Years
#' @description Retrieves a vector of all unique trial years.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of years.
#' @noRd
get_all_trial_years <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT year FROM trials WHERE year IS NOT NULL ORDER BY year DESC"
  )$year
}

#' @title Get All Trial Seasons
#' @description Retrieves a vector of all unique trial seasons.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of seasons.
#' @noRd
get_all_trial_seasons <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT season FROM trials WHERE season IS NOT NULL ORDER BY season"
  )$season
}

#' @title Get All Trial Statuses
#' @description Retrieves a vector of all unique trial statuses.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of statuses.
#' @noRd
get_all_trial_statuses <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT trial_status FROM trials WHERE trial_status IS NOT NULL ORDER BY trial_status"
  )$trial_status
}

#' @title Get All Trial Types
#' @description Retrieves a vector of all unique trial types.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of types.
#' @noRd
get_all_trial_types <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT trial_type FROM trials WHERE trial_type IS NOT NULL ORDER BY trial_type"
  )$trial_type
}

#' @title Get All Principal Investigators
#' @description Retrieves a vector of all unique PIs.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of PIs.
#' @noRd
get_all_principal_investigators <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT principal_investigator FROM trials WHERE principal_investigator IS NOT NULL ORDER BY principal_investigator"
  )$principal_investigator
}

#' @title Get All Storage Locations
#' @description Retrieves a vector of all unique storage locations.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of storage locations.
#' @noRd
get_all_storage_locations <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT storage_location FROM inventory WHERE storage_location IS NOT NULL ORDER BY storage_location"
  )$storage_location
}

#' @title Get All Seed Statuses
#' @description Retrieves a vector of all unique seed statuses.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of seed statuses.
#' @noRd
get_all_seed_statuses <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT seed_status FROM inventory WHERE seed_status IS NOT NULL ORDER BY seed_status"
  )$seed_status
}

#' @title Get All Replication Numbers
#' @description Retrieves a vector of all unique replication numbers from plots.
#' @param db_path A character string specifying the path to the SQLite file.
#' @return A character vector of replication numbers.
#' @noRd
get_all_replications <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT replication FROM plots WHERE replication IS NOT NULL ORDER BY replication"
  )$replication
}

################################################################################
#    DATA RETRIEVAL & REPORTING
################################################################################

#' @title Get Seed Inventory Status
#'
#' @description
#' Retrieves the current physical stock levels for all germplasm in the database,
#' or a specific accession. Automatically handles seeds that are registered but
#' not currently in stock by displaying zero balances.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param target_accession (Optional) A specific accession name to search for
#'   (e.g., "SC-2026-001"). If `NULL`, returns the entire seed inventory.
#'
#' @return A data frame containing the accession name, species, quantity, unit,
#'   and storage location.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
#' \dontrun{
#' # View the entire college seed inventory
#' all_stock <- get_inventory_status()
#'
#' # Check if you have enough of a specific Sorghum line for a screenhouse test
#' sc_stock <- get_inventory_status(target_accession = "SC-2026-001")
#' }
get_inventory_status <- function(
  db_path = "data/breeding_db.sqlite",
  target_accession = NULL
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "
    SELECT 
      g.accession_name,
      g.species,
      i.inventory_id,
      i.storage_location,
      i.container,
      i.quantity,
      i.unit,
      i.seed_status,
      i.source_type,
      i.source_reference,
      i.deposit_date,
      i.viability_percent
    FROM germplasm g
    LEFT JOIN (
      SELECT * FROM inventory WHERE quantity > 0
    ) i ON g.germplasm_id = i.germplasm_id
  "

  if (!is.null(target_accession)) {
    query <- paste0(query, " WHERE g.accession_name = ?")
    result <- DBI::dbGetQuery(con, query, params = list(toupper(trimws(target_accession))))
  } else {
    result <- DBI::dbGetQuery(con, query)
  }

  return(result)
}


#' @title Get Germplasm Passport (Historical Performance by Trial)
#'
#' @description
#' Generates a comprehensive performance profile for a specific seed line.
#' To account for Genotype by Environment (GxE) interactions, it averages
#' replicate blocks but strictly isolates performance by specific trial locations
#' and dates.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param target_accession The specific accession name to profile.
#'
#' @return A data frame of historical trait averages broken down by trial environment.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull the historical performance of a Sorghum line across all past trials
#' passport_data <- get_germplasm_passport(target_accession = "SC-2026-001")
#' }
get_germplasm_passport <- function(
  db_path = "data/breeding_db.sqlite",
  target_accession
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "
    SELECT 
      g.accession_name,
      g.species,
      g.preferred_name,
      g.generation,
      g.pedigree,
      tr.trial_name,
      tr.location,
      t.trait_name,
      t.unit,
      AVG(o.observation_value) as value
    FROM germplasm g
    JOIN plots p ON g.germplasm_id = p.germplasm_id
    JOIN trials tr ON p.trial_id = tr.trial_id
    JOIN observations o ON p.plot_id = o.plot_id
    JOIN traits t ON o.trait_id = t.trait_id
    WHERE g.accession_name = ?
    GROUP BY 
      g.accession_name, 
      g.species, 
      g.preferred_name,
      g.generation,
      g.pedigree, 
      tr.trial_name, 
      tr.location, 
      t.trait_name, 
      t.unit
    ORDER BY tr.planting_date DESC
  "

  result <- DBI::dbGetQuery(con, query, params = list(toupper(trimws(target_accession))))
  return(result)
}

#' @title Get Full Trial Data and Metadata
#'
#' @description
#' Extracts an entire experiment's data. Unpacks custom JSON trial metadata
#' (like irrigation methods) and automatically pivots the plot observations
#' into a breeder-friendly "Wide Format" spreadsheet, ready for CSV export
#' or statistical modeling.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param target_trial_name The human-readable name of the trial to extract.
#' @param wide_format Logical. If `TRUE` (default), pivots the observations so
#'   each trait is its own column. If `FALSE`, returns the raw long format.
#'
#' @return A named list containing `trial_info` (unpacked metadata) and
#'   `observations` (the data frame of field measurements).
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite fromJSON
#' @importFrom stats reshape
#' @export
#'
#' @examples
#' \dontrun{
#' # Export the full dataset for the master's thesis Awnness validation trial
#' trial_export <- get_trial_data(target_trial_name = "2026_Sorghum_Awnness_DAI")
#'
#' # Print the specific experimental design used
#' print(trial_export$trial_info$custom_parameters$experimental_design)
#'
#' # View the wide-format field book
#' head(trial_export$observations)
#' }
get_trial_data <- function(
  db_path = "data/breeding_db.sqlite",
  target_trial_name,
  wide_format = TRUE
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "
    SELECT 
      tr.trial_name,
      tr.location,
      tr.planting_date,
      tr.trial_code,
      tr.trial_type,
      tr.objective,
      tr.year,
      tr.season,
      tr.experimental_design,
      tr.number_of_replications,
      tr.principal_investigator,
      tr.project_name,
      tr.trial_status,
      p.plot_number,
      p.block,
      p.replication,
      p.row,
      p.column,
      g.accession_name,
      t.trait_name,
      o.observation_value
    FROM trials tr
    JOIN plots p ON tr.trial_id = p.trial_id
    JOIN germplasm g ON p.germplasm_id = g.germplasm_id
    JOIN observations o ON p.plot_id = o.plot_id
    JOIN traits t ON o.trait_id = t.trait_id
    WHERE tr.trial_name = ?
  "
  
  raw_data <- DBI::dbGetQuery(con, query, params = list(trimws(target_trial_name)))

  if (nrow(raw_data) == 0) {
    message("No data found for the specified trial.")
    return(NULL)
  }

  # Subset observation columns
  obs_df <- unique(raw_data[, c(
    "plot_number",
    "replication",
    "block",
    "row",
    "column",
    "accession_name",
    "trait_name",
    "observation_value"
  )])

  # Pivot to wide format
  if (wide_format) {
    obs_df <- reshape(
      obs_df,
      idvar = c("plot_number", "replication", "block", "row", "column", "accession_name"),
      timevar = "trait_name",
      v.names = "observation_value",
      direction = "wide"
    )
    # Clean column prefixes
    names(obs_df) <- gsub("^observation_value\\.", "", names(obs_df))
  }

  # Return list
  list(
    trial_info = list(
      trial_name = raw_data$trial_name[1],
      trial_code = raw_data$trial_code[1],
      trial_type = raw_data$trial_type[1],
      objective = raw_data$objective[1],
      location = raw_data$location[1],
      year = raw_data$year[1],
      season = raw_data$season[1],
      planting_date = raw_data$planting_date[1],
      experimental_design = raw_data$experimental_design[1],
      number_of_replications = raw_data$number_of_replications[1],
      principal_investigator = raw_data$principal_investigator[1],
      project_name = raw_data$project_name[1],
      trial_status = raw_data$trial_status[1]
    ),
    observations = obs_df
  )
}


#' @title Get Raw Field Book Data (By Trial or Accession)
#'
#' @description
#' Extracts raw, plot-level field data. This function is flexible: it can be
#' queried by a specific trial name (to see all accessions in that experiment)
#' OR by a specific accession name (to see how that seed performed across all
#' its various trials).
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param trial_name (Optional) Name of the specific trial to extract.
#' @param accession_name (Optional) Name of the specific accession to extract.
#'
#' @return A named list of data frames. Each data frame represents the wide-format
#'   field book for a specific trial, perfectly mimicking an Excel workbook with
#'   multiple sheets.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom stats reshape
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the field book for a whole trial
#' trial_data <- get_field_book(trial_name = "2026_Sorghum_Awnness_DAI")
#'
#' # Get the field books for every trial a specific seed was in
#' seed_history <- get_field_book(accession_name = "SC-2026-001")
#' }
get_field_book <- function(
  db_path = "data/breeding_db.sqlite",
  trial_name = NULL,
  accession_name = NULL
) {
  # Check input parameters
  if (is.null(trial_name) && is.null(accession_name)) {
    stop(
      "Please provide either a trial_name or an accession_name to search for."
    )
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Base query
  query <- "
    SELECT 
      tr.trial_name,
      p.plot_number,
      p.block,
      p.replication,
      p.row,
      p.column,
      g.accession_name,
      t.trait_name,
      o.observation_value
    FROM trials tr
    JOIN plots p ON tr.trial_id = p.trial_id
    JOIN germplasm g ON p.germplasm_id = g.germplasm_id
    JOIN observations o ON p.plot_id = o.plot_id
    JOIN traits t ON o.trait_id = t.trait_id
    WHERE 1=1
  "

  params <- list()

  # Apply filters
  if (!is.null(trial_name)) {
    query <- paste0(query, " AND tr.trial_name = ?")
    params <- append(params, trimws(trial_name))
  }

  if (!is.null(accession_name)) {
    query <- paste0(query, " AND g.accession_name = ?")
    params <- append(params, toupper(trimws(accession_name)))
  }

  raw_data <- DBI::dbGetQuery(con, query, params = params)

  if (nrow(raw_data) == 0) {
    message("No field data found for those parameters.")
    return(list())
  }

  # Pivot to wide format
  wide_data <- reshape(
    raw_data,
    idvar = c("trial_name", "plot_number", "replication", "block", "row", "column", "accession_name"),
    timevar = "trait_name",
    v.names = "observation_value",
    direction = "wide"
  )
  names(wide_data) <- gsub("^observation_value\\.", "", names(wide_data))

  list_of_dfs <- split(wide_data, wide_data$trial_name)

  return(list_of_dfs)
}


#' @title Get the Scientific Audit Ledger
#'
#' @description
#' Retrieves a log of recent database activity (inserts, updates, etc.) to maintain
#' a scientific audit trail.
#'
#' @param db_path Path to the SQLite file.
#' @param limit Number of recent transactions to return. Defaults to 100.
#'
#' @return A dataframe of recent database activity.
#' @export
#'
#' @examples
#' \dontrun{
#' audit_log <- get_audit_ledger(limit = 50)
#' head(audit_log)
#' }
get_audit_ledger <- function(db_path = "data/breeding_db.sqlite", limit = 100) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Query recent logs
  query <- "
    SELECT 
      action_timestamp, 
      table_name, 
      record_id,
      action, 
      user_name, 
      details
    FROM audit_log
    ORDER BY action_timestamp DESC
    LIMIT ?
  "

  result <- DBI::dbGetQuery(con, query, params = list(limit))
  return(result)
}


#' @title Get Low Stock Alerts
#'
#' @description
#' Identifies germplasm accessions in the inventory whose quantities have fallen
#' below a specified threshold, signaling the need for seed multiplication.
#'
#' @param db_path Path to the SQLite file.
#' @param threshold Numeric value in grams. Any seed with stock below this value is returned. Defaults to 500g.
#'
#' @return A dataframe of germplasm accessions that need multiplication.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all accessions with less than 1000g of seed remaining
#' low_stock_alerts <- get_low_stock(threshold = 1000)
#' }
get_low_stock <- function(
  db_path = "data/breeding_db.sqlite",
  threshold = 500
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Query low stock items
  query <- "
    SELECT 
      g.accession_name,
      g.species,
      i.quantity,
      i.unit,
      i.storage_location
    FROM germplasm g
    JOIN inventory i ON g.germplasm_id = i.germplasm_id
    WHERE i.quantity < ?
    ORDER BY i.quantity ASC
  "

  result <- DBI::dbGetQuery(con, query, params = list(threshold))

  return(result)
}


#' @title Withdraw Seeds from Inventory
#'
#' @description
#' Removes a specified amount of seeds from the inventory using the human-readable
#' accession name. It automatically converts units to grams, updates the stock,
#' and writes a breeder-friendly ledger entry. Prevents overdrafts if the requested
#' amount exceeds current stock.
#'
#' @param db_path A character string specifying the path to the SQLite file.
#'   Defaults to `"data/breeding_db.sqlite"`.
#' @param accession_name The unique identifier of the seed line.
#' @param withdraw_amount Numeric quantity of seeds to remove.
#' @param storage_location Identifier for the freezer or cold room from which to withdraw.
#' @param withdraw_unit Unit of the amount (e.g., "g", "grams", "kg", "kilograms").
#' @param user_name Name of the researcher withdrawing the seeds.
#' @param reason Details regarding the withdrawal (e.g., "Screenhouse trial control").
#'
#' @return Invisibly returns `TRUE` on success, or throws an error on failure.
#' @export
#'
#' @examples
#' \dontrun{
#' withdraw_seed(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   accession_name = "SC-2026-001",
#'   storage_location = "Cold_Room_Shelf_A",
#'   withdraw_amount = 75,
#'   withdraw_unit = "grams",
#'   user_name = "Israel Tetteh",
#'   reason = "Setup for drought stress block"
#' )
#' }
withdraw_seed <- function(db_path = "data/breeding_db.sqlite",
                          inventory_id,
                          withdraw_amount,
                          withdraw_unit,
                          user_name,
                          reason) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # --- Validation & Setup ---
  if (withdraw_amount <= 0) stop("Withdrawal amount must be positive.")
  user_name_clean <- trimws(user_name)
  reason_clean <- trimws(reason)
  unit_lower <- tolower(trimws(withdraw_unit))

  # Convert withdrawal amount to grams for consistent comparison
  amount_in_grams <- if (unit_lower %in% c("kg", "kilogram", "kilograms")) {
    withdraw_amount * 1000
  } else {
    withdraw_amount
  }

  # --- Database Transaction ---
  DBI::dbWithTransaction(con, {
    # Get current stock details for the specific inventory item
    current_stock <- DBI::dbGetQuery(
      con,
      "SELECT i.quantity, i.unit, g.accession_name, i.storage_location
       FROM inventory i
       JOIN germplasm g ON i.germplasm_id = g.germplasm_id
       WHERE i.inventory_id = ?",
      params = list(inventory_id)
    )

    if (nrow(current_stock) == 0) {
      stop("Inventory lot not found. It may have been moved or deleted.")
    }

    # Check if the units are compatible (can't withdraw grams from 'packets')
    if (tolower(current_stock$unit) != "g" && tolower(current_stock$unit) != "kg") {
      stop(paste("Cannot withdraw by weight from a lot measured in", current_stock$unit))
    }

    # Update the quantity
    update_query <- "
      UPDATE inventory
      SET quantity = quantity - ?
      WHERE inventory_id = ? AND quantity >= ?;
    "
    res <- DBI::dbExecute(
      con,
      update_query,
      params = list(amount_in_grams, inventory_id, amount_in_grams)
    )

    if (res == 0) {
      stop("Transaction failed: Insufficient seed quantity in the selected lot.")
    }

    # Log transaction
    details_list <- list(
      type = "WITHDRAWAL",
      accession = current_stock$accession_name,
      lot_id = inventory_id,
      quantity_before = current_stock$quantity,
      quantity_changed = -amount_in_grams,
      quantity_after = current_stock$quantity - amount_in_grams,
      unit = "g", # Standardized to grams
      reason = clean_text(reason)
    )
    details_json <- jsonlite::toJSON(details_list, auto_unbox = TRUE)
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log (table_name, record_id, action, user_name, details) VALUES (?, ?, ?, ?, ?)",
      params = list("inventory", inventory_id, "UPDATE", user_name_clean, details_json)
    )
  })
  
  message(sprintf(
    "Successfully withdrew %s %s of '%s'. Inventory updated.",
    withdraw_amount,
    withdraw_unit,
    current_stock$accession_name
  ))
  invisible(TRUE)
}

#' @title Search and Filter Germplasm
#' @description Dynamically queries the germplasm table based on user filters.
#' @param db_path Path to the SQLite database.
#' @param target_accession Specific accession to search for (optional).
#' @param bio_status Biological status (e.g., "Landrace"). Use "All" to skip.
#' @param acc_type Accession type (e.g., "Advanced Line"). Use "All" to skip.
#' @param source_cat General seed source (e.g., "Gene Bank"). Use "All" to skip.
#' @param origin_country Country of origin. Use "All" to skip.
#' @return A data frame of matching germplasm records.
#' @export
search_germplasm <- function(
  db_path = "data/breeding_db.sqlite",
  target_accession = NULL,
  bio_status = "All",
  acc_type = "All",
  source_cat = "All",
  origin_country = "All"
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Base query selecting the most important passport fields
  query <- "
    SELECT 
      accession_name AS 'Accession',
      species AS 'Species',
      pedigree AS 'Pedigree',
      generation AS 'Generation',
      biological_status AS 'Bio Status',
      accession_type AS 'Type',
      seed_source AS 'Source',
      country_of_origin AS 'Origin',
      collection_site AS 'Collection Site',
      status AS 'Status'
    FROM germplasm
    WHERE 1=1
  "
  
  params <- list()
  
  # Dynamically append filters if the user selected them
  if (!is.null(target_accession) && target_accession != "") {
    query <- paste0(query, " AND accession_name LIKE ?")
    params <- append(params, paste0("%", toupper(trimws(target_accession)), "%"))
  }
  
  if (bio_status != "All") {
    query <- paste0(query, " AND biological_status = ?")
    params <- append(params, trimws(bio_status))
  }
  
  if (acc_type != "All") {
    query <- paste0(query, " AND accession_type = ?")
    params <- append(params, trimws(acc_type))
  }
  
  if (source_cat != "All") {
    query <- paste0(query, " AND seed_source = ?")
    params <- append(params, trimws(source_cat))
  }
  
  if (origin_country != "All") {
    query <- paste0(query, " AND country_of_origin = ?")
    params <- append(params, trimws(origin_country))
  }
  
  query <- paste0(query, " ORDER BY accession_name ASC")
  
  result <- DBI::dbGetQuery(con, query, params = params)
  return(result)
}

#' @title Search and Filter Field Plots
#' @description Dynamically queries the plots table based on user filters.
#' @param db_path Path to the SQLite database.
#' @param search_trial Free text search for Trial Name (optional).
#' @param search_acc Free text search for Accession Name (optional).
#' @param rep_num Replication number. Use "All" to skip.
#' @return A data frame of matching plot records.
#' @export
search_plots <- function(
  db_path = "data/breeding_db.sqlite",
  search_trial = NULL,
  search_acc = NULL,
  rep_num = "All"
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Base query joining plots, trials, and germplasm
  query <- "
    SELECT 
      tr.trial_name AS 'Trial Name',
      p.plot_number AS 'Plot Number',
      g.accession_name AS 'Accession',
      p.replication AS 'Rep',
      p.block AS 'Block',
      p.row AS 'Row',
      p.column AS 'Column',
      p.remarks AS 'Remarks'
    FROM plots p
    JOIN trials tr ON p.trial_id = tr.trial_id
    JOIN germplasm g ON p.germplasm_id = g.germplasm_id
    WHERE 1=1
  "
  
  params <- list()
  
  # Dynamically append filters
  if (!is.null(search_trial) && search_trial != "") {
    query <- paste0(query, " AND tr.trial_name LIKE ?")
    params <- append(params, paste0("%", trimws(search_trial), "%"))
  }
  
  if (!is.null(search_acc) && search_acc != "") {
    query <- paste0(query, " AND g.accession_name LIKE ?")
    params <- append(params, paste0("%", toupper(trimws(search_acc)), "%"))
  }
  
  if (rep_num != "All") {
    query <- paste0(query, " AND p.replication = ?")
    params <- append(params, as.integer(rep_num))
  }
  
  # Order logically by Trial, then Replication, then Plot Number
  query <- paste0(query, " ORDER BY tr.trial_name ASC, p.replication ASC, p.plot_number ASC")
  
  result <- DBI::dbGetQuery(con, query, params = params)
  return(result)
}

#' @title Search and Filter Trials
#' @description Dynamically queries the trials table based on user filters.
#' @param db_path Path to the SQLite database.
#' @param search_text Free text search for Trial Name or Code (optional).
#' @param trial_year The year of the trial. Use "All" to skip.
#' @param trial_season The season (e.g., "Major", "Minor"). Use "All" to skip.
#' @param t_status The status (e.g., "Active", "Completed"). Use "All" to skip.
#' @param t_type The type of trial (e.g., "Yield Trial"). Use "All" to skip.
#' @param pi_name The Principal Investigator. Use "All" to skip.
#' @return A data frame of matching trial records.
#' @export
search_trials <- function(
  db_path = "data/breeding_db.sqlite",
  search_text = NULL,
  trial_year = "All",
  trial_season = "All",
  t_status = "All",
  t_type = "All",
  pi_name = "All"
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Base query selecting key experimental context
  query <- "
    SELECT 
      trial_name AS 'Trial Name',
      trial_code AS 'Code',
      trial_type AS 'Type',
      objective AS 'Objective',
      location AS 'Location',
      year AS 'Year',
      season AS 'Season',
      experimental_design AS 'Design',
      principal_investigator AS 'PI',
      trial_status AS 'Status'
    FROM trials
    WHERE 1=1
  "
  
  params <- list()
  
  # Dynamically append filters
  if (!is.null(search_text) && search_text != "") {
    query <- paste0(query, " AND (trial_name LIKE ? OR trial_code LIKE ?)")
    search_pattern <- paste0("%", trimws(search_text), "%")
    params <- append(params, list(search_pattern, search_pattern))
  }
  
  if (trial_year != "All") {
    query <- paste0(query, " AND year = ?")
    params <- append(params, as.integer(trial_year))
  }
  
  if (trial_season != "All") {
    query <- paste0(query, " AND season = ?")
    params <- append(params, trimws(trial_season))
  }
  
  if (t_status != "All") {
    query <- paste0(query, " AND trial_status = ?")
    params <- append(params, trimws(t_status))
  }
  
  if (t_type != "All") {
    query <- paste0(query, " AND trial_type = ?")
    params <- append(params, trimws(t_type))
  }
  
  if (pi_name != "All") {
    query <- paste0(query, " AND principal_investigator LIKE ?")
    params <- append(params, paste0("%", trimws(pi_name), "%"))
  }
  
  query <- paste0(query, " ORDER BY year DESC, trial_name ASC")
  
  result <- DBI::dbGetQuery(con, query, params = params)
  return(result)
}

#' @title Search and Filter Seed Inventory
#' @description Dynamically queries the physical seed inventory.
#' @param db_path Path to the SQLite database.
#' @param target_accession Specific accession to search for (optional).
#' @param storage_loc Storage location (e.g., "Cold_Room_Shelf_A"). Use "All" to skip.
#' @param seed_stat Status (e.g., "Available", "Exhausted"). Use "All" to skip.
#' @param seed_src Source type (e.g., "Harvest"). Use "All" to skip.
#' @param low_stock_threshold Numeric. If provided, only returns stock < threshold.
#' @return A data frame of matching inventory records.
#' @export
search_inventory <- function(
  db_path = "data/breeding_db.sqlite",
  target_accession = NULL,
  storage_loc = "All",
  seed_stat = "All",
  seed_src = "All",
  low_stock_threshold = NULL
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Base query joining inventory and germplasm
  query <- "
    SELECT 
      g.accession_name AS 'Accession',
      i.quantity AS 'Quantity',
      i.unit AS 'Unit',
      i.storage_location AS 'Location',
      i.container AS 'Container',
      i.seed_status AS 'Status',
      i.source_type AS 'Source',
      i.viability_percent AS 'Viability (%)',
      i.deposit_date AS 'Deposit Date'
    FROM inventory i
    JOIN germplasm g ON i.germplasm_id = g.germplasm_id
    WHERE 1=1
  "
  
  params <- list()
  
  # Dynamically append filters
  if (!is.null(target_accession) && target_accession != "") {
    query <- paste0(query, " AND g.accession_name LIKE ?")
    params <- append(params, paste0("%", toupper(trimws(target_accession)), "%"))
  }
  
  if (storage_loc != "All") {
    query <- paste0(query, " AND i.storage_location = ?")
    params <- append(params, trimws(storage_loc))
  }
  
  if (seed_stat != "All") {
    query <- paste0(query, " AND i.seed_status = ?")
    params <- append(params, trimws(seed_stat))
  }
  
  if (seed_src != "All") {
    query <- paste0(query, " AND i.source_type = ?")
    params <- append(params, trimws(seed_src))
  }
  
  if (!is.null(low_stock_threshold) && is.numeric(low_stock_threshold)) {
    query <- paste0(query, " AND i.quantity < ? AND i.unit IN ('g', 'grams')")
    params <- append(params, low_stock_threshold)
  }
  
  query <- paste0(query, " ORDER BY g.accession_name ASC, i.quantity DESC")
  
  result <- DBI::dbGetQuery(con, query, params = params)
  return(result)
}