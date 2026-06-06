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
      pedigree TEXT,
      species TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    );"
    )

    # Trials table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS trials (
      trial_id INTEGER PRIMARY KEY AUTOINCREMENT,
      trial_name TEXT NOT NULL,
      trial_loc TEXT NOT NULL,
      start_date DATE,
      metadata TEXT
    );"
    )

    # Plots table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS plots (
      plot_id INTEGER PRIMARY KEY AUTOINCREMENT,
      trial_id INTEGER,
      germplasm_id INTEGER,
      plot_number INTEGER,
      block INTEGER,
      FOREIGN KEY (trial_id) REFERENCES trials(trial_id),
      FOREIGN KEY (germplasm_id) REFERENCES germplasm(germplasm_id)
    );"
    )

    # Traits table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS traits (
      trait_id INTEGER PRIMARY KEY AUTOINCREMENT,
      trait_name TEXT NOT NULL,
      unit TEXT
    );"
    )

    # Observations table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS observations (
      observation_id INTEGER PRIMARY KEY AUTOINCREMENT,
      plot_id INTEGER,
      trait_id INTEGER,
      value REAL,
      obs_date DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (plot_id) REFERENCES plots(plot_id),
      FOREIGN KEY (trait_id) REFERENCES traits(trait_id)
    );"
    )

    # Inventory table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS inventory (
      inventory_id INTEGER PRIMARY KEY AUTOINCREMENT,
      germplasm_id INTEGER,
      quantity REAL,
      unit TEXT,
      storage_location TEXT,
      FOREIGN KEY (germplasm_id) REFERENCES germplasm(germplasm_id)
    );"
    )

    # Transaction log table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS transaction_log (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      table_name TEXT,
      action_type TEXT,
      user_name TEXT,
      details TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    );"
    )
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
    DBI::dbExecute(con, "DELETE FROM transaction_log;")
    
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
  pedigree,
  species
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "INSERT INTO germplasm (accession_name, pedigree, species) VALUES (?, ?, ?)"

  tryCatch(
    {
      DBI::dbExecute(
        con,
        query,
        params = list(accession_name, pedigree, species)
      )
      message(sprintf("Successfully added germplasm: '%s'", accession_name))
      invisible(TRUE)
    },
    error = function(e) {
      stop(
        "Failed to add germplasm. Ensure accession_name is unique. Error: ",
        e$message
      )
    }
  )
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
#' @return Invisibly returns `TRUE` on success.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Use case: Defining standard traits for yield and disease resistance
#' add_trait(
#'   db_path = tempfile(fileext = ".sqlite"),
#'   trait_name = "Pod_Yield",
#'   unit = "kg/ha"
#' )
#' }
add_trait <- function(db_path = "data/breeding_db.sqlite", trait_name, unit) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "INSERT INTO traits (trait_name, unit) VALUES (?, ?)"
  DBI::dbExecute(con, query, params = list(trait_name, unit))
  invisible(TRUE)
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
#' @return Invisibly returns `TRUE` on success.
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
add_trial <- function(
  db_path = "data/breeding_db.sqlite",
  trial_name,
  trial_loc,
  start_date,
  metadata_list = list()
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Serialize metadata to JSON
  metadata_json <- jsonlite::toJSON(metadata_list, auto_unbox = TRUE)

  query <- "INSERT INTO trials (trial_name, trial_loc, start_date, metadata) VALUES (?, ?, ?, ?)"
  DBI::dbExecute(
    con,
    query,
    params = list(
      trial_name,
      trial_loc,
      as.character(start_date),
      metadata_json
    )
  )
  invisible(TRUE)
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
  block
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Get trial ID
  t_res <- DBI::dbGetQuery(
    con,
    "SELECT trial_id FROM trials WHERE trial_name = ?",
    params = list(trial_name)
  )
  if (nrow(t_res) == 0) {
    stop("Trial name not found in the database.")
  }
  trial_id <- t_res$trial_id[1]

  # Get germplasm ID
  g_res <- DBI::dbGetQuery(
    con,
    "SELECT germplasm_id FROM germplasm WHERE accession_name = ?",
    params = list(accession_name)
  )
  if (nrow(g_res) == 0) {
    stop("Accession name not found in the database.")
  }
  germplasm_id <- g_res$germplasm_id[1]

  query <- "INSERT INTO plots (trial_id, germplasm_id, plot_number, block) VALUES (?, ?, ?, ?)"
  DBI::dbExecute(
    con,
    query,
    params = list(trial_id, germplasm_id, plot_number, block)
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
  user_name
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
  p_res <- DBI::dbGetQuery(con, p_query, params = list(trial_name, plot_number))
  if (nrow(p_res) == 0) {
    stop("Plot number not found for that trial.")
  }
  plot_id <- p_res$plot_id[1]
  acc_name <- p_res$accession_name[1]

  # Get trait ID
  t_res <- DBI::dbGetQuery(
    con,
    "SELECT trait_id FROM traits WHERE trait_name = ?",
    params = list(trait_name)
  )
  if (nrow(t_res) == 0) {
    stop("Trait name not found in the database.")
  }
  trait_id <- t_res$trait_id[1]

  DBI::dbWithTransaction(con, {
    # Insert record
    DBI::dbExecute(
      con,
      "INSERT INTO observations (plot_id, trait_id, value) VALUES (?, ?, ?)",
      params = list(plot_id, trait_id, value)
    )

    # Format log details
    log_details <- sprintf(
      "Recorded %s: %s for Plot %s (%s)",
      trait_name,
      value,
      plot_number,
      acc_name
    )
    DBI::dbExecute(
      con,
      "INSERT INTO transaction_log (table_name, action_type, user_name, details) VALUES (?, ?, ?, ?)",
      params = list("observations", "INSERT", user_name, log_details)
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
add_inventory_deposit <- function(
  db_path = "data/breeding_db.sqlite",
  accession_name,
  amount_grams,
  storage_location,
  user_name,
  reason
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Get germplasm ID
  g_res <- DBI::dbGetQuery(
    con,
    "SELECT germplasm_id FROM germplasm WHERE accession_name = ?",
    params = list(accession_name)
  )
  if (nrow(g_res) == 0) {
    stop("Accession name not found in the database.")
  }
  germplasm_id <- g_res$germplasm_id[1]

  DBI::dbWithTransaction(con, {
    check_query <- "SELECT quantity FROM inventory WHERE germplasm_id = ?"
    current_stock <- DBI::dbGetQuery(
      con,
      check_query,
      params = list(germplasm_id)
    )

    if (nrow(current_stock) == 0) {
      insert_inv <- "INSERT INTO inventory (germplasm_id, quantity, unit, storage_location) VALUES (?, ?, 'grams', ?)"
      DBI::dbExecute(
        con,
        insert_inv,
        params = list(germplasm_id, amount_grams, storage_location)
      )
    } else {
      update_inv <- "UPDATE inventory SET quantity = quantity + ? WHERE germplasm_id = ?"
      DBI::dbExecute(con, update_inv, params = list(amount_grams, germplasm_id))
    }

    log_query <- "INSERT INTO transaction_log (table_name, action_type, user_name, details) VALUES (?, ?, ?, ?)"
    details <- sprintf(
      "DEPOSIT: %s grams of %s. Reason: %s",
      amount_grams,
      accession_name,
      reason
    )
    DBI::dbExecute(
      con,
      log_query,
      params = list("inventory", "DEPOSIT", user_name, details)
    )
  })

  message(sprintf(
    "Successfully deposited %s grams of '%s' into inventory.",
    amount_grams,
    accession_name
  ))
  invisible(TRUE)
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
      COALESCE(i.quantity, 0) AS quantity, 
      COALESCE(i.unit, 'grams') AS unit, 
      COALESCE(i.storage_location, 'Not Deposited') AS storage_location
    FROM germplasm g
    LEFT JOIN inventory i ON g.germplasm_id = i.germplasm_id
  "

  if (!is.null(target_accession)) {
    query <- paste0(query, " WHERE g.accession_name = ?")
    result <- DBI::dbGetQuery(con, query, params = list(target_accession))
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
      g.pedigree,
      tr.trial_name,
      tr.trial_loc,
      t.trait_name,
      t.unit,
      AVG(o.value) as value
    FROM germplasm g
    JOIN plots p ON g.germplasm_id = p.germplasm_id
    JOIN trials tr ON p.trial_id = tr.trial_id
    JOIN observations o ON p.plot_id = o.plot_id
    JOIN traits t ON o.trait_id = t.trait_id
    WHERE g.accession_name = ?
    GROUP BY 
      g.accession_name, 
      g.species, 
      g.pedigree, 
      tr.trial_name, 
      tr.trial_loc, 
      t.trait_name, 
      t.unit
    ORDER BY tr.start_date DESC
  "

  result <- DBI::dbGetQuery(con, query, params = list(target_accession))
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
      tr.trial_loc,
      tr.start_date,
      tr.metadata,
      p.plot_number,
      p.block,
      g.accession_name,
      t.trait_name,
      o.value
    FROM trials tr
    JOIN plots p ON tr.trial_id = p.trial_id
    JOIN germplasm g ON p.germplasm_id = g.germplasm_id
    JOIN observations o ON p.plot_id = o.plot_id
    JOIN traits t ON o.trait_id = t.trait_id
    WHERE tr.trial_name = ?
  "

  raw_data <- DBI::dbGetQuery(con, query, params = list(target_trial_name))

  if (nrow(raw_data) == 0) {
    message("No data found for the specified trial.")
    return(NULL)
  }

  # Parse JSON metadata
  metadata_list <- tryCatch(
    {
      jsonlite::fromJSON(raw_data$metadata[1])
    },
    error = function(e) list()
  )

  # Subset observation columns
  obs_df <- raw_data[, c(
    "plot_number",
    "block",
    "accession_name",
    "trait_name",
    "value"
  )]

  # Pivot to wide format
  if (wide_format) {
    obs_df <- reshape(
      obs_df,
      idvar = c("plot_number", "block", "accession_name"),
      timevar = "trait_name",
      v.names = "value",
      direction = "wide"
    )
    # Clean column prefixes
    names(obs_df) <- gsub("^value\\.", "", names(obs_df))
  }

  # Return list
  list(
    trial_info = list(
      name = raw_data$trial_name[1],
      location = raw_data$trial_loc[1],
      date = raw_data$start_date[1],
      custom_parameters = metadata_list
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
      g.accession_name,
      t.trait_name,
      o.value
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
    params <- append(params, trial_name)
  }

  if (!is.null(accession_name)) {
    query <- paste0(query, " AND g.accession_name = ?")
    params <- append(params, accession_name)
  }

  raw_data <- DBI::dbGetQuery(con, query, params = params)

  if (nrow(raw_data) == 0) {
    message("No field data found for those parameters.")
    return(list())
  }

  # Pivot to wide format
  wide_data <- reshape(
    raw_data,
    idvar = c("trial_name", "plot_number", "block", "accession_name"),
    timevar = "trait_name",
    v.names = "value",
    direction = "wide"
  )
  names(wide_data) <- gsub("^value\\.", "", names(wide_data))

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
      timestamp, 
      table_name, 
      action_type, 
      user_name, 
      details
    FROM transaction_log
    ORDER BY timestamp DESC
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
#'   withdraw_amount = 75,
#'   withdraw_unit = "grams",
#'   user_name = "Israel Tetteh",
#'   reason = "Setup for drought stress block"
#' )
#' }
withdraw_seed <- function(
  db_path = "data/breeding_db.sqlite",
  accession_name,
  withdraw_amount,
  withdraw_unit,
  user_name,
  reason
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  g_res <- DBI::dbGetQuery(
    con,
    "SELECT germplasm_id FROM germplasm WHERE accession_name = ?",
    params = list(accession_name)
  )

  if (nrow(g_res) == 0) {
    stop("Accession name not found in the database. Please check the spelling.")
  }
  germplasm_id <- g_res$germplasm_id[1]

  unit_lower <- tolower(withdraw_unit)
  amount_in_grams <- if (unit_lower %in% c("kg", "kilogram", "kilograms")) {
    withdraw_amount * 1000
  } else {
    withdraw_amount
  }

  DBI::dbWithTransaction(con, {
    # Subtract stock if sufficient
    update_query <- "
      UPDATE inventory
      SET quantity = quantity - ?
      WHERE germplasm_id = ? AND quantity >= ?;
    "
    res <- DBI::dbExecute(
      con,
      update_query,
      params = list(amount_in_grams, germplasm_id, amount_in_grams)
    )

    # Check if update succeeded
    if (res == 0) {
      stop(
        "Transaction failed: Insufficient seed quantity in the freezer or seed not deposited yet."
      )
    }

    # Log transaction
    log_query <- "
      INSERT INTO transaction_log (table_name, action_type, user_name, details)
      VALUES (?, ?, ?, ?)
    "
    details <- sprintf(
      "WITHDRAWAL: %s %s of %s. Converted to %s g. Reason: %s",
      withdraw_amount,
      withdraw_unit,
      accession_name,
      amount_in_grams,
      reason
    )

    DBI::dbExecute(
      con,
      log_query,
      params = list("inventory", "WITHDRAWAL", user_name, details)
    )
  })

  message(sprintf(
    "Successfully withdrew %s %s of '%s'. Inventory updated.",
    withdraw_amount,
    withdraw_unit,
    accession_name
  ))
  invisible(TRUE)
}
