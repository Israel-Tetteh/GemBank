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
#' init_db(db_path = tempfile(fileext = ".sqlite"))
#' }
init_db <- function(db_path = "data/breeding_db.sqlite") {
  # Ensure the directory exists
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  # Connect to the database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  #  Close database afterwards
  on.exit(DBI::dbDisconnect(con))

  #  Set foreign Keys
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # Database Schema
  DBI::dbWithTransaction(con, {
    # GERMPLASM TABLE / PASSPORT TABLE
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

    # TRIAL TABLE
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS trials (
      trial_id INTEGER PRIMARY KEY AUTOINCREMENT,
      trial_name TEXT NOT NULL,
      start_date DATE,
      metadata TEXT
    );"
    )

    # PLOT TABLE
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

    # TRAITS TABLE
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS traits (
      trait_id INTEGER PRIMARY KEY AUTOINCREMENT,
      trait_name TEXT NOT NULL,
      unit TEXT
    );"
    )

    # OBSERVATION TABLE
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

    # INVENTORY TABLE
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

    # TRANSACTION LOG TABLE
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS transaction_log (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      table_name TEXT,
      action_type TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    );"
    )
  })

  message("Database initialized successfully at: ", db_path)
}




#' Clear All Data from the Breeding Database
#'
#' @param db_path Path to the sqlite file. Defaults to `"data/breeding_db.sqlite"`.
#' @export
clear_db <- function(db_path = "data/breeding_db.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Enable foreign key support
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbWithTransaction(con, {
    # Delete data in reverse dependency order
    DBI::dbExecute(con, "DELETE FROM observations;")
    DBI::dbExecute(con, "DELETE FROM inventory;")
    DBI::dbExecute(con, "DELETE FROM plots;")
    DBI::dbExecute(con, "DELETE FROM traits;")
    DBI::dbExecute(con, "DELETE FROM trials;")
    DBI::dbExecute(con, "DELETE FROM germplasm;")
    DBI::dbExecute(con, "DELETE FROM transaction_log;")

    # Reset AUTOINCREMENT counters so new entries start at ID 1
    DBI::dbExecute(con, "DELETE FROM sqlite_sequence;")
  })

  message("All data has been successfully deleted from the database.")
}




library(DBI)
library(RSQLite)

#' Create Tables from Vector Schema
#'
#' @param db_path Path to the SQLite database file (e.g., "data/germbank.sqlite")
#' @param structure_vec A vector defining the schema:
#'   First element = number of tables
#'   Then for each table:
#'     - table name
#'     - number of columns
#'     - column names
#' @export
create_tables_from_vector <- function(db_path, structure_vec) {
  # Connect to the database file
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))

  pos <- 1
  num_tables <- as.integer(structure_vec[pos]); pos <- pos + 1

  for (i in seq_len(num_tables)) {
    # Table name
    table_name <- structure_vec[pos]; pos <- pos + 1

    # Number of columns
    num_cols <- as.integer(structure_vec[pos]); pos <- pos + 1

    # Column names
    cols <- structure_vec[pos:(pos + num_cols - 1)]
    pos <- pos + num_cols

    # Build SQL (defaulting all columns to TEXT)
    col_defs <- paste(cols, "TEXT", collapse = ", ")
    sql <- sprintf("CREATE TABLE IF NOT EXISTS %s (%s)", table_name, col_defs)

    dbExecute(con, sql)
    message(sprintf("Table '%s' created with columns: %s", table_name, paste(cols, collapse = ", ")))
  }

  message("Tables created successfully in database: ", db_path)
}


   # Sample Data
db_structure <- c(
  2,                           # number of tables
  "germplasm_storage",         # table 1 name
  4, "storage_id", "germplasm_id", "location", "quantity",  # table 1 columns
  "transaction_log",           # table 2 name
  3, "log_id", "action_type", "timestamp"                   # table 2 columns
)


create_tables_from_vector("data/germbank.sqlite", db_structure)

# Check tables
con <- dbConnect(RSQLite::SQLite(), "data/germbank.sqlite")
dbListTables(con)

con <- DBI::dbConnect(RSQLite::SQLite(), "data/germbank.sqlite")

# Drop unwanted tables
DBI::dbExecute(con, "DROP TABLE IF EXISTS students;")
DBI::dbExecute(con, "DROP TABLE IF EXISTS courses;")

tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")$name

for (tbl in tables) {
  cat("\nTable:", tbl, "\n")
  print(dbGetQuery(con, sprintf("PRAGMA table_info(%s);", tbl)))
}









