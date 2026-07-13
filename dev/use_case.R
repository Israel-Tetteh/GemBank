
# Create the database
init_db(db_path = "data/breeding_db.sqlite") # define your own path maybe your desktop

db_file <- "data/breeding_db.sqlite"
current_user <- "Israel Tetteh"

# Functions that allow users write into the database
 #-  Germplasm table
add_germplasm(db_file, "SC-2026-001", "Local-Landrace-A", "Sorghum bicolor")
add_germplasm(db_file, "ICSV-111", "ICSV-111-Reference", "Sorghum bicolor")

  #- Traits table
add_trait(db_file, "Awn_Length", "cm")
add_trait(db_file, "KASP_DAI_Call", "Allele")

  #- Add meta data if needed, here I was basically testing all tables
  #- Trial table
add_trial(
  db_file,
  trial_name = "2026_Sorghum_Awnness_DAI_Validation",
  trial_loc = "KNUST_Agric_Field",
  start_date = "2026-05-10",
  metadata_list = list(
    experimental_design = "Alpha Lattice",
    plot_dimensions = "3m x 0.25m",
    supervisor = "Dr. Alexander Wireko Kena"
  )
)

 # Plots table
add_plot(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  "SC-2026-001",
  plot_number = 101,
  block = 1
)
add_plot(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  "ICSV-111",
  plot_number = 102,
  block = 1
)

  # Observation table
add_observation(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  101,
  "Awn_Length",
  4.2,
  user_name = current_user
)
add_observation(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  101,
  "KASP_DAI_Call",
  1.0,
  user_name = current_user
)

add_observation(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  102,
  "Awn_Length",
  0.0,
  user_name = current_user
)
add_observation(
  db_file,
  "2026_Sorghum_Awnness_DAI_Validation",
  102,
  "KASP_DAI_Call",
  0.0,
  user_name = current_user
)

  # Inventory Table
add_inventory_deposit(
  db_file,
  accession_name = "SC-2026-001",
  amount_grams = 1500,
  storage_location = "Cold_Room_Shelf_A",
  user_name = current_user,
  reason = "Harvest from 2026 DAI Validation Trial"
)

add_inventory_deposit(
  db_file,
  accession_name = "ICSV-111",
  amount_grams = 850,
  storage_location = "Cold_Room_Shelf_A",
  user_name = current_user,
  reason = "Harvest from 2026 DAI Validation Trial"
)



#==========
# Querying from the database.
#==========
# Path to database
db_file <- "data/breeding_db.sqlite"

# Complete Seed Inventory Check
master_inventory <- get_inventory_status(db_path = db_file)
print(master_inventory)

# Low Stock Warning
urgent_multiplication <- get_low_stock(db_path = db_file, threshold = 1000)
print(urgent_multiplication)

# Get passport information about an accession inquestion
sc_passport <- get_germplasm_passport(
  db_path = db_file,
  target_accession = "SC-2026-001"
)
print(sc_passport)


# Extract a specific trial to view its design and  Bulk metadata
active_trial <- get_trial_data(
  db_path = db_file,
  target_trial_name = "2026_Sorghum_Awnness_DAI_Validation",
  wide_format = TRUE
)
print(active_trial)


# Get simplified Meta data
field_book_list <- get_field_book(
  db_path = db_file,
  accession_name = "ICSV-111"
)
print(field_book_list)

# Check transaction logs
recent_logs <- get_audit_ledger(db_path = db_file, limit = 5)
print(recent_logs)


# Withdraw seeds.
 withdraw_seed(
   db_path = db_file,
   accession_name = "SC-2026-001",
   withdraw_amount = 75,
   withdraw_unit = "grams",
   user_name = "Israel Tetteh",
   reason = "Setup for drought stress block"
 )

# Check the audit ledger to see the result
get_audit_ledger(db_path = db_file, limit = 5)

# Check the inventory status
get_inventory_status(db_path = db_file)


# golem::add_module(name = 'mod_inventory_scripts')
