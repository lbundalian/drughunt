library(R6)
library(DBI)
library(RSQLite)

DbContext <- R6Class("DbContext",
                     public = list(
                       db_path = "data/pancancerdb.sqlite3",
                       data_dir = "data",
                       conn = NULL,
                       
                       initialize = function() {
                         if (!file.exists(self$db_path)) {
                           message("Database not found. Initializing SQLite database...")
                           self$build_db()
                         } else {
                           message("Database already exists. Skipping initialization.")
                         }
                       },
                       
                       build_db = function() {
                         self$connect_db()
                         csv_files <- list.files(self$data_dir, pattern = "\\.csv$", full.names = TRUE)
                         
                         for (csv in csv_files) {
                           original_name <- tools::file_path_sans_ext(basename(csv))
                           table_name <- gsub("^PANCANCER_", "", original_name)  # Remove "PANCANCER_" prefix
                           
                           message(paste("Creating table:", table_name))
                           
                           # Load CSV
                           data <- read.csv(csv, stringsAsFactors = FALSE)
                           
                           # Normalize column names
                           colnames(data) <- toupper(gsub(" ", ".", colnames(data)))
                           
                           # Write table to SQLite
                           dbWriteTable(self$conn, table_name, data, overwrite = TRUE, row.names = FALSE)
                         }
                         
                         message("Database successfully created and populated.")
                         self$disconnect_db()
                       },
                       
                       connect_db = function() {
                         self$conn <- dbConnect(RSQLite::SQLite(), self$db_path)
                         message("✅ Database connection established.")
                       },
                       
                       disconnect_db = function() {
                         if (!is.null(self$conn)) {
                           dbDisconnect(self$conn)
                           self$conn <- NULL
                           message("❌ Database connection closed.")
                         }
                       },
                       
                       get_drug_list = function() {
                         self$connect_db()
                         query <- "SELECT DISTINCT \"DRUG_NAME\" FROM ANOVA_DATA"
                         result <- dbGetQuery(self$conn, query)$DRUG_NAME
                         
                         self$disconnect_db()
                         return(result)
                       },
                       
                       get_target_list = function() {
                         self$connect_db()
                         query <- "SELECT DISTINCT \"DRUG_TARGET\" FROM ANOVA_DATA"
                         result <- dbGetQuery(self$conn, query)$DRUG_TARGET
                         # Split comma-separated entries and flatten the list into a single vector
                         # result <- unlist(strsplit(result, ","))
                         # Trim any leading/trailing whitespace from each element
                         # result <- trimws(result)
                         self$disconnect_db()
                         return(result)
                       },
                       
                       
                       get_feature_list = function() {
                         self$connect_db()
                         query <- "SELECT DISTINCT \"FEATURE_NAME\" FROM ANOVA_DATA WHERE \"FEATURE_NAME\" NOT LIKE '%PANCAN%'"
                         result <- dbGetQuery(self$conn, query)$FEATURE_NAME
                         
                         self$disconnect_db()
                         return(result)
                       },
                       
                       
                       get_drug_data = function(drug) {
                         self$connect_db()
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"DRUG_NAME\" = '%s'", drug)
                         result <- dbGetQuery(self$conn, query)
                         self$disconnect_db()
                         return(result)
                       },
                       
                       get_target_data = function(target) {
                         self$connect_db()
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"DRUG_TARGET\" = '%s'", target)
                         result <- dbGetQuery(self$conn, query)
                         self$disconnect_db()
                         return(result)
                       },
                       
                       get_feature_data = function(feature) {
                         self$connect_db()
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"FEATURE_NAME\" = '%s'", feature)
                         result <- dbGetQuery(self$conn, query)
                         self$disconnect_db()
                         return(result)
                       }
                       
                     )
)
