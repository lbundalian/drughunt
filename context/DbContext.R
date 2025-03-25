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
                         self$conn <- DBI::dbConnect(RSQLite::SQLite(), self$db_path)
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
                         result <- unlist(strsplit(result, ","))

                         # Trim any leading/trailing whitespace from each element
                         result <- trimws(result)
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
                         # query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"DRUG_TARGET\" = '%s'", target)
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"DRUG_TARGET\" LIKE '%%%s%%'", target)
                         # query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL) AND \"DRUG_TARGET\" ~* E'\\\\m%s\\\\M'", target)
                         
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
                       },
                       find_mutation = function(drugs= NULL, penalty = 1) {
                         self$connect_db()
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL)")
                         
                         query <- "SELECT *,
                                    CASE
                                      WHEN FEATURE_NAME NOT LIKE '%PANCAN%' THEN FEATURE_NAME
                                      WHEN DRUG_TARGET IS NOT NULL THEN DRUG_TARGET
                                      
                                      ELSE NULL
                                    END AS CANCER_FEATURE
                                    FROM ANOVA_DATA
                                    WHERE (FEATURE_NAME NOT LIKE '%PANCAN%' OR DRUG_TARGET IS NOT NULL)"
                         result <- dbGetQuery(self$conn, query)
                         data_filtered <- result %>%
                           filter((!grepl("PANCAN", FEATURE_NAME)) | (!is.na(DRUG_TARGET)))
                         
                         
                         if(length(drugs) > 0){
                           data_filtered <- data_filtered %>% filter(DRUG_NAME %in% drugs)
                         } 
                         
                         
                         data_filtered <- data_filtered %>%
                           mutate(SIGNED_EFFECT_SIZE = ifelse(FEATURE_DELTA_MEAN_IC50 != 0,
                                                              IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
                                                              0))                         
                         
                         result <- data_filtered %>% mutate(RANK_SCORE = SIGNED_EFFECT_SIZE * -log10(FEATURE_PVAL+1e-6))
                         # 
                         
                         result <- data_filtered %>% 
                           # Create the computed score
                           mutate(RANK_SCORE = SIGNED_EFFECT_SIZE * -log10(FEATURE_PVAL + 1e-6)) %>% 
                           # Group by CANCER_FEATURE and average numeric values over duplicates
                           group_by(CANCER_FEATURE) %>%
                           summarise(
                             SIGNED_EFFECT_SIZE = mean(SIGNED_EFFECT_SIZE, na.rm = TRUE),
                             FEATURE_PVAL = mean(FEATURE_PVAL, na.rm = TRUE),
                             RANK_SCORE = mean(RANK_SCORE, na.rm = TRUE),
                             # If you have additional columns (for example mutation_profile), decide how to summarize them.
                             CANCER_FEATURE = first(CANCER_FEATURE)
                           ) %>%
                           ungroup() %>%
                           # Compute the rank (using average method for ties)
                           mutate(rank = rank(RANK_SCORE, ties.method = "average"))
                         
                         result <- result %>% arrange(RANK_SCORE) %>% head(15)
                         
                         self$disconnect_db()
                         return(result)
                         
                       },
                       
                       find_best_drug = function(feature= NULL, target=NULL,penalty = 1) {
                         self$connect_db()
                         ### querying the feature
                         query <- sprintf("SELECT * FROM ANOVA_DATA WHERE (\"FEATURE_NAME\" NOT LIKE '%%PANCAN%%' OR \"DRUG_TARGET\" IS NOT NULL)")
                         print(feature)
                         result <- dbGetQuery(self$conn, query)
                         data_filtered <- result %>%
                           filter((!grepl("PANCAN", FEATURE_NAME)) | (!is.na(DRUG_TARGET)))
                         
                         # data_filtered$DRUG_TARGET <- strsplit(data_filtered$DRUG_TARGET, ",")
                         # data_filtered$DRUG_TARGET <- lapply(data_filtered$DRUG_TARGET, trimws)
                         # 
                         # Define a pattern that matches the feature as a whole word.
                         # (^|[^[:alnum:]]) ensures the feature is either at the beginning
                         # or preceded by a non-alphanumeric character,
                         # and ([^[:alnum:]]|$) ensures it is followed by a non-alphanumeric character or is at the end.
                         
                         
                         # Filter data for rows where FEATURE_NAME or DRUG_TARGET matches the pattern.
                         
                         # data_filtered <- data_filtered %>%
                         #   filter(grepl(pattern, FEATURE_NAME, perl = TRUE) | grepl(pattern, DRUG_TARGET, perl = TRUE))
                                                  
                         if(length(feature) > 0){
                           # pattern <- paste0("(^|[^[:alnum:]])", feature, "([^[:alnum:]]|$)")
                           # data_filtered <- data_filtered %>%
                           #   filter(grepl(pattern, FEATURE_NAME, perl = TRUE))  
                           data_filtered <- data_filtered %>% filter(FEATURE_NAME %in% feature)
                         } else if (length(target) > 0){
                           pattern <- paste0("(^|[^[:alnum:]])", target, "([^[:alnum:]]|$)")
                           print(pattern)
                           print(data_filtered)
                           data_filtered <- data_filtered %>%
                             filter(grepl(pattern, DRUG_TARGET, perl = TRUE))
                         } 
                         
                         
                         # Compute the signed effect size.
                         # (Using an ifelse to avoid division by zero if FEATURE_DELTA_MEAN_IC50 happens to be zero.)
                         data_filtered <- data_filtered %>%
                           mutate(SIGNED_EFFECT_SIZE = ifelse(FEATURE_DELTA_MEAN_IC50 != 0,
                                                              IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
                                                              0))                         
                         # 
                         # result <- result %>% mutate(
                         #   SIGNED_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)))
                         # write.csv(data_filtered,"proto.csv")
                         ### querying the global resistance
                         query_resistance <- "SELECT DRUG_NAME, 
                              MAX(IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / ABS(FEATURE_DELTA_MEAN_IC50))) AS GLOBAL_RESISTANCE
                               FROM ANOVA_DATA 
                               GROUP BY DRUG_NAME"
                         resistance <- dbGetQuery(self$conn, query_resistance)
                         
                         # Merge the global resistance into the result data based on DRUG_NAME.
                         result <- merge(data_filtered, resistance, by = "DRUG_NAME", all.x = TRUE)
                         result <- result %>% mutate(COMPOSITE_SCORE = ((-SIGNED_EFFECT_SIZE) - penalty * GLOBAL_RESISTANCE) * -log10(FEATURE_PVAL+1e-6))
                         result <- result %>% arrange(desc(COMPOSITE_SCORE)) %>% head(15)
                         result <- data_filtered %>% mutate(RANK_SCORE = SIGNED_EFFECT_SIZE * -log10(FEATURE_PVAL+1e-6))
                         result <- result %>% arrange(RANK_SCORE) %>% head(15)
                         
                         
                         self$disconnect_db()
                         return(result)
                         
                       }
                       
                       
                     )
)
