library(readxl)
library(DBI)
library(RSQLite)

# 1. From .xlsx to .sqlite
excel_file <- "database/SmartflAI 2025 Database Records.xlsx"            
sqlite_file <- "database/SmartflAI_database.sqlite"        
overwrite_tables <- TRUE                    

# 2. DB connection
con <- dbConnect(SQLite(), dbname = sqlite_file)

# 3. Read and process Excel sheet 
sheet_names <- excel_sheets(excel_file)
cat("Found sheets:\n", paste("-", sheet_names, collapse = "\n"), "\n")
for (sheet in sheet_names) {
  cat("\nProcessing sheet:", sheet, "\n")
  
  # Read sheet into a data frame
  df <- read_excel(excel_file, sheet = sheet)
  
  # Clean column names to avoid SQL issues
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
# 4. Define column types
  # Rename columns to standard names 
  names(df) <- tolower(names(df))
  colnames(df)[colnames(df) == "iso.week"] <- "iso_week"
  
  # Coerce types
  df <- df[, c("date", "iso_week", "datapoint", "count")] 
  df$date <- as.character(df$date)
  df$iso_week <- as.integer(df$iso_week)
  df$datapoint <- as.character(df$datapoint)
  df$count <- as.integer(df$count)
  
# 5. Create or Update SQLite DB
  dbWriteTable(con, sheet, df, overwrite = overwrite_tables)
  cat("  → Table", sheet, "written to database with forced column types\n")
}

# 6. Close DB connection (DO NOT IGNORE or DELETE THESE LINES OF CODE)
dbDisconnect(con)
cat("\nAll sheets processed and saved to", sqlite_file, "\n")


