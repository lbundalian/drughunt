import os
import sqlite3
import pandas as pd

# Define database path
DB_PATH = "pancancerdb.sqlite3"
DATA_DIR = "data"

def normalize_column_names(columns):
    """Convert column names to uppercase and replace spaces with underscores."""
    return [col.upper().replace(" ", "_") for col in columns]

def create_database():
    """Create SQLite database from CSV files if it does not already exist."""
    
    if os.path.exists(DB_PATH):
        print("Database already exists. Skipping initialization.")
        return

    print("Database not found. Initializing SQLite database...")
    
    # Connect to SQLite database
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Get all CSV files in the data directory
    csv_files = [f for f in os.listdir(DATA_DIR) if f.endswith(".csv")]

    for file in csv_files:
        # Generate table name (remove "PANCANCER_" prefix)
        table_name = os.path.splitext(file)[0].replace("PANCANCER_", "")

        print(f"Processing {file} -> Creating table: {table_name}")

        # Read CSV into DataFrame
        df = pd.read_csv(os.path.join(DATA_DIR, file))

        # Normalize column names
        df.columns = normalize_column_names(df.columns)

        # Write DataFrame to SQLite
        df.to_sql(table_name, conn, if_exists="replace", index=False)

    print("Database successfully created and populated.")
    
    # Close connection
    conn.close()

# Run database creation
if __name__ == "__main__":
    create_database()
