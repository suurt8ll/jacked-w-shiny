#!/usr/bin/env python3

import argparse
import csv
import sqlite3
import sys
import os
import shutil
from datetime import datetime

def parse_arguments():
    """Parses command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Migrate exercise and weight data from CSV into an SQLite database, preserving float precision where possible.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("csv_file", help="Path to the input CSV file.")
    parser.add_argument("input_db", help="Path to the original SQLite database file.")
    parser.add_argument("output_db", help="Path for the new/output SQLite database file.")
    return parser.parse_args()

def safe_int(value, default=0):
    """Safely convert a value to an integer, returning a default if conversion fails or value is None/empty."""
    if value is None or value == '':
        return default
    try:
        # Handle potential floats in integer columns by converting via float first if needed
        return int(float(value))
    except (ValueError, TypeError):
        return default

def safe_float(value, default=0.0):
    """Safely convert a value to a float, returning a default if conversion fails or value is None/empty."""
    if value is None or value == '':
        return default
    try:
        return float(value)
    except (ValueError, TypeError):
        return default

def safe_bool(value, default=False):
    """Safely convert a value to a boolean (recognizing '0', '1', True, False)."""
    if value is None:
        return default
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        if value.lower() in ('true', '1', 'yes', 'y'):
            return True
        if value.lower() in ('false', '0', 'no', 'n', ''):
            return False
    # Try converting to int first for 0/1 cases
    try:
        return bool(int(value))
    except (ValueError, TypeError):
        pass # Fall through if not int-like

    return default # Default if not recognized


def migrate_data(csv_path, input_db_path, output_db_path):
    """Performs the data migration."""

    # 1. Validate input files exist
    if not os.path.exists(csv_path):
        print(f"Error: CSV file not found at '{csv_path}'")
        sys.exit(1)
    if not os.path.exists(input_db_path):
        print(f"Error: Input database not found at '{input_db_path}'")
        sys.exit(1)

    # 2. Copy original DB to output path to preserve existing structure/data
    try:
        print(f"Copying '{input_db_path}' to '{output_db_path}'...")
        shutil.copy2(input_db_path, output_db_path) # copy2 preserves metadata
        print("Copy complete.")
    except Exception as e:
        print(f"Error copying database: {e}")
        sys.exit(1)

    # 3. Connect to the NEW database
    conn = None
    try:
        print(f"Connecting to database '{output_db_path}'...")
        conn = sqlite3.connect(output_db_path)
        # Ensure floats are handled correctly by the Python sqlite3 driver
        conn.execute("PRAGMA foreign_keys = ON;") # Good practice, though maybe not strictly needed here
        cursor = conn.cursor()
        print("Connection successful.")

        # 4. Read CSV and insert data
        print(f"Processing CSV file '{csv_path}'...")
        inserted_sets = 0
        inserted_weights = 0
        skipped_rows = 0

        with open(csv_path, 'r', newline='', encoding='utf-8') as infile:
            reader = csv.DictReader(infile)
            if not reader.fieldnames:
                 print(f"Error: CSV file '{csv_path}' seems empty or has no header.")
                 sys.exit(1)

            # Check for essential columns (id is still required in CSV for differentiation/logging,
            # but not used for DB primary key insertion)
            required_set_cols = ['id', 'name', 'reps', 'weight', 'created']
            required_weight_cols = ['id', 'weight', 'created'] # 'name' is used for differentiation

            # We still check for 'id' in the CSV, but we won't use it for the DB PK
            if not all(col in reader.fieldnames for col in required_set_cols):
                 print(f"Error: CSV missing one or more required columns for sets: {required_set_cols}")
                 sys.exit(1)
            if not all(col in reader.fieldnames for col in required_weight_cols):
                 print(f"Error: CSV missing one or more required columns for weights: {required_weight_cols}")
                 sys.exit(1)


            for i, row in enumerate(reader):
                # We still read the CSV ID, but it's not used for the DB primary key
                csv_row_id = safe_int(row.get('id'), default=None)
                if csv_row_id is None:
                    print(f"Warning: Skipping row {i+1} due to missing or invalid 'id' in CSV.")
                    skipped_rows += 1
                    continue

                try:
                    name = row.get('name', '').strip()
                    created_str = row.get('created', '')
                    # Basic validation for created date format (adjust if needed)
                    try:
                        # Attempt to parse to ensure it's a somewhat valid timestamp string
                        datetime.fromisoformat(created_str.replace(' ', 'T'))
                    except ValueError:
                         print(f"Warning: Skipping row {i+1} (CSV ID: {csv_row_id}) due to invalid 'created' date format: '{created_str}'. Expected ISO format like YYYY-MM-DDTHH:MM:SS.")
                         skipped_rows += 1
                         continue


                    # --- Differentiate between 'sets' and 'weights' ---
                    if name.lower() == 'weight':
                        # --- Process as Body Weight entry ---
                        # Omit 'id' from the INSERT statement to let AUTOINCREMENT generate it
                        sql = """
                            INSERT INTO weights (value, created, unit)
                            VALUES (?, ?, ?);
                        """
                        # Use safe_float to get the value as a float
                        weight_value_float = safe_float(row.get('weight'), 0.0)

                        # Pass the float value directly to the database
                        params = (
                            weight_value_float, # Pass the float value here
                            created_str,
                            row.get('unit') or 'kg' # Default unit 'kg'
                        )
                        cursor.execute(sql, params)
                        inserted_weights += 1

                    else:
                        # --- Process as Exercise Set entry ---
                        # Omit 'id' from the INSERT statement to let AUTOINCREMENT generate it
                        sql = """
                            INSERT INTO sets
                            (name, reps, weight, created, unit, hidden, image, sets, minutes, seconds, steps)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
                        """
                        # Use safe_float for 'weight' column based on likely app behavior
                        set_weight_float = safe_float(row.get('weight'), default=0.0)

                        params = (
                            name,
                            safe_int(row.get('reps'), default=0), # reps is INTEGER
                            set_weight_float, # Pass float for weight
                            created_str,
                            row.get('unit') or 'kg', # Default unit 'kg'
                            safe_bool(row.get('hidden'), default=False), # Default hidden=false
                            row.get('image') or None, # Use None if empty/missing -> NULL
                            safe_int(row.get('sets'), default=3), # sets is INTEGER
                            safe_int(row.get('minutes'), default=3), # minutes is INTEGER
                            safe_int(row.get('seconds'), default=30), # seconds is INTEGER
                            row.get('steps') or None # Use None if empty/missing -> NULL
                        )
                        cursor.execute(sql, params)
                        inserted_sets += 1

                except KeyError as e:
                    print(f"Warning: Skipping row {i+1} (CSV ID: {csv_row_id}) due to missing column: {e}")
                    skipped_rows += 1
                except Exception as e:
                    print(f"Warning: Skipping row {i+1} (CSV ID: {csv_row_id}) due to unexpected error: {e}")
                    skipped_rows += 1

        # 5. Commit changes
        print("Committing changes to the database...")
        conn.commit()
        print("Commit successful.")

        print("\n--- Migration Summary ---")
        print(f"Processed rows: {i + 1}")
        print(f"Inserted into 'sets' (new IDs generated): {inserted_sets}")
        print(f"Inserted into 'weights' (new IDs generated): {inserted_weights}")
        print(f"Skipped rows: {skipped_rows}")
        print(f"Migration complete. Output database saved to '{output_db_path}'")

    except sqlite3.Error as e:
        print(f"\nDatabase error occurred: {e}")
        if conn:
            print("Rolling back changes...")
            conn.rollback()
        sys.exit(1)
    except FileNotFoundError as e:
         # This might catch the CSV file open error if initial check passed but file removed race condition
         print(f"Error: File not found during processing: {e}")
         sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error occurred: {e}")
        if conn:
            print("Rolling back changes...")
            conn.rollback()
        sys.exit(1)
    finally:
        # 6. Close connection
        if conn:
            conn.close()
            print("Database connection closed.")


if __name__ == "__main__":
    args = parse_arguments()
    migrate_data(args.csv_file, args.input_db, args.output_db)