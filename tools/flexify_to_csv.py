import sqlite3
import csv
import os
import argparse
from datetime import datetime
from loguru import logger

# --- CSV Header ---
CSV_HEADER = [
    "id", "name", "reps", "weight", "created", "unit",
    "hidden", "image", "sets", "minutes", "seconds", "steps"
]

# --- Helper Function ---
def format_timestamp(unix_timestamp):
    """Converts a Unix timestamp (seconds) to ISO 8601 format."""
    if unix_timestamp is None:
        return ""
    try:
        dt_object = datetime.fromtimestamp(unix_timestamp)
        return dt_object.strftime('%Y-%m-%dT%H:%M:%S')
    except (ValueError, TypeError):
        logger.warning(f"Could not parse timestamp {unix_timestamp}")
        return ""

# --- Main Data Processing Function ---
def export_flexify_data(db_path, sets_output_path, bw_output_path):
    """
    Connects to the Flexify database, extracts gym_sets data,
    and splits it into workout sets and bodyweight CSV files,
    filtering out template entries.
    """
    conn = None
    skipped_template_count = 0 # Counter for skipped rows
    try:
        # Ensure the output directory exists
        output_dir = os.path.dirname(sets_output_path)
        if output_dir:
             os.makedirs(output_dir, exist_ok=True)
             logger.info(f"Ensured output directory exists: {output_dir}")
        else:
             logger.info("Outputting to current directory.")


        # Connect to the database
        logger.info(f"Connecting to database: {db_path}")
        if not os.path.exists(db_path):
            logger.error(f"Database file not found at {db_path}")
            raise FileNotFoundError(f"Database file not found at {db_path}")

        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        logger.info("Database connection established.")

        # Select relevant columns from the gym_sets table
        cursor.execute("""
            SELECT
                id, name, reps, weight, body_weight, created, unit, hidden, image,
                cardio, duration, distance, notes, rest_ms
            FROM gym_sets
            ORDER BY created
        """)

        rows = cursor.fetchall()
        total_rows_read = len(rows)
        logger.info(f"Read {total_rows_read} total rows from gym_sets table.")

        # Prepare data lists for CSV writing
        workout_rows = []
        bw_rows = []

        # Process each row
        for row_idx, row in enumerate(rows):
            # Map columns by index based on the SELECT statement
            row_data = {
                "id": row[0], "name": row[1], "reps": row[2],
                "workout_weight": row[3], "body_weight": row[4], "created": row[5],
                "unit": row[6], "hidden": row[7], "image": row[8],
                "cardio": row[9], "duration": row[10], "distance": row[11],
                "notes": row[12], "rest_ms": row[13]
            }

            is_bodyweight_entry = (row_data["name"] == "Weight")

            # Prepare the common part of the CSV row data
            csv_row_dict = {}
            for header in CSV_HEADER:
                value = None
                if header == "id": value = row_data.get("id")
                elif header == "name": value = row_data.get("name")
                elif header == "reps": value = row_data.get("reps")
                elif header == "weight":
                    value = row_data.get("body_weight") if is_bodyweight_entry else row_data.get("workout_weight")
                elif header == "created": value = format_timestamp(row_data.get("created"))
                elif header == "unit": value = row_data.get("unit")
                elif header == "hidden": value = str(row_data.get("hidden", 0))
                elif header == "image": value = row_data.get("image")
                elif header == "sets": value = ""
                elif header == "minutes": value = ""
                elif header == "seconds": value = ""
                elif header == "steps": value = ""
                csv_row_dict[header] = value if value is not None else ""

            # Convert dict to list in correct order
            csv_row = [csv_row_dict[h] for h in CSV_HEADER]

            # Append to the correct list, applying the filter for workout sets
            if is_bodyweight_entry:
                bw_rows.append(csv_row)
            else:
                # *** FILTER ADDED HERE ***
                # Only include workout sets with more than 0 reps
                reps_value = row_data.get("reps", 0) # Get reps, default to 0 if missing
                # Ensure reps_value is treated as a number for comparison
                try:
                    numeric_reps = float(reps_value) if reps_value is not None else 0.0
                except (ValueError, TypeError):
                    numeric_reps = 0.0
                    logger.warning(f"Could not convert reps '{reps_value}' to number for row ID {row_data.get('id')}. Treating as 0.")

                if numeric_reps > 0:
                    workout_rows.append(csv_row)
                else:
                    skipped_template_count += 1
                    # Log skipped rows at DEBUG level if needed
                    logger.trace(f"Skipping template/zero-rep row ID {row_data.get('id')}: name='{row_data.get('name')}', reps={reps_value}")


        # Log summary of skipped rows
        if skipped_template_count > 0:
            logger.info(f"Skipped {skipped_template_count} rows likely representing templates (reps <= 0).")

        # Write workout data to sets.csv
        logger.info(f"Writing {len(workout_rows)} workout rows to {sets_output_path}")
        with open(sets_output_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
            writer.writerow(CSV_HEADER)
            writer.writerows(workout_rows)

        # Write bodyweight data to bw.csv
        logger.info(f"Writing {len(bw_rows)} bodyweight rows to {bw_output_path}")
        with open(bw_output_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
            writer.writerow(CSV_HEADER)
            writer.writerows(bw_rows)

        logger.success("Export complete.")

    except FileNotFoundError as e:
        logger.error(f"File system error: {e}")
    except sqlite3.Error as e:
        logger.error(f"Database error: {e}")
    except Exception as e:
        logger.exception(f"An unexpected error occurred: {e}")
    finally:
        if conn:
            conn.close()
            logger.info("Database connection closed.")

# --- Argument Parsing and Execution ---
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Extract workout sets and bodyweight data from a Flexify SQLite database into separate CSV files."
    )
    parser.add_argument(
        "input_db",
        help="Path to the input Flexify SQLite database file (e.g., data/flexify.sqlite)."
    )
    parser.add_argument(
        "-o", "--output-dir",
        help="Directory where the output CSV files (sets.csv, bw.csv) will be saved. "
             "Defaults to the directory containing the input database file.",
        default=None
    )

    args = parser.parse_args()

    # Determine the output directory
    if args.output_dir is None:
        output_directory = os.path.dirname(os.path.abspath(args.input_db))
        if not output_directory:
            output_directory = '.'
    else:
        output_directory = args.output_dir

    # Construct full output file paths
    sets_csv_output_path = os.path.join(output_directory, 'sets.csv')
    bw_csv_output_path = os.path.join(output_directory, 'bw.csv')

    logger.info(f"Input Database: {os.path.abspath(args.input_db)}")
    logger.info(f"Output Directory: {os.path.abspath(output_directory)}")
    logger.info(f"Output Sets CSV: {os.path.abspath(sets_csv_output_path)}")
    logger.info(f"Output BW CSV: {os.path.abspath(bw_csv_output_path)}")

    # Run the export process
    export_flexify_data(args.input_db, sets_csv_output_path, bw_csv_output_path)