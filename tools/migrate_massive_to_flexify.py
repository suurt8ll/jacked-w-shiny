import sqlite3
import argparse
import shutil
import sys
from datetime import datetime, timezone

def iso_to_seconds(iso_string):
    """Converts an ISO 8601 string to seconds since epoch (UTC)."""
    if not iso_string:
        return None
    try:
        # Parse the ISO string
        # Handle potential 'Z' by replacing with '+00:00' for fromisoformat
        dt = datetime.fromisoformat(iso_string.replace('Z', '+00:00'))
        # Ensure it's timezone-aware (assume UTC if no offset)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        # Convert to UTC timestamp in seconds
        timestamp_seconds = int(dt.timestamp())
        return timestamp_seconds
    except ValueError:
        print(f"Warning: Could not parse date string: {iso_string}", file=sys.stderr)
        return None
    except Exception as e:
        print(f"Warning: Error converting date '{iso_string}': {e}", file=sys.stderr)
        return None

def migrate_data(massive_db_path, flexify_template_path, output_db_path):
    """Migrates workout sets and body weight from Massive DB to a new Flexify DB."""

    print(f"Starting migration...")
    print(f"  Source (Massive): {massive_db_path}")
    print(f"  Template (Flexify): {flexify_template_path}")
    print(f"  Output (New Flexify): {output_db_path}")

    # 1. Copy the template database to the output path
    try:
        shutil.copy2(flexify_template_path, output_db_path)
        print(f"Successfully copied template to {output_db_path}")
    except Exception as e:
        print(f"Error: Could not copy template database: {e}", file=sys.stderr)
        sys.exit(1)

    conn_massive = None
    conn_output = None

    try:
        # 2. Connect to databases
        conn_massive = sqlite3.connect(massive_db_path)
        # Use row_factory to easily access columns by name
        conn_massive.row_factory = sqlite3.Row
        cursor_massive = conn_massive.cursor()

        conn_output = sqlite3.connect(output_db_path)
        cursor_output = conn_output.cursor()
        print("Database connections established.")

        # --- Migration Counters ---
        sets_migrated = 0
        weights_migrated = 0
        sets_skipped = 0
        weights_skipped = 0

        # List to store (timestamp_seconds, weight_value) for lookup
        processed_weights_sorted = []

        # 3. Migrate Body Weight Data (Massive `weights` -> Flexify `gym_sets`)
        print("\nMigrating body weight data...")
        # Make sure to order by created ASC to process chronologically
        cursor_massive.execute("SELECT value, created, unit FROM weights ORDER BY created ASC")
        massive_weights = cursor_massive.fetchall()

        insert_weight_sql = """
            INSERT INTO gym_sets (
                name, reps, weight, unit, created, hidden, cardio, distance,
                duration, body_weight, category, image, incline, notes, plan_id, rest_ms
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """

        for row in massive_weights:
            created_seconds = iso_to_seconds(row['created'])
            if created_seconds is None:
                print(f"Skipping weight entry due to invalid date: {row['created']}")
                weights_skipped += 1
                continue

            # Store the valid weight entry for later lookup when processing sets
            processed_weights_sorted.append((created_seconds, row['value']))

            # Use the weight value for both 'weight' and 'body_weight' columns
            # Set reps=1.0 for a single measurement event
            values = (
                'Weight',                     # name (Flexify's convention for body weight)
                1.0,                          # reps (represent as a single measurement)
                row['value'],                 # weight (the actual body weight value)
                row['unit'] or 'kg',          # unit (default to kg if NULL)
                created_seconds,              # created (converted timestamp in seconds)
                0,                            # hidden (0 = visible entry)
                0,                            # cardio (0 = false)
                0.0,                          # distance
                0.0,                          # duration
                row['value'],                 # body_weight (also store here for the weight entry itself)
                None,                         # category
                None,                         # image
                None,                         # incline
                None,                         # notes
                None,                         # plan_id
                None                          # rest_ms
            )
            try:
                cursor_output.execute(insert_weight_sql, values)
                weights_migrated += 1
            except sqlite3.Error as e:
                print(f"Error inserting weight record created {row['created']}: {e}", file=sys.stderr)
                weights_skipped += 1

        print(f"Finished migrating body weight: {weights_migrated} migrated, {weights_skipped} skipped.")
        # Ensure the list is sorted, although the SQL query should already handle this
        processed_weights_sorted.sort(key=lambda x: x[0])

        # 4. Migrate Workout Set Data (Massive `sets` -> Flexify `gym_sets`)
        print("\nMigrating workout set data...")
        # Make sure to order by created ASC to process chronologically
        cursor_massive.execute("""
            SELECT name, reps, weight, created, unit, hidden, image, minutes, seconds
            FROM sets
            ORDER BY created ASC
        """)
        massive_sets = cursor_massive.fetchall()

        insert_set_sql = """
            INSERT INTO gym_sets (
                name, reps, weight, unit, created, hidden, image, rest_ms,
                body_weight, cardio, category, distance, duration, incline, notes, plan_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """

        # --- Body weight lookup logic initialization ---
        current_weight_index = 0
        # Default body weight: Use the first recorded weight if available, else 0.0
        last_known_body_weight = processed_weights_sorted[0][1] if processed_weights_sorted else 0.0
        print(f"Using initial body weight for sets: {last_known_body_weight}") # Optional: Info message
        # --- End initialization ---

        for row in massive_sets:
            # created_millis = iso_to_millis(row['created']) # Old line
            created_seconds = iso_to_seconds(row['created']) # Use the corrected function
            if created_seconds is None:
                print(f"Skipping set entry '{row['name']}' due to invalid date: {row['created']}")
                sets_skipped += 1
                continue

            # --- Find the relevant body weight for this set's timestamp ---
            # Iterate through sorted weights to find the latest one <= set's timestamp
            while (current_weight_index < len(processed_weights_sorted) and
                   processed_weights_sorted[current_weight_index][0] <= created_seconds):
                last_known_body_weight = processed_weights_sorted[current_weight_index][1]
                current_weight_index += 1
            # Now last_known_body_weight holds the correct value for this set
            # --- End body weight lookup ---

            # Calculate rest time in milliseconds
            rest_ms = None
            try:
                minutes = row['minutes'] if row['minutes'] is not None else 0
                seconds = row['seconds'] if row['seconds'] is not None else 0
                if minutes > 0 or seconds > 0:
                     rest_ms = (minutes * 60 + seconds) * 1000
            except TypeError:
                 print(f"Warning: Could not calculate rest time for set '{row['name']}' created {row['created']}. Setting rest_ms to NULL.", file=sys.stderr)
                 rest_ms = None

            values = (
                row['name'],                  # name
                row['reps'],                  # reps
                row['weight'],                # weight
                row['unit'] or 'kg',          # unit
                created_seconds,              # created (in seconds)
                row['hidden'],                # hidden
                row['image'],                 # image
                rest_ms,                      # rest_ms
                last_known_body_weight,       # body_weight (looked up)
                0,                            # cardio
                None,                         # category
                0.0,                          # distance
                0.0,                          # duration
                None,                         # incline
                None,                         # notes
                None                          # plan_id
            )
            try:
                cursor_output.execute(insert_set_sql, values)
                sets_migrated += 1
            except sqlite3.Error as e:
                print(f"Error inserting set record '{row['name']}' created {row['created']}: {e}", file=sys.stderr)
                sets_skipped += 1

        print(f"Finished migrating workout sets: {sets_migrated} migrated, {sets_skipped} skipped.")

        # 5. Commit changes to the output database
        conn_output.commit()
        print("\nChanges committed to the output database.")

    except sqlite3.Error as e:
        print(f"\nAn SQLite error occurred: {e}", file=sys.stderr)
        if conn_output:
            conn_output.rollback() # Rollback changes on error
        sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error occurred: {e}", file=sys.stderr)
        if conn_output:
            conn_output.rollback()
        sys.exit(1)
    finally:
        # 6. Close connections
        if conn_massive:
            conn_massive.close()
        if conn_output:
            conn_output.close()
        print("Database connections closed.")

    print("\nMigration process completed.")
    print(f"  Total Body Weight Records: Migrated={weights_migrated}, Skipped={weights_skipped}")
    print(f"  Total Workout Sets: Migrated={sets_migrated}, Skipped={sets_skipped}")
    print(f"\nOutput database saved to: {output_db_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Migrate workout sets and body weight data from Massive DB to a new Flexify DB.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("massive_db", help="Path to the source Massive database file (e.g., massive.db)")
    parser.add_argument("flexify_template_db", help="Path to the initialized Flexify template database file (e.g., flexify.sqlite)")
    parser.add_argument("output_db", help="Path for the new output database file (e.g., flexify_migrated.sqlite)")

    args = parser.parse_args()

    migrate_data(args.massive_db, args.flexify_template_db, args.output_db)