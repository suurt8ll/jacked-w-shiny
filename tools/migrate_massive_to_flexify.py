import sqlite3
import argparse
import shutil
import sys
from datetime import datetime, timezone

def iso_to_millis(iso_string):
    """Converts an ISO 8601 string to milliseconds since epoch (UTC)."""
    if not iso_string:
        return None
    try:
        # Parse the ISO string
        dt = datetime.fromisoformat(iso_string.replace('Z', '+00:00'))
        # Ensure it's timezone-aware (assume UTC if no offset)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        # Convert to UTC timestamp in seconds and then to milliseconds
        timestamp_millis = int(dt.timestamp() * 1000)
        return timestamp_millis
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

        # 3. Migrate Body Weight Data (Massive `weights` -> Flexify `gym_sets`)
        print("\nMigrating body weight data...")
        cursor_massive.execute("SELECT value, created, unit FROM weights ORDER BY created ASC")
        massive_weights = cursor_massive.fetchall()

        insert_weight_sql = """
            INSERT INTO gym_sets (
                name, reps, weight, unit, created, hidden, cardio, distance,
                duration, body_weight, category, image, incline, notes, plan_id, rest_ms
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """

        for row in massive_weights:
            created_millis = iso_to_millis(row['created'])
            if created_millis is None:
                print(f"Skipping weight entry due to invalid date: {row['created']}")
                weights_skipped += 1
                continue

            # Use the weight value for both 'weight' and 'body_weight' columns
            # Set reps=1.0 for a single measurement event
            values = (
                'Weight',                     # name (Flexify's convention for body weight)
                1.0,                          # reps (represent as a single measurement)
                row['value'],                 # weight (the actual body weight value)
                row['unit'] or 'kg',          # unit (default to kg if NULL)
                created_millis,               # created (converted timestamp)
                0,                            # hidden (0 = visible entry)
                0,                            # cardio (0 = false)
                0.0,                          # distance
                0.0,                          # duration
                row['value'],                 # body_weight (also store here)
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

        # 4. Migrate Workout Set Data (Massive `sets` -> Flexify `gym_sets`)
        print("\nMigrating workout set data...")
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

        # TODO: Optional: Get the latest known body weight for slightly better data
        # For simplicity here, we'll default body_weight to 0.0 for sets
        # A more advanced version could query the newly inserted weights.
        default_body_weight_for_sets = 0.0

        for row in massive_sets:
            created_millis = iso_to_millis(row['created'])
            if created_millis is None:
                print(f"Skipping set entry '{row['name']}' due to invalid date: {row['created']}")
                sets_skipped += 1
                continue

            # Calculate rest time in milliseconds
            rest_ms = None
            try:
                # Ensure minutes and seconds are not None before calculation
                minutes = row['minutes'] if row['minutes'] is not None else 0
                seconds = row['seconds'] if row['seconds'] is not None else 0
                if minutes > 0 or seconds > 0:
                     rest_ms = (minutes * 60 + seconds) * 1000
            except TypeError:
                 print(f"Warning: Could not calculate rest time for set '{row['name']}' created {row['created']}. Setting rest_ms to NULL.", file=sys.stderr)
                 rest_ms = None # Set explicitly to None if calculation fails

            values = (
                row['name'],                  # name
                row['reps'],                  # reps (will be float if stored as REAL)
                row['weight'],                # weight (will be float if stored as REAL)
                row['unit'] or 'kg',          # unit (default to kg if NULL)
                created_millis,               # created (converted timestamp)
                row['hidden'],                # hidden (0 or 1)
                row['image'],                 # image
                rest_ms,                      # rest_ms (calculated)
                default_body_weight_for_sets, # body_weight (defaulting to 0.0)
                0,                            # cardio (0 = false)
                None,                         # category (Massive doesn't store this per set)
                0.0,                          # distance
                0.0,                          # duration
                None,                         # incline
                None,                         # notes (Massive doesn't store this per set)
                None                          # plan_id (Not migrating plans)
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