import sqlite3
import shutil
import os
import pandas as pd

# --- Configuration ---
SOURCE_DB = "data/flexify.sqlite"
TARGET_DB = "data/flexify_beefed.sqlite"
DEFINITIONS_CSV = "data/exercise_definitions.csv"
BACKUP_CSV = "data/exercise_definitions.bak.csv"

def get_connection(db_path):
    """Creates a connection to the SQLite database."""
    return sqlite3.connect(db_path)

def detect_time_unit(series):
    """
    Heuristic to detect if timestamps are seconds or milliseconds.
    """
    if series.empty: return 's'
    median_ts = series.median()
    # > 3 billion usually implies milliseconds for recent dates
    if median_ts > 3_000_000_000: 
        return 'ms'
    return 's'

def clean_round(val):
    """
    Forces a clean 2 decimal float for Python before sending to SQLite.
    (SQLite might still store 92.9 as 92.9000000000000056 internally due to IEEE 754)
    """
    if pd.isna(val): return None
    return float(f"{val:.2f}")

def maintain_definitions(conn):
    print("--- Task 1: Updating & Sorting Exercise Definitions CSV ---")
    
    # 1. Load existing definitions
    if not os.path.exists(DEFINITIONS_CSV):
        print(f"Warning: {DEFINITIONS_CSV} not found. Creating new.")
        # We include 'first_seen' in the schema now
        csv_df = pd.DataFrame(columns=["Exercise", "isBW", "bwMultiplier", "MovementPattern", "PrimaryMover", "Notes", "first_seen"])
    else:
        csv_df = pd.read_csv(DEFINITIONS_CSV)
        # Create backup
        shutil.copy(DEFINITIONS_CSV, BACKUP_CSV)
        print(f"Backup created at {BACKUP_CSV}")

    # 2. Get stats from DB (name and first_seen)
    # Exclude 'Weight' as that is bodyweight log, not an exercise
    query = """
    SELECT name, MIN(created) as db_first_seen_raw 
    FROM gym_sets 
    WHERE name != 'Weight' 
    GROUP BY name
    """
    db_df = pd.read_sql_query(query, conn)
    
    # Convert DB timestamps to readable ISO format for the CSV
    time_unit = detect_time_unit(db_df['db_first_seen_raw'])
    db_df['first_seen'] = pd.to_datetime(db_df['db_first_seen_raw'], unit=time_unit)

    # 3. Merge CSV with DB stats
    # We do an OUTER join. 
    # - Rows in CSV but not DB: Old exercises, keep them.
    # - Rows in DB but not CSV: New exercises, add them.
    merged = pd.merge(csv_df, db_df, left_on="Exercise", right_on="name", how="outer")
    
    # 4. Fill Data
    # If 'Exercise' is NaN (meaning it came from DB only), fill it with 'name'
    merged['Exercise'] = merged['Exercise'].fillna(merged['name'])
    
    # Identify new rows (where 'isBW' was NaN, meaning it wasn't in the CSV)
    new_mask = merged['isBW'].isna()
    if new_mask.any():
        print(f"Found {new_mask.sum()} new exercises.")
        merged.loc[new_mask, 'isBW'] = 0
        merged.loc[new_mask, 'bwMultiplier'] = 0
        merged.loc[new_mask, 'Notes'] = "Auto-added needs review"
    
    # Update 'first_seen' in CSV using the fresh data from DB. 
    # If the CSV already had a 'first_seen', we overwrite it with the DB truth (y_column)
    # If the exercise is not in DB anymore (legacy), we keep the old date if it exists (x_column)
    if 'first_seen_x' in merged.columns and 'first_seen_y' in merged.columns:
        merged['first_seen'] = merged['first_seen_y'].fillna(merged['first_seen_x'])
    elif 'first_seen' in db_df.columns:
         # if CSV didn't have first_seen column before, just take from DB
         pass 

    # 5. Clean up columns
    # Ensure we have the columns we want in order
    cols = ["Exercise", "isBW", "bwMultiplier", "MovementPattern", "PrimaryMover", "Notes", "first_seen"]
    final_df = merged[cols].copy()
    
    # 6. Sort by first_seen
    final_df = final_df.sort_values(by="first_seen", na_position='last')
    
    # 7. Save
    final_df.to_csv(DEFINITIONS_CSV, index=False)
    print(f"Updated {DEFINITIONS_CSV} (sorted by first appearance).")

def backfill_categories(conn):
    print("\n--- Task 3: Backfilling Categories ---")
    
    # Get all names and their associated non-empty categories
    query = "SELECT name, category FROM gym_sets WHERE category IS NOT NULL AND category != ''"
    df = pd.read_sql_query(query, conn)
    
    grouped = df.groupby('name')['category'].unique()
    
    cursor = conn.cursor()
    updates_made = 0
    
    for name, categories in grouped.items():
        if len(categories) == 0:
            continue
        
        target_category = None
        
        if len(categories) == 1:
            # Good case: exactly one category used for this exercise
            target_category = categories[0]
            # Logic: Update only rows that are MISSING a category
            sql = "UPDATE gym_sets SET category = ? WHERE name = ? AND (category IS NULL OR category = '')"
            cursor.execute(sql, (target_category, name))
            updates_made += cursor.rowcount
            
        else:
            # Conflict case: Multiple categories
            print(f"\n[!] CONFLICT: Exercise '{name}' uses multiple categories: {categories}")
            print(f"    Type the category name to FORCE UPDATE ALL entries for '{name}'.")
            print(f"    Press [ENTER] to skip and leave as is.")
            
            user_input = input(f"    > ").strip()
            
            if user_input:
                target_category = user_input
                print(f"    -> Overwriting ALL '{name}' entries to '{target_category}'...")
                # Logic: Overwrite EVERYTHING (even existing different categories)
                sql = "UPDATE gym_sets SET category = ? WHERE name = ?"
                cursor.execute(sql, (target_category, name))
                updates_made += cursor.rowcount
            else:
                print("    -> Skipped.")

    conn.commit()
    print(f"Category updates applied to {updates_made} rows.")

def inject_bodyweight(conn):
    print("\n--- Task 2: Injecting Smart Bodyweight (Moving Average) ---")
    
    df = pd.read_sql_query("SELECT id, created, name, weight FROM gym_sets", conn)
    
    if df.empty: return

    # Detect time unit and convert to datetime
    time_unit = detect_time_unit(df['created'])
    df['dt'] = pd.to_datetime(df['created'], unit=time_unit)
    
    # Extract Ground Truth Bodyweight
    bw_df = df[df['name'] == 'Weight'].copy()
    if bw_df.empty:
        print("No bodyweight measurements found. Skipping.")
        return

    # Calculate Moving Average Curve
    bw_series = bw_df.set_index('dt')['weight']
    daily_bw = bw_series.resample('D').mean()
    daily_interpolated = daily_bw.interpolate(method='time')
    smoothed_bw = daily_interpolated.rolling(window=7, min_periods=1).mean()
    
    # Map smoothed weights back to DB
    lookup_df = pd.DataFrame({'dt': smoothed_bw.index, 'smart_bw': smoothed_bw.values})
    lookup_df = lookup_df.dropna().sort_values('dt')
    
    df = df.sort_values('dt')
    merged_df = pd.merge_asof(df, lookup_df, on='dt', direction='backward')
    merged_df['smart_bw'] = merged_df['smart_bw'].bfill()
    
    # Clean Rounding (Python side)
    # We apply the helper function to ensure it's a clean standard float
    merged_df['smart_bw'] = merged_df['smart_bw'].apply(clean_round)
    
    # Update Database
    update_data = merged_df[['id', 'smart_bw']].dropna()
    
    # Convert to list of tuples (smart_bw, id)
    update_list = [(row.smart_bw, row.id) for row in update_data.itertuples(index=False)]

    cursor = conn.cursor()
    cursor.executemany("UPDATE gym_sets SET body_weight = ? WHERE id = ?", update_list)
    conn.commit()
    
    print(f"Updated 'body_weight' column for {len(update_list)} rows.")

def main():
    if not os.path.exists(SOURCE_DB):
        print(f"Error: Source database {SOURCE_DB} not found.")
        return

    print(f"Copying {SOURCE_DB} to {TARGET_DB}...")
    shutil.copy(SOURCE_DB, TARGET_DB)
    
    conn = get_connection(TARGET_DB)
    
    try:
        # Task 1: CSV Maintenance
        maintain_definitions(conn)
        
        # Task 3: Categories (Run before bodyweight just to keep logic clean)
        backfill_categories(conn)
        
        # Task 2: Bodyweight
        inject_bodyweight(conn)
        
    finally:
        conn.close()
        print("\n--- Maintenance Complete ---")
        print(f"Beefed up database saved to: {TARGET_DB}")

if __name__ == "__main__":
    main()