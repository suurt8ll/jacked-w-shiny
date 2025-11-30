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
    conn = sqlite3.connect(db_path)
    return conn

def detect_time_unit(series):
    """
    Heuristic to detect if timestamps are seconds or milliseconds.
    If the median value is greater than 3 billion, it's likely milliseconds.
    """
    median_ts = series.median()
    # 3e10 is roughly the year 2920 in seconds, but 1970 in milliseconds.
    # 3e9 is year 2065 in seconds.
    if median_ts > 3_000_000_000: 
        return 'ms'
    return 's'

def maintain_definitions(conn):
    print("--- Task 1: Updating Exercise Definitions CSV ---")
    
    # 1. Load existing definitions
    if not os.path.exists(DEFINITIONS_CSV):
        print(f"Warning: {DEFINITIONS_CSV} not found. Creating new.")
        existing_df = pd.DataFrame(columns=["Exercise", "isBW", "bwMultiplier", "MovementPattern", "PrimaryMover", "Notes"])
    else:
        existing_df = pd.read_csv(DEFINITIONS_CSV)
        # Create backup
        shutil.copy(DEFINITIONS_CSV, BACKUP_CSV)

    existing_exercises = set(existing_df['Exercise'].str.strip().unique())

    # 2. Get all exercises from DB (excluding 'Weight' which is bodyweight logs)
    query = """
    SELECT name, MIN(created) as first_seen 
    FROM gym_sets 
    WHERE name != 'Weight' 
    GROUP BY name
    """
    db_exercises_df = pd.read_sql_query(query, conn)
    
    # 3. Identify new exercises
    new_exercises = []
    for _, row in db_exercises_df.iterrows():
        name = row['name'].strip()
        if name not in existing_exercises:
            new_exercises.append({
                "Exercise": name,
                "first_seen": row['first_seen'],
                "isBW": 0,
                "bwMultiplier": 0,
                "MovementPattern": None,
                "PrimaryMover": None,
                "Notes": "Auto-added needs review"
            })
    
    if new_exercises:
        print(f"Found {len(new_exercises)} new exercises.")
        new_df = pd.DataFrame(new_exercises)
        
        # Sort new exercises by when they were first seen
        new_df = new_df.sort_values(by="first_seen")
        
        # Drop the temp sorting column
        new_df = new_df.drop(columns=["first_seen"])
        
        # Combine and Save
        final_df = pd.concat([existing_df, new_df], ignore_index=True)
        final_df.to_csv(DEFINITIONS_CSV, index=False)
        print(f"Updated {DEFINITIONS_CSV}")
    else:
        print("No new exercises found.")

def backfill_categories(conn):
    print("\n--- Task 3: Backfilling Categories ---")
    
    # Get all names and their associated non-empty categories
    query = "SELECT name, category FROM gym_sets WHERE category IS NOT NULL AND category != ''"
    df = pd.read_sql_query(query, conn)
    
    # Group by name to see unique categories used for each exercise
    grouped = df.groupby('name')['category'].unique()
    
    updates = {}
    
    for name, categories in grouped.items():
        if len(categories) == 0:
            continue
        elif len(categories) == 1:
            # Good case: exactly one category used for this exercise
            updates[name] = categories[0]
        else:
            # Bad case: Multiple categories for the same exercise name
            print(f"WARNING: Exercise '{name}' has conflicting categories: {categories}. Skipping auto-update for this exercise.")

    # Apply updates
    cursor = conn.cursor()
    update_count = 0
    
    for name, category in updates.items():
        # Update rows where name matches BUT category is missing
        sql = "UPDATE gym_sets SET category = ? WHERE name = ? AND (category IS NULL OR category = '')"
        cursor.execute(sql, (category, name))
        update_count += cursor.rowcount
        
    conn.commit()
    print(f"Backfilled category for {update_count} rows.")

def inject_bodyweight(conn):
    print("\n--- Task 2: Injecting Smart Bodyweight (Moving Average) ---")
    
    # 1. Load relevant data
    # We need 'id', 'created', 'name', 'weight' (the actual lift/measurement)
    df = pd.read_sql_query("SELECT id, created, name, weight FROM gym_sets", conn)
    
    if df.empty:
        print("Database is empty, skipping.")
        return

    # Detect time unit and convert to datetime
    time_unit = detect_time_unit(df['created'])
    df['dt'] = pd.to_datetime(df['created'], unit=time_unit)
    
    # 2. Extract Ground Truth Bodyweight
    # In this app, rows with name='Weight' are the bodyweight logs
    bw_df = df[df['name'] == 'Weight'].copy()
    
    if bw_df.empty:
        print("No bodyweight measurements found (name='Weight'). Skipping injection.")
        return

    # 3. Calculate Moving Average Curve
    # Resample to Daily to handle multiple weigh-ins per day or gaps
    bw_series = bw_df.set_index('dt')['weight']
    
    # Resample to daily average (if multiple weigh-ins in one day)
    daily_bw = bw_series.resample('D').mean()
    
    # Interpolate time-based to fill gaps between weigh-in days (linear interpolation between points)
    daily_interpolated = daily_bw.interpolate(method='time')
    
    # Calculate Rolling Moving Average (e.g., 7-day window) to smooth it out
    # min_periods=1 ensures we get values even at the start of the log
    smoothed_bw = daily_interpolated.rolling(window=7, min_periods=1).mean()
    
    # 4. Map smoothed weights back to ALL database entries
    # We use merge_asof to find the closest previous timestamp in our smoothed curve
    
    # Prepare lookup table
    lookup_df = pd.DataFrame({'dt': smoothed_bw.index, 'smart_bw': smoothed_bw.values})
    lookup_df = lookup_df.dropna().sort_values('dt')
    
    # Sort main df for merge_asof
    df = df.sort_values('dt')
    
    # Perform the merge
    # direction='backward' means use the latest available rolling average value
    merged_df = pd.merge_asof(df, lookup_df, on='dt', direction='backward')
    
    # Handle cases before the first weigh-in (backfill with the first available weight)
    merged_df['smart_bw'] = merged_df['smart_bw'].bfill()
    
    # Round to 2 decimal places
    merged_df['smart_bw'] = merged_df['smart_bw'].round(2)
    
    # 5. Update the Database
    # We only want to update rows where the calculated bodyweight is valid
    update_data = merged_df[['id', 'smart_bw']].dropna()
    
    # Convert to list of tuples for SQLite executemany
    update_list = list(update_data.itertuples(index=False, name=None))
    # Tuple structure: (id, smart_bw) -> SQL expects (smart_bw, id)
    update_list_formatted = [(bw, row_id) for row_id, bw in update_list]

    cursor = conn.cursor()
    cursor.executemany("UPDATE gym_sets SET body_weight = ? WHERE id = ?", update_list_formatted)
    conn.commit()
    
    print(f"Updated 'body_weight' column for {len(update_list_formatted)} rows.")

def main():
    if not os.path.exists(SOURCE_DB):
        print(f"Error: Source database {SOURCE_DB} not found.")
        return

    print(f"Copying {SOURCE_DB} to {TARGET_DB}...")
    shutil.copy(SOURCE_DB, TARGET_DB)
    
    conn = get_connection(TARGET_DB)
    
    try:
        # Task 1
        maintain_definitions(conn)
        
        # Task 3 (Run before Task 2 or doesn't matter, but good to clean metadata first)
        backfill_categories(conn)
        
        # Task 2
        inject_bodyweight(conn)
        
    finally:
        conn.close()
        print("\n--- Maintenance Complete ---")
        print(f"Beefed up database saved to: {TARGET_DB}")

if __name__ == "__main__":
    main()
