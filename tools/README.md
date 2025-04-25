# Tools

## `migrate_data.R`

I used this script to unify data from my old ODS file and Massive's `massive.db` file. It creates a `.csv` file that is identical to the file `sets.csv` that Massive exports.

## `migrate_csv_to_sqlite.py`

This was used to convert the `.csv` file produced by the previous script back to `massive.db` file that could be imported by Massive app. It requires an empty `massive.db` file as one of it's arguments.

## `migrate_massive_to_flexify.py`

This converts `massive.db` into a working `flexify.sqlite` database that can be imported by `Flexify` app. Plans are currently not migrated, only body-weight and exercise data.
