#!/bin/bash

# --- Configuration ---
# Path to Rscript executable (usually in PATH, but can be set explicitly)
RSCRIPT_EXEC="Rscript"

# Path to Python virtual environment
VENV_PATH=".venv"
# Path to the Python executable within the virtual environment
PYTHON_EXEC="$VENV_PATH/bin/python"
# Path to the Flexify conversion script
FLEXIFY_SCRIPT_PATH="tools/flexify_to_csv.py"
# Directory containing Flexify databases and where CSVs will be generated
DATA_DIR="data"


# --- Helper Functions ---
usage() {
  cat << EOF
Usage: $(basename "$0") [options]

Wrapper script to launch a Shiny R application using shiny::runApp().
This script will first activate a Python virtual environment and run
'${FLEXIFY_SCRIPT_PATH}' to convert the latest Flexify database
(from '${DATA_DIR}/flexify*.sqlite') into CSV files.

Shiny Options (mirror shiny::runApp arguments):
  -a, --appDir <dir>          Directory containing the Shiny app (app.R, ui.R/server.R).
                              Defaults to current directory if not specified here or in R options.
  -p, --port <port>           Port number to run the app on.
                              Defaults to shiny.port R option or a random available port.
  -l, --launch-browser <bool> Launch the system's default web browser? (TRUE/FALSE)
                              Defaults to shiny.launch.browser R option or interactive() session.
  -H, --host <ip>             Host IP address to bind to.
                              Defaults to shiny.host R option or '127.0.0.1'.
  -w, --workerId <id>         Worker ID for multi-worker setups (e.g., Shiny Server Pro).
                              Defaults to "".
  -q, --quiet                 Suppress output messages from Shiny? (Flag, sets quiet=TRUE)
                              Defaults to FALSE.
  -d, --display-mode <mode>   Display mode ('auto', 'normal', 'showcase').
                              Defaults to 'auto'.
  -t, --test-mode             Enable test mode? (Flag, sets test.mode=TRUE)
                              Defaults to shiny.testmode R option or FALSE.

Additional wrapper options:
  -b, --browser-value <value> Set R's options(browser = VALUE) before launching.
                              Example: --browser-value 'firefox' or --browser-value '/usr/bin/google-chrome %s'
                              Example: --browser-value 'function(url) {.Call("browseURL", url)}' # R default
  -r, --autoreload <bool>     Enable Shiny's automatic code reloading? (TRUE/FALSE)
                              Defaults to FALSE if not specified.
  -h, --help                  Show this help message and exit.
  -v, --verbose               Show the generated R command before running.

Notes:
* Arguments passed here override R options (like shiny.port).
* Boolean flags like --quiet and --test-mode set the R argument to TRUE.
* Boolean arguments like --launch-browser and --autoreload require a TRUE/FALSE value.
* String values for --browser-value might need appropriate shell quoting if they contain spaces or special characters.
EOF
  exit 1
}

# --- Argument Parsing ---
# Initialize variables
APP_DIR=""
PORT=""
LAUNCH_BROWSER=""
HOST=""
WORKER_ID=""
QUIET="FALSE" # Default R value
DISPLAY_MODE=""
TEST_MODE="FALSE" # Default R value
BROWSER_VALUE=""
AUTORELOAD_VALUE=""
VERBOSE=0

GETOPT_CMD="getopt"
PARSED_ARGS=$($GETOPT_CMD -o a:p:l:H:w:qd:tb:hvr: --long appDir:,port:,launch-browser:,host:,workerId:,quiet,display-mode:,test-mode,browser-value:,help,verbose,autoreload: \
             -n "$(basename "$0")" -- "$@")

if [ $? -ne 0 ]; then
    usage
fi

eval set -- "$PARSED_ARGS"

while true; do
    case "$1" in
        -a | --appDir) APP_DIR="$2"; shift 2 ;;
        -p | --port) PORT="$2"; shift 2 ;;
        -l | --launch-browser) LAUNCH_BROWSER="$2"; shift 2 ;;
        -H | --host) HOST="$2"; shift 2 ;;
        -w | --workerId) WORKER_ID="$2"; shift 2 ;;
        -q | --quiet) QUIET="TRUE"; shift 1 ;;
        -d | --display-mode) DISPLAY_MODE="$2"; shift 2 ;;
        -t | --test-mode) TEST_MODE="TRUE"; shift 1 ;;
        -b | --browser-value) BROWSER_VALUE="$2"; shift 2 ;;
        -r | --autoreload) AUTORELOAD_VALUE="$2"; shift 2 ;;
        -h | --help) usage ;;
        -v | --verbose) VERBOSE=1; shift 1 ;;
        --) shift; break ;;
        *) echo "Programming error in getopt parsing!"; exit 3 ;;
    esac
done

if [ $# -gt 0 ]; then
    echo "Error: Unexpected arguments found: $@"
    usage
fi

# --- Pre-Shiny Steps: Virtual Env and Data Conversion ---
echo "--- Preparing data for Shiny app ---"

# 1. Check for virtual environment and Python executable
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Python virtual environment not found at $VENV_PATH"
    echo "Please create it first (e.g., python -m venv $VENV_PATH && $VENV_PATH/bin/pip install -r requirements.txt)"
    exit 1
fi
if [ ! -x "$PYTHON_EXEC" ]; then
    echo "Error: Python executable not found or not executable in virtual environment at $PYTHON_EXEC"
    exit 1
fi

# 2. Check for Flexify conversion script
if [ ! -f "$FLEXIFY_SCRIPT_PATH" ]; then
    echo "Error: Flexify conversion script not found at $FLEXIFY_SCRIPT_PATH"
    exit 1
fi

# 3. Find the latest Flexify SQLite database
echo "Looking for the latest Flexify SQLite database in '$DATA_DIR'..."
# List files matching pattern, sort by modification time (newest first), take the first one.
# Suppress "No such file or directory" error from ls if no files match, then check if LATEST_DB_FILE is empty.
LATEST_DB_FILE=$(ls -1t "$DATA_DIR"/flexify*.sqlite 2>/dev/null | head -n 1)

if [ -z "$LATEST_DB_FILE" ]; then
    echo "Error: No Flexify SQLite database files (flexify*.sqlite) found in '$DATA_DIR'."
    echo "Cannot proceed with data conversion."
    exit 1
fi
echo "Found latest database: $LATEST_DB_FILE"

# 4. Run the conversion script
echo "Running Flexify to CSV conversion using $PYTHON_EXEC $FLEXIFY_SCRIPT_PATH..."
# Assuming flexify_to_csv.py takes the DB file as its first argument
# and outputs CSVs to a location Shiny expects (e.g., ./data or a configured path within the script)
"$PYTHON_EXEC" "$FLEXIFY_SCRIPT_PATH" "$LATEST_DB_FILE"
CONVERSION_STATUS=$?

if [ $CONVERSION_STATUS -ne 0 ]; then
    echo "Error: Flexify to CSV conversion script failed with exit code $CONVERSION_STATUS."
    exit $CONVERSION_STATUS
fi
echo "Data conversion successful. CSV files should be updated in '$DATA_DIR'."
echo "--- Data preparation complete ---"
echo # Newline for readability before Shiny output

# --- Construct R Command ---
R_CODE="library(shiny);"

if [[ -n "$BROWSER_VALUE" ]]; then
    R_CODE+=" options(browser = '${BROWSER_VALUE//\'/\'\\\'\'}');"
fi

if [[ -n "$AUTORELOAD_VALUE" ]]; then
    AUTORELOAD_R_VALUE="${AUTORELOAD_VALUE^^}"
    if [[ "$AUTORELOAD_R_VALUE" != "TRUE" && "$AUTORELOAD_R_VALUE" != "FALSE" ]]; then
        echo "Error: Invalid value for --autoreload/-r: '$AUTORELOAD_VALUE'. Must be TRUE or FALSE."
        usage
    fi
    R_CODE+=" options(shiny.autoreload = ${AUTORELOAD_R_VALUE});"
fi

R_ARGS=()
if [[ -n "$APP_DIR" ]]; then R_ARGS+=("appDir = '${APP_DIR//\'/\'\\\'\'}'"); fi
if [[ -n "$PORT" ]]; then R_ARGS+=("port = $PORT"); fi
if [[ -n "$LAUNCH_BROWSER" ]]; then R_ARGS+=("launch.browser = ${LAUNCH_BROWSER^^}"); fi
if [[ -n "$HOST" ]]; then R_ARGS+=("host = '${HOST//\'/\'\\\'\'}'"); fi
if [[ -n "$WORKER_ID" ]]; then R_ARGS+=("workerId = '${WORKER_ID//\'/\'\\\'\'}'"); fi
R_ARGS+=("quiet = $QUIET")
if [[ -n "$DISPLAY_MODE" ]]; then R_ARGS+=("display.mode = '${DISPLAY_MODE//\'/\'\\\'\'}'"); fi
R_ARGS+=("test.mode = $TEST_MODE")

R_ARGS_STR=$(IFS=, ; echo "${R_ARGS[*]}")
R_CODE+=" shiny::runApp(${R_ARGS_STR});"

# --- Execute R Command ---
if [[ "$VERBOSE" -eq 1 ]]; then
  echo "--------------------------------------------------"
  echo "Generated R Command:"
  echo "$R_CODE"
  echo "--------------------------------------------------"
  echo "Executing Shiny App..."
fi

if ! command -v "$RSCRIPT_EXEC" &> /dev/null; then
    echo "Error: '$RSCRIPT_EXEC' command not found."
    echo "Please ensure R is installed and Rscript is in your PATH,"
    echo "or set the RSCRIPT_EXEC variable in the script."
    exit 127
fi

"$RSCRIPT_EXEC" -e "$R_CODE"
R_EXIT_CODE=$?

if [[ "$VERBOSE" -eq 1 ]]; then
    echo "--------------------------------------------------"
    echo "Rscript finished with exit code: $R_EXIT_CODE"
    echo "--------------------------------------------------"
fi

exit $R_EXIT_CODE
