#!/bin/bash

# --- Configuration ---
# Path to Rscript executable (usually in PATH, but can be set explicitly)
RSCRIPT_EXEC="Rscript"

# --- Helper Functions ---
usage() {
  cat << EOF
Usage: $(basename "$0") [options]

Wrapper script to launch a Shiny R application using shiny::runApp().

Options mirror shiny::runApp arguments:
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
  -h, --help                  Show this help message and exit.
  -v, --verbose               Show the generated R command before running.

Notes:
* Arguments passed here override R options (like shiny.port).
* Boolean flags like --quiet and --test-mode set the R argument to TRUE.
* Boolean arguments like --launch-browser require a TRUE/FALSE value.
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
VERBOSE=0

# Use getopt for robust argument parsing (handles long options)
# Note: Adjust getopt syntax slightly if using macOS getopt (install gnu-getopt: brew install gnu-getopt)
GETOPT_CMD="getopt"
# On macOS with gnu-getopt installed via Homebrew, it might be named 'ggetopt'
# or you might need to add it to your PATH. Check your system.
# if [[ "$(uname)" == "Darwin" ]] && command -v ggetopt &> /dev/null; then
#    GETOPT_CMD="ggetopt"
# fi

# Define options
# Short options: a:, p:, l:, H:, w:, q, d:, t, b:, h, v
# Long options: appDir:, port:, launch-browser:, host:, workerId:, quiet, display-mode:, test-mode, browser-value:, help, verbose
# The ':' means the option takes an argument.
PARSED_ARGS=$($GETOPT_CMD -o a:p:l:H:w:qd:tb:hv --long appDir:,port:,launch-browser:,host:,workerId:,quiet,display-mode:,test-mode,browser-value:,help,verbose \
             -n "$(basename "$0")" -- "$@")

# Check if getopt had parsing errors
if [ $? -ne 0 ]; then
    usage
fi

# Use eval to correctly handle arguments with spaces
eval set -- "$PARSED_ARGS"

# Process parsed arguments
while true; do
    case "$1" in
        -a | --appDir)
            APP_DIR="$2"
            shift 2
            ;;
        -p | --port)
            PORT="$2"
            shift 2
            ;;
        -l | --launch-browser)
            LAUNCH_BROWSER="$2"
            shift 2
            ;;
        -H | --host)
            HOST="$2"
            shift 2
            ;;
        -w | --workerId)
            WORKER_ID="$2"
            shift 2
            ;;
        -q | --quiet)
            QUIET="TRUE" # Set R value to TRUE when flag is present
            shift 1
            ;;
        -d | --display-mode)
            DISPLAY_MODE="$2"
            shift 2
            ;;
        -t | --test-mode)
            TEST_MODE="TRUE" # Set R value to TRUE when flag is present
            shift 1
            ;;
        -b | --browser-value)
            BROWSER_VALUE="$2"
            shift 2
            ;;
        -h | --help)
            usage
            ;;
        -v | --verbose)
            VERBOSE=1
            shift 1
            ;;
        --)
            shift # Consume the '--' separator
            break # End of options
            ;;
        *)
            echo "Programming error in getopt parsing!"
            exit 3
            ;;
    esac
done

# Handle any remaining arguments (should be none according to our getopt setup)
if [ $# -gt 0 ]; then
    echo "Error: Unexpected arguments found: $@"
    usage
fi

# --- Construct R Command ---

# Start R code string
R_CODE="library(shiny);"

# Add browser option if specified
if [[ -n "$BROWSER_VALUE" ]]; then
    # Quote the value properly for R string literal. Basic single quoting.
    # More complex values might need careful escaping.
    # This handles simple cases like 'firefox' or '/path/to/browser %s'
    # For R functions passed as string, ensure the string is valid R syntax.
    R_CODE+=" options(browser = '${BROWSER_VALUE//\'/\'\\\'\'}');" # Escape single quotes within the value
fi

# Build the list of arguments for runApp
R_ARGS=()
if [[ -n "$APP_DIR" ]]; then
    # Quote for R string literal
    R_ARGS+=("appDir = '${APP_DIR//\'/\'\\\'\'}'")
fi
if [[ -n "$PORT" ]]; then
    # Numeric, no quotes needed in R
    R_ARGS+=("port = $PORT")
fi
if [[ -n "$LAUNCH_BROWSER" ]]; then
    # Boolean, uppercase TRUE/FALSE, no quotes needed in R
    R_ARGS+=("launch.browser = ${LAUNCH_BROWSER^^}")
fi
if [[ -n "$HOST" ]]; then
    # Quote for R string literal
    R_ARGS+=("host = '${HOST//\'/\'\\\'\'}'")
fi
if [[ -n "$WORKER_ID" ]]; then
    # Quote for R string literal
    R_ARGS+=("workerId = '${WORKER_ID//\'/\'\\\'\'}'")
fi
# Quiet is handled directly by its TRUE/FALSE value
R_ARGS+=("quiet = $QUIET")

if [[ -n "$DISPLAY_MODE" ]]; then
    # Quote for R string literal
    R_ARGS+=("display.mode = '${DISPLAY_MODE//\'/\'\\\'\'}'")
fi
# Test mode is handled directly by its TRUE/FALSE value
R_ARGS+=("test.mode = $TEST_MODE")

# Join arguments with commas
R_ARGS_STR=$(IFS=, ; echo "${R_ARGS[*]}")

# Append the runApp call
R_CODE+=" shiny::runApp(${R_ARGS_STR});"

# --- Execute R Command ---
if [[ "$VERBOSE" -eq 1 ]]; then
  echo "--------------------------------------------------"
  echo "Generated R Command:"
  echo "$R_CODE"
  echo "--------------------------------------------------"
  echo "Executing Shiny App..."
fi

# Check if Rscript exists
if ! command -v "$RSCRIPT_EXEC" &> /dev/null; then
    echo "Error: '$RSCRIPT_EXEC' command not found."
    echo "Please ensure R is installed and Rscript is in your PATH,"
    echo "or set the RSCRIPT_EXEC variable in the script."
    exit 127
fi

# Execute the R code
"$RSCRIPT_EXEC" -e "$R_CODE"

# Capture exit status from Rscript
R_EXIT_CODE=$?

if [[ "$VERBOSE" -eq 1 ]]; then
    echo "--------------------------------------------------"
    echo "Rscript finished with exit code: $R_EXIT_CODE"
    echo "--------------------------------------------------"
fi

exit $R_EXIT_CODE