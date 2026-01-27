#!/bin/bash
# File: run_manytimes.sh
# Usage:  ./run_manytimes.sh [-m MODEL] "input.txt"  "output_prefix"  50

# ===================== CONFIGURATION =====================
# Default model (can be overridden with -m option)
MODEL="gemma3:12b"              # Default model (e.g. qwen2.5:32b, llama3.1:70b, etc)
# =========================================================

# Parse options
FORCE=false
while getopts "m:f" opt; do
    case $opt in
        m)
            MODEL="$OPTARG"
            ;;        f)
            FORCE=true
            ;;        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

# Shift to get positional arguments after options
shift $((OPTIND-1))

OLLAMA_CMD="ollama run $MODEL"

# Check required arguments
if [ $# -lt 3 ]; then
    echo "Usage: $0 [-m MODEL] [-f] INPUT_FILE  OUTPUT_PREFIX  NUMBER_OF_RUNS"
    echo ""
    echo "Options:"
    echo "  -m MODEL        : specify model to use (default: gemma3:12b)"
    echo "  -f              : force regeneration from scratch (ignore existing files)"
    echo ""
    echo "Arguments:"
    echo "  INPUT_FILE      : prompt file to feed to ollama"
    echo "  OUTPUT_PREFIX   : prefix for output files (will add 00,01,02...)"
    echo "  NUMBER_OF_RUNS  : how many times to run (1~100)"
    echo ""
    echo "Current model: $MODEL"
    echo ""
    echo "Examples:"
    echo "  ./run_manytimes.sh  sqbracket1.txt          sqbracket1          50"
    echo "  ./run_manytimes.sh  -m qwen2.5:32b  prompts/math.txt  math-result-v1  30"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_PREFIX="$2"
N_INPUT="$3"

# Force N to be a proper integer (fixes counter display issue)
if ! [[ "$N_INPUT" =~ ^[0-9]+$ ]]; then
    echo "Error: NUMBER_OF_RUNS must be a positive integer (got: '$N_INPUT')"
    exit 1
fi

N=$((10#$N_INPUT))

if [ "$N" -lt 1 ] || [ "$N" -gt 100 ]; then
    echo "Error: NUMBER_OF_RUNS must be between 1 and 100"
    exit 1
fi

# Check if input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: Input file '$INPUT_FILE' does not exist"
    exit 1
fi

echo "Starting batch run:"
echo "  Model           : $MODEL"
echo "  Input file      : $INPUT_FILE"
echo "  Output files    : ${OUTPUT_PREFIX}00 ~ ${OUTPUT_PREFIX}$(printf "%02d" $((N-1)))"
echo "  Total runs      : $N times"
if [ "$FORCE" = true ]; then
    echo "  Force mode      : ON (regenerating all files)"
fi
echo "----------------------------------------"

# Find starting point: check existing output files
start_from=0

if [ "$FORCE" = false ]; then
# Get modification time of the input file (prompt)
INPUT_MTIME=$(stat -c %Y "$INPUT_FILE" 2>/dev/null || stat -f %m "$INPUT_FILE" 2>/dev/null)
existing_files=()
last_uptodate_idx=-1
for ((i=0; i<N; i++)); do
    num=$(printf "%02d" "$i")
    output_file="${OUTPUT_PREFIX}${num}"
    if [ -f "$output_file" ]; then
        existing_files+=("$output_file")
        # Check if file is outdated (older than input file)
        OUTPUT_MTIME=$(stat -c %Y "$output_file" 2>/dev/null || stat -f %m "$output_file" 2>/dev/null)
        if [ "$OUTPUT_MTIME" -lt "$INPUT_MTIME" ]; then
            echo "  File $output_file is outdated (older than input file)"
            # Start from the last up-to-date file (which might be incomplete)
            # If no up-to-date files exist, start from 0
            if [ "$last_uptodate_idx" -ge 0 ]; then
                start_from=$last_uptodate_idx
                echo "  Restarting from last up-to-date file (might be incomplete)"
            else
                start_from=0
            fi
            break
        else
            # File is up-to-date, remember this index
            last_uptodate_idx=$i
        fi
    else
        # File doesn't exist, start from here (or from last up-to-date if exists)
        if [ "$last_uptodate_idx" -ge 0 ]; then
            start_from=$last_uptodate_idx
            echo "  Missing file detected, restarting from last up-to-date file (might be incomplete)"
        else
            start_from=$i
        fi
        break
    fi
done

# If all files exist and are up-to-date, restart from the last file
# (last file might be incomplete)
if [ "${#existing_files[@]}" -gt 0 ] && [ "$start_from" -eq 0 ] && [ "$last_uptodate_idx" -ge 0 ]; then
    start_from=$last_uptodate_idx
    last_file="${existing_files[$last_uptodate_idx]}"
    echo "  Restarting from last file: $last_file (might be incomplete)"
fi

if [ "$start_from" -gt 0 ]; then
    echo "  Resuming from index $start_from (skipping already completed files)"
    echo "----------------------------------------"
fi

fi  # End of if [ "$FORCE" = false ]

for ((i=start_from; i<N; i++)); do
    num=$(printf "%02d" "$i")
    output_file="${OUTPUT_PREFIX}${num}"

    # Explicit integer calculation for current count (more reliable in some bash environments)
    current=$((i + 1))

    echo "[${current}/${N}] Generating ${output_file}..."

    # Current setting: stderr is NOT redirected
    # ollama progress bars, loading animations, etc. will appear on the terminal screen
    $OLLAMA_CMD < "$INPUT_FILE" > "$output_file"

    # === Optional: ways to handle stderr ===
    # 1. Suppress all stderr (no progress bars):    $OLLAMA_CMD < "$INPUT_FILE" > "$output_file" 2>/dev/null
    # 2. Save stderr to file:                       $OLLAMA_CMD < "$INPUT_FILE" > "$output_file" 2>"${output_file}.err"
    # 3. Capture everything:                        $OLLAMA_CMD < "$INPUT_FILE" &> "${output_file}.full"

done

echo "----------------------------------------"
echo "Batch completed!"
echo "Created $N files using model: $MODEL"
echo "Files: ${OUTPUT_PREFIX}00 ~ ${OUTPUT_PREFIX}$(printf "%02d" $((N-1)))"
echo "Quick check: ls -1 ${OUTPUT_PREFIX}*"
