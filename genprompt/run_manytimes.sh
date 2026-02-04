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

OLLAMA_API_BASE="http://localhost:11434/api"
OLLAMA_API_URL="$OLLAMA_API_BASE/generate"
OLLAMA_SERVER_PID=""

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

ensure_ollama_server() {
    if curl -sf "$OLLAMA_API_BASE/tags" >/dev/null; then
        return 0
    fi

    echo "Ollama server not running. Starting 'ollama serve' with OLLAMA_NUM_PARALLEL=4..."
    OLLAMA_NUM_PARALLEL=4 ollama serve > /tmp/ollama-serve.log 2>&1 &
    OLLAMA_SERVER_PID=$!

    for _ in {1..30}; do
        if curl -sf "$OLLAMA_API_BASE/tags" >/dev/null; then
            return 0
        fi
        sleep 1
    done

    echo "Error: Ollama server did not become ready in time."
    if [ -n "$OLLAMA_SERVER_PID" ]; then
        kill "$OLLAMA_SERVER_PID" 2>/dev/null || true
    fi
    exit 1
}

cleanup_ollama_server() {
    if [ -n "$OLLAMA_SERVER_PID" ]; then
        kill "$OLLAMA_SERVER_PID" 2>/dev/null || true
    fi
}

trap cleanup_ollama_server EXIT

ensure_ollama_server

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

REQUEST_JSON=$(MODEL="$MODEL" INPUT_FILE="$INPUT_FILE" python3 - <<'PY'
import json
import os

model = os.environ.get("MODEL", "")
input_file = os.environ.get("INPUT_FILE", "")
with open(input_file, "r", encoding="utf-8") as f:
    prompt = f.read()

print(json.dumps({
    "model": model,
    "prompt": prompt,
    "stream": False,
    "options": {
        "temperature": 0.0,
        "num_ctx": 4096,
        "keep_alive": 0
    }
}))
PY
)

for ((i=start_from; i<N; i++)); do
    num=$(printf "%02d" "$i")
    output_file="${OUTPUT_PREFIX}${num}"

    # Explicit integer calculation for current count (more reliable in some bash environments)
    current=$((i + 1))

    echo "[${current}/${N}] Generating ${output_file}..."

    if ! response_json=$(curl -sSf -X POST "$OLLAMA_API_URL" \
        -H 'Content-Type: application/json' \
        -d "$REQUEST_JSON"); then
        echo "Error: Ollama API request failed"
        exit 1
    fi

    printf '%s' "$response_json" | python3 - <<'PY' > "$output_file"
import json
import sys

try:
    data = json.load(sys.stdin)
except json.JSONDecodeError as e:
    sys.stderr.write(f"Error: failed to parse Ollama response JSON: {e}\n")
    sys.exit(1)

response = data.get("response", "")
sys.stdout.write(response)
PY

done

echo "----------------------------------------"
echo "Batch completed!"
echo "Created $N files using model: $MODEL"
echo "Files: ${OUTPUT_PREFIX}00 ~ ${OUTPUT_PREFIX}$(printf "%02d" $((N-1)))"
echo "Quick check: ls -1 ${OUTPUT_PREFIX}*"
