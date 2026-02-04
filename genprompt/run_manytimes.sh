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

# Setup API configuration
OLLAMA_API_BASE="http://localhost:11434/api"
OLLAMA_API_URL="$OLLAMA_API_BASE/generate"
OLLAMA_SERVER_PID=""

ensure_ollama_server() {
    # Check if server is already running
    if curl -sf "$OLLAMA_API_BASE/tags" >/dev/null 2>&1; then
        echo "Ollama server is already running"
        return 0
    fi

    # Kill any existing ollama serve processes
    pkill -f "ollama serve" 2>/dev/null || true
    sleep 2

    echo "Starting Ollama server with OLLAMA_NUM_PARALLEL=5..."
    OLLAMA_NUM_PARALLEL=5 ollama serve > /tmp/ollama-serve.log 2>&1 &
    OLLAMA_SERVER_PID=$!
    echo "Ollama server PID: $OLLAMA_SERVER_PID"

    # Wait for server to be ready
    echo "Waiting for Ollama server to be ready..."
    for i in {1..60}; do
        if curl -sf "$OLLAMA_API_BASE/tags" >/dev/null 2>&1; then
            echo "Ollama server is ready!"
            return 0
        fi
        echo "  [$i/60] Waiting..."
        sleep 1
    done

    echo "Error: Ollama server did not become ready in time."
    if [ -n "$OLLAMA_SERVER_PID" ]; then
        kill "$OLLAMA_SERVER_PID" 2>/dev/null || true
    fi
    tail -20 /tmp/ollama-serve.log
    exit 1
}

cleanup_ollama_server() {
    if [ -n "$OLLAMA_SERVER_PID" ]; then
        echo "Cleaning up Ollama server (PID: $OLLAMA_SERVER_PID)"
        kill "$OLLAMA_SERVER_PID" 2>/dev/null || true
    fi
}

trap cleanup_ollama_server EXIT

# Build list of files that need to be generated
files_to_generate=()

if [ "$FORCE" = false ]; then
    # Get modification time of the input file (prompt)
    INPUT_MTIME=$(stat -c %Y "$INPUT_FILE" 2>/dev/null || stat -f %m "$INPUT_FILE" 2>/dev/null)
    
    for ((i=0; i<N; i++)); do
        num=$(printf "%02d" "$i")
        output_file="${OUTPUT_PREFIX}${num}"
        
        if [ -f "$output_file" ]; then
            # Check if file is outdated (older than input file)
            OUTPUT_MTIME=$(stat -c %Y "$output_file" 2>/dev/null || stat -f %m "$output_file" 2>/dev/null)
            if [ "$OUTPUT_MTIME" -lt "$INPUT_MTIME" ]; then
                files_to_generate+=($i)
            fi
        else
            # File doesn't exist, needs to be generated
            files_to_generate+=($i)
        fi
    done
else
    # Force mode: generate all files
    for ((i=0; i<N; i++)); do
        files_to_generate+=($i)
    done
fi

if [ ${#files_to_generate[@]} -eq 0 ]; then
    echo "All files are up-to-date. Nothing to do."
    exit 0
fi

echo "Files to generate: ${#files_to_generate[@]} out of $N"
echo "----------------------------------------"

# Pull model if not already available
echo "Checking if model '$MODEL' is available..."
if ! ollama show "$MODEL" >/dev/null 2>&1; then
    echo "Model '$MODEL' not found. Pulling it now..."
    if ! ollama pull "$MODEL"; then
        echo "Error: Failed to pull model '$MODEL'"
        exit 1
    fi
    echo "Model '$MODEL' pulled successfully"
else
    echo "Model '$MODEL' is already available"
fi

# Ensure Ollama server is running
ensure_ollama_server

# Helper function to process a single iteration
process_iteration() {
    local i=$1
    local N=$2
    local INPUT_FILE=$3
    local OUTPUT_PREFIX=$4
    local MODEL=$5
    local OLLAMA_API_URL=$6
    
    num=$(printf "%02d" "$i")
    output_file="${OUTPUT_PREFIX}${num}"
    temp_output_file="${output_file}.tmp$$-$RANDOM"
    temp_json_file=$(mktemp)

    # Explicit integer calculation for current count (more reliable in some bash environments)
    current=$((i + 1))

    echo "[${current}/${N}] Generating ${output_file}..."
    
    # Small delay to avoid overwhelming the server
    sleep 0.1

    # Create JSON request file
    python3 <<PYSCRIPT > "$temp_json_file"
import json

with open('$INPUT_FILE', 'r', encoding='utf-8') as f:
    prompt = f.read()

data = {
    'model': '$MODEL',
    'prompt': prompt,
    'stream': False,
    'options': {
        'temperature': 0.0,
        'num_ctx': 4096
    }
}
print(json.dumps(data))
PYSCRIPT

    # Send request and capture response with error details
    temp_response_file=$(mktemp)
    http_code=$(curl -s -w "%{http_code}" -o "$temp_response_file" -X POST "$OLLAMA_API_URL" \
        -H 'Content-Type: application/json' \
        -d @"$temp_json_file" 2>&1)
    
    rm -f "$temp_json_file"
    
    # Debug output
    response_size=$(wc -c < "$temp_response_file")
    echo "  [DEBUG] HTTP Code: $http_code, Response size: $response_size" >&2
    
    # Check HTTP response code
    if [ "$http_code" != "200" ]; then
        echo "Error: HTTP $http_code for ${output_file}" >&2
        head -c 200 "$temp_response_file" | tr '\n' ' ' >&2
        echo "" >&2
        rm -f "$temp_response_file"
        return 1
    fi
    
    # Check if response is empty
    if [ "$response_size" -eq 0 ]; then
        echo "Error: Empty response from Ollama API for ${output_file}" >&2
        rm -f "$temp_response_file"
        return 1
    fi

    # Parse JSON response using jq to extract the "response" field
    if ! response_text=$(jq -r '.response' "$temp_response_file" 2>/dev/null); then
        echo "Error: Failed to parse JSON response for ${output_file}" >&2
        rm -f "$temp_response_file"
        return 1
    fi
    
    rm -f "$temp_response_file"
    
    # Write response to temp output file
    printf '%s' "$response_text" > "$temp_output_file"


    if [ $? -eq 0 ]; then
        mv "$temp_output_file" "$output_file"
    else
        echo "Error: Failed to process response for ${output_file}" >&2
        rm -f "$temp_output_file"
        return 1
    fi
}

export -f process_iteration
export N INPUT_FILE OUTPUT_PREFIX MODEL OLLAMA_API_URL

# Use GNU parallel to run iterations in parallel
# Use 8 parallel jobs for faster processing
printf '%s\n' "${files_to_generate[@]}" | parallel -j8 process_iteration {} $N "$INPUT_FILE" "$OUTPUT_PREFIX" "$MODEL" "$OLLAMA_API_URL"
if [ $? -ne 0 ]; then
    echo "Error: Parallel execution failed"
    exit 1
fi

echo "----------------------------------------"
echo "Batch completed!"
echo "Generated ${#files_to_generate[@]} files using model: $MODEL"
echo "Files: ${OUTPUT_PREFIX}00 ~ ${OUTPUT_PREFIX}$(printf "%02d" $((N-1)))"
echo "Quick check: ls -1 ${OUTPUT_PREFIX}*"
