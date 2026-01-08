#!/bin/bash
# File: run_manytimes.sh
# Usage:  ./run_manytimes.sh  "input.txt"  "output_prefix"  50

# ===================== CONFIGURATION =====================
# Change the model here whenever needed
MODEL="codegemma:2b"              # ← Modify this line (e.g. qwen2.5:32b, llama3.1:70b, etc)

OLLAMA_CMD="ollama run $MODEL"
# =========================================================

# Check required arguments
if [ $# -lt 3 ]; then
    echo "Usage: $0  INPUT_FILE  OUTPUT_PREFIX  NUMBER_OF_RUNS"
    echo ""
    echo "  INPUT_FILE      : prompt file to feed to ollama"
    echo "  OUTPUT_PREFIX   : prefix for output files (will add 00,01,02...)"
    echo "  NUMBER_OF_RUNS  : how many times to run (1~100)"
    echo ""
    echo "Current model: $MODEL"
    echo ""
    echo "Examples:"
    echo "  ./run_manytimes.sh  sqbracket1.txt          sqbracket1          50"
    echo "  ./run_manytimes.sh  prompts/math.txt        math-result-v1      30"
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
echo "----------------------------------------"

for ((i=0; i<N; i++)); do
    num=$(printf "%02d" "$i")
    output_file="${OUTPUT_PREFIX}${num}"

    # Explicit integer calculation for current count (more reliable in some bash environments)
    current=$((i + 1))

    echo "[${current}/${N}] Generating ${output_file}..."

    # Current setting: stderr is NOT redirected
    # → ollama progress bars, loading animations, etc. will appear on the terminal screen
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
