#!/bin/bash

# Usage: ./obfuscate_symbols.sh <input.yml> <output.yml> [-s <original> <replacement>] ...
# Examples:
#   Single symbol pair:
#     ./obfuscate_symbols.sh input.yml output.yml -s '{' '<<'
#   Multiple symbol pairs:
#     ./obfuscate_symbols.sh input.yml output.yml -s '{' '<<' -s '}' '>>' -s '[' '<['
#   Legacy compatibility (assumes { -> replacement and } -> replacement):
#     ./obfuscate_symbols.sh input.yml output.yml '<<' '>>'

# Parse command line arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <input.yml> <output.yml> [-s <original> <replacement>] ..."
    echo ""
    echo "Examples:"
    echo "  Single pair: $0 input.yml output.yml -s '{' '<<'"
    echo "  Multiple pairs: $0 input.yml output.yml -s '{' '<<' -s '}' '>>'"
    echo "  Legacy (two args): $0 input.yml output.yml '<<' '>>'"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_FILE="$2"

if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: Input file '$INPUT_FILE' not found"
    exit 1
fi

# Array to store symbol pairs
declare -a SYMBOLS_FROM
declare -a SYMBOLS_TO

# Parse arguments
shift 2  # Remove input and output file arguments

# Legacy mode: if next two arguments don't start with -s, treat as old format
if [ $# -eq 2 ] && [[ "$1" != "-s" ]]; then
    # Legacy mode: assume { -> $1 and } -> $2
    SYMBOLS_FROM+=("{")
    SYMBOLS_TO+=("$1")
    SYMBOLS_FROM+=("}")
    SYMBOLS_TO+=("$2")
    echo "Legacy mode: { -> $1, } -> $2"
else
    # New mode: parse -s pairs
    while [ $# -gt 0 ]; do
        if [ "$1" = "-s" ]; then
            if [ $# -lt 3 ]; then
                echo "Error: -s flag requires two arguments (original and replacement)"
                exit 1
            fi
            SYMBOLS_FROM+=("$2")
            SYMBOLS_TO+=("$3")
            shift 3
        else
            echo "Error: Unknown argument '$1'. Use -s <original> <replacement>"
            exit 1
        fi
    done
fi

# Validate that we have at least one symbol pair
if [ ${#SYMBOLS_FROM[@]} -eq 0 ]; then
    echo "Error: No symbol pairs specified"
    exit 1
fi

# Sanity check: validate symbol pairs
validate_symbols() {
    local -n from_array=$1
    local -n to_array=$2
    
    # Check for duplicates in SYMBOLS_FROM
    local -a checked_from
    for symbol in "${from_array[@]}"; do
        for checked in "${checked_from[@]}"; do
            if [ "$symbol" = "$checked" ]; then
                echo "Error: Duplicate symbol in source (FROM): '$symbol'"
                exit 1
            fi
        done
        checked_from+=("$symbol")
    done
    
    # Check for duplicates in SYMBOLS_TO
    local -a checked_to
    for symbol in "${to_array[@]}"; do
        for checked in "${checked_to[@]}"; do
            if [ "$symbol" = "$checked" ]; then
                echo "Error: Duplicate symbol in target (TO): '$symbol'"
                exit 1
            fi
        done
        checked_to+=("$symbol")
    done
    
    # Check for overlap between SYMBOLS_FROM and SYMBOLS_TO
    for from_sym in "${from_array[@]}"; do
        for to_sym in "${to_array[@]}"; do
            if [ "$from_sym" = "$to_sym" ]; then
                echo "Error: Symbol '$from_sym' appears in both FROM and TO lists"
                exit 1
            fi
        done
    done
}

validate_symbols SYMBOLS_FROM SYMBOLS_TO

# Process the YAML file line by line
in_errmsg=0
in_code=0
in_more=0

while IFS= read -r line; do
    # Check for section markers
    if [[ "$line" == "---" ]]; then
        in_errmsg=0
        in_code=0
        in_more=0
        echo "$line"
    elif [[ "$line" =~ ^[a-zA-Z_]+:.*$ ]]; then
        # Any field with colon (errmsg:, code:, more:, etc.)
        # Reset flags first
        in_errmsg=0
        in_code=0
        in_more=0
        
        # Then set appropriate flag and replace if needed
        if [[ "$line" == "errmsg:"* ]]; then
            in_errmsg=1
            # Apply all symbol replacements
            for i in "${!SYMBOLS_FROM[@]}"; do
                line=$(echo "$line" | sed "s/${SYMBOLS_FROM[$i]}/${SYMBOLS_TO[$i]}/g")
            done
        elif [[ "$line" == "code:"* ]]; then
            in_code=1
            # Apply all symbol replacements
            for i in "${!SYMBOLS_FROM[@]}"; do
                line=$(echo "$line" | sed "s/${SYMBOLS_FROM[$i]}/${SYMBOLS_TO[$i]}/g")
            done
        elif [[ "$line" == "more:"* ]]; then
            in_more=1
            # Apply all symbol replacements
            for i in "${!SYMBOLS_FROM[@]}"; do
                line=$(echo "$line" | sed "s/${SYMBOLS_FROM[$i]}/${SYMBOLS_TO[$i]}/g")
            done
        fi
        echo "$line"
    else
        # If we're inside errmsg, code, or more section, replace symbols
        if [ $in_errmsg -eq 1 ] || [ $in_code -eq 1 ] || [ $in_more -eq 1 ]; then
            # Apply all symbol replacements
            for i in "${!SYMBOLS_FROM[@]}"; do
                line=$(echo "$line" | sed "s/${SYMBOLS_FROM[$i]}/${SYMBOLS_TO[$i]}/g")
            done
        fi
        echo "$line"
    fi
done < "$INPUT_FILE" > "$OUTPUT_FILE"

# Print summary
echo "Successfully created '$OUTPUT_FILE' with the following replacements:"
for i in "${!SYMBOLS_FROM[@]}"; do
    echo "  '${SYMBOLS_FROM[$i]}' -> '${SYMBOLS_TO[$i]}'"
done
