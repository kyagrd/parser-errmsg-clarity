#!/bin/bash

# Define the pattern for output files
FILE_PATTERN="\.out[0-9][0-9]$"

# Get a sorted list of subdirectories.
SUBDIRS=$(find . -maxdepth 1 -type d -not -name "." | sort)

# Arrays to store summary and detailed reports
SUMMARY_LINES=()
DETAILED_REPORTS=()

# Iterate over each subdirectory found.
for SUBDIR_PATH in $SUBDIRS; do
    SUBDIR_NAME=$(basename "$SUBDIR_PATH")

    # Use 'find' and pipe to 'awk' for processing files and generating summary for current directory.
    # The awk script now prints a header line first, followed by prefix counts.
    CURRENT_DIR_REPORT=$(find "$SUBDIR_PATH" -maxdepth 1 -type f -printf "%f
" 2>/dev/null | 
        awk '
        BEGIN {
            total_files = 0;
            non_conforming_count = 0;
        }
        {
            total_files++;
            if (match($0, /^(.*)\.out([0-9]{2})$/, m)) {
                prefixes[m[1]]++;
            } else {
                non_conforming_count++;
            }
        }
        END {
            # This is the summary header line for the current directory.
            # It will be the first line of awk output.
            summary_status = (non_conforming_count > 0) ? "has other files" : "no other files";
            print total_files "," summary_status;

            # Print detailed prefix counts.
            # We sort these outside awk for consistent output.
            for (p in prefixes) {
                print p "," prefixes[p];
            }
        }
    ')

    # Parse the CURRENT_DIR_REPORT from awk.
    # The first line is the summary header; the rest are detailed prefix counts.
    HEADER_LINE=$(echo "$CURRENT_DIR_REPORT" | head -n 1)
    DETAILED_PREFIX_LINES=$(echo "$CURRENT_DIR_REPORT" | tail -n +2)

    # Extract total files and status from the header line.
    TOTAL_FILES=$(echo "$HEADER_LINE" | cut -d',' -f1)
    STATUS=$(echo "$HEADER_LINE" | cut -d',' -f2)

    # Store the summary line for the overall summary section.
    SUMMARY_LINES+=("$SUBDIR_NAME: $TOTAL_FILES output files, $STATUS")

    # Build and store the detailed report block for the current directory.
    DETAILED_REPORT_BLOCK="$SUBDIR_NAME: $TOTAL_FILES output files, $STATUS
"
    if [ -n "$DETAILED_PREFIX_LINES" ]; then
        # Sort the detailed prefix lines before adding to the report block.
        while IFS=',' read -r PREFIX COUNT; do
            EMPHASIS=""
            if [ "$COUNT" -ne 100 ]; then
                EMPHASIS=" ***************"
            fi
            printf -v FORMATTED_PREFIX "%-15s" "$PREFIX"
            printf -v FORMATTED_COUNT "%3s" "$COUNT"
            DETAILED_REPORT_BLOCK+="    - ${FORMATTED_PREFIX}: ${FORMATTED_COUNT} files ${EMPHASIS}
"

        done <<< "$(echo "$DETAILED_PREFIX_LINES" | sort)"
    fi
    DETAILED_REPORTS+=("$DETAILED_REPORT_BLOCK")
done

# --- Print Phase ---

# Print the overall summary.
echo "--- Overall Summary ---"
for line in "${SUMMARY_LINES[@]}"; do
    # Example line: "codegemma_7b: 1600 files, no other files"
    # Extract subdirectory name
    SUBDIR_NAME=$(echo "$line" | cut -d':' -f1)
    # Extract the rest (e.g., " 1600 files, no other files")
    REST=$(echo "$line" | cut -d':' -f2-)
    # Extract total files (e.g., "1600")
    TOTAL_FILES=$(echo "$REST" | grep -oE '[0-9]+' | head -n 1)
    # Extract status (e.g., "다른 파일 없음")
    STATUS=$(echo "$REST" | grep -oE '(has|no) other files')

    printf "%-30s: %5s files, %s\n" "$SUBDIR_NAME" "$TOTAL_FILES" "$STATUS"
done
echo "" # Add an empty line for visual separation.

# Print the detailed reports.
echo "--- Detailed Analysis ---"
for report_block in "${DETAILED_REPORTS[@]}"; do
    echo -e "$report_block"
done
