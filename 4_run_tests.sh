#!/bin/bash

# This script finds all files with the ".R" extension in the specified 'statistics' folder,
# sorts them alphabetically, and then executes each one using Rscript.
# It explicitly excludes the file named "not_this_file.R".

TARGET_FOLDER="statistics"
EXCLUDE_FILE="0_packages_n_global_variables.R"

echo "Searching for .R files in the '$TARGET_FOLDER' folder, excluding '$EXCLUDE_FILE'..."

# Find all .R files in the target folder, sort them alphabetically,
# and store them in an array.
# The 'find' command with maxdepth 1 limits the search to the specified folder only.
# The '-not -name "$EXCLUDE_FILE"' part excludes the specific file.
# The 'sort' command ensures alphabetical order.
r_files=($(find "$TARGET_FOLDER" -maxdepth 1 -name "*.R" -not -name "$EXCLUDE_FILE" | sort))

# Check if any .R files were found
if [ ${#r_files[@]} -eq 0 ]; then
  echo "No .R files found in the '$TARGET_FOLDER' folder (after exclusions)."
else
  echo "Found ${#r_files[@]} .R file(s). Executing them in alphabetical order:"
  # Loop through each found .R file
  for r_file in "${r_files[@]}"; do
    # The filename already includes the folder path (e.g., statistics/my_script.R)
    # No need to remove leading './' as find directly outputs 'folder_name/file.R'
    echo "--- Running Rscript for: $r_file ---"
    # Execute the R script
    Rscript "$r_file"

    # Check the exit status of the Rscript command
  if [ $? -eq 0 ]; then
      echo "--- Successfully executed $r_file ---"
    else
      echo "--- Error executing $r_file. Check Rscript output above. ---"
    fi
    echo "" # Add a blank line for readability between runs
  done
fi

echo "Script finished."