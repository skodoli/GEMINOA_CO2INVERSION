#!/bin/bash

# Directory containing the Julia files
DIRECTORY="/mnt/data2/skodoli/INVERSION/BEACON_inv_tr/Util"

# Find all Julia files in the directory and subdirectories
find "$DIRECTORY" -name "*.jl" -type f | while read -r FILE; do
    echo "Processing file: $FILE"
    
    # Use sed to remove lines containing "NetCDF.close"
    sed -i '/NetCDF.close/d' "$FILE"
    
    echo "Removed NetCDF.close from: $FILE"
done

echo "Completed removing NetCDF.close from all Julia files in $DIRECTORY."

