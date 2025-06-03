#!/bin/bash

# Specify the folder containing the files
folder_path="/mnt/data2/skodoli/INVERSION/BEACON_inv_tr/STILT_out/obs"

# Change to the specified directory
cd "$folder_path" || exit

# Iterate over the files matching the pattern
for file in obs_*00_-4*.nc; do
    # Check if the filename matches the pattern obs_*00_-4*.nc
    if [[ "$file" =~ ^(obs_.*[0-9]{2})00(_-4.*\.nc)$ ]]; then
        # Create new filename by removing the '00' before '_-4'
        new_file="${BASH_REMATCH[1]}${BASH_REMATCH[2]}"
        
        # Rename the file
        mv "$file" "$new_file"
        echo "Renamed $file to $new_file"
    fi
done

