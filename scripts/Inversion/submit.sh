#!/bin/bash

# Define the job name prefix
JOB_NAME_PREFIX="est_fluxes_"

# Define the start and end date
START_DATE="20230901"
END_DATE="20230930"


# Iterate over the date range and submit each job
current_date="$START_DATE"
while [[ "$current_date" -le "$END_DATE" ]]; do
    # Define the job name
    JOB_NAME="${JOB_NAME_PREFIX}${current_date}_DAILY.jl"
    
    # Define the SLURM script content
    SLURM_SCRIPT=$(cat << EOF
#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=INV_RUN
#SBATCH --nodes=1
#SBATCH --ntasks=64
#SBATCH --cpus-per-task=1
#SBATCH --mem=128G
#SBATCH --time=12:00:00  # Adjust the time limit as needed

# Load Julia module
module load julia

# Run the Julia script
julia ${JOB_NAME}
EOF
)

    # Write the SLURM script to a temporary file
    SLURM_SCRIPT_FILE="slurm_${JOB_NAME}.sh"
    echo "$SLURM_SCRIPT" > "$SLURM_SCRIPT_FILE"

    # Submit the SLURM job
    sbatch "$SLURM_SCRIPT_FILE"

    # Increment the date
    current_date=$(date -d "$current_date + 1 day" +"%Y%m%d")
done

