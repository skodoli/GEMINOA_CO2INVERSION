#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=STILT_footprint
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=128G
#SBATCH --time=100:00:00
#SBATCH --output=STILT_footprint_%j.out
#SBATCH --error=STILT_footprint_%j.err

# Load any necessary modules (adjust as needed for your system)
#module load R/4.1.0

# Set environment variables for better R performance
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK

# Change to your working directory
cd /mnt/data2/skodoli/myproject_LPDM/r

# Execute your R script
Rscript RUN_BEACON.r
