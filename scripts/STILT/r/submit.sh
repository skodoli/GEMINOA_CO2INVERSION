#!/bin/bash
#SBATCH --partition=normal
#SBATCH --job-name=STILT_footprint
#SBATCH --nodes=1
#SBATCH --ntasks=64
#SBATCH --cpus-per-task=1
#SBATCH --mem=128G
#SBATCH --time=15:00:00



# Change to your working directory
cd /media/hdd2/skodoli/STILTR/myproject/r

# Execute your program or command
Rscript run_stilt_try1.r

