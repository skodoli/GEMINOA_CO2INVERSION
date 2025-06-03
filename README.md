##########################################################################################################
# GEMINOA Inversion Model

This repository contains code and sample data for inverse modeling of urban CO₂ emissions using the STILT model, WRF outputs, and flux optimization routines developed for the GEMINOA project in Glasgow.

## Features

- Preprocessing of WRF and STILT data
- Flux optimization using Bayesian inversion
- Example configuration and sample run

## Repository Structure

- `data/`: Sample input files (small)
- `src/`: Scripts for preprocessing and inversion
- `notebooks/`: Example notebook to run a full case
- `config/`: Model and run-time configuration

## Getting Started




########################################################################################################




#### **Building Emissions**


#### **Make NetCDF**

| Script Name     | Description                                         | Notes                           | 
| --------------- | --------------------------------------------------- | ------------------------------- |
| `Make_netCDF.py` | Combines Beacon files into one NetCDF receptor file |  script  |

---

#### **Run STILT**

| Script Name    | Description                      | Notes |                                           
| -------------- | -------------------------------- | ----- | 
| `run_BEACON.r` | Runs STILT, generates footprints |   Need modifications of STILT model source files before run| 

---

#### **Make Observation Files**

| Script Name          | Description                                     |                                                   |
| -------------------- | ----------------------------------------------- | 
| `build_background.r` | Combines footprints and background observations |      

---

#### **Run Inversion**

| Script Name              | Description                                                      | Notes                                                                                                            
| ------------------------ | ---------------------------------------------------------------- | ---------------------------------------------------------- |  
| `linkDat.csh`            | Creates symbolic links for emissions and observation input files | Edit paths for emissions, observations, and base directory | 
| `template_est_fluxes.jl` | Core inversion script (Julia), replicated per hour               | Set inversion params: error, grid size, cross-validation   |  
| `make_scripts.csh`       | Generates batch scripts for inversion                            | Handles replication and cross-validation setup             |
| `run_script.csh`         | Runs all inversion jobs via cluster submission                   | Produces logs, run using `./run_script.csh`                |

#############################################################################################################################
