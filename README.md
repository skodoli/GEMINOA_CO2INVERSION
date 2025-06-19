##########################################################################################################
# GEMINOA Inversion Model

This repository contains code and sample data for inverse modeling of urban CO₂ emissions using the STILT model, WRF outputs, and flux optimization routines developed for the GEMINOA project in Glasgow.

## Features

- Preprocessing of WRF and STILT data
- Flux optimization using Bayesian inversion
- Example configuration and sample run

## Repository Structure

-  Sample input files (small)
-  Scripts for preprocessing and inversion
-  Example notebook to run a full case
-  Model and run-time configuration

## Getting Started

#WRF Compilation

Remember to modify the Registry File (Registry.EM_COMMON) and WRF namelist file
1.2. Activate "history output" for some additional variables in the Registry/Registry.EM file (Registry.EM_COMMON in v3.4).

This will require recompilation. The variables are:

muu, muv,mut, alt.

Also recommended: tke (this is not strictly necessary for the WRF-STILT interface, but potentially useful when using PBL schemes that compute TKE).

Sample lines shown below:

state    real   muu             ij     dyn_em      1          -     hr          "muu"
state    real   muv             ij     dyn_em      1          -     hr          "muv"
state    real   mut             ij     dyn_em      1          -     hr          "mut"
state    real   alt            ikj     dyn_em      1         -      rh        "alt"         "inverse density"               "m3 kg-1



 Add two lines to the namelist file, to the "&dynamics" namelist:

 do_avgflx_em                        = 1, 1, 1, 1, 1, 1, 1, 1, 1,
 do_avgflx_cugd                      = 1, 1, 1, 1, 1, 1, 1, 1, 1,


########################################################################################################

##STILT Installation and Modification

Ensure the UATAQ R package is up to date
Rscript -e "install.packages('devtools'); devtools::install_github('benfasoli/uataq')"

Initialize STILT project using the uataq::stilt_init() R function
Rscript -e "uataq::stilt_init('myproject')"

##Modification of STILT source code
Replace 'calc_footprint.r', 'calc_trajectory.r' and 'write_footprint.r' files with the files with same name  provided in STILT folder to match with grid settings of prior emission inventory data.

### 












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
