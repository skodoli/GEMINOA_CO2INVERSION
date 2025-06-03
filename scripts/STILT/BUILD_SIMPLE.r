# Very simple script to add a background (simultaneous to observation) from a csv file to make obs files
# 03/06/2020 by AJT

# Multi-threading parameters
nSeg   <- 1
my_num <- 1

# STILT Parameters
# User inputs ------------------------------------------------------------------
stilt_wd <- file.path('/mnt/data2/skodoli/myproject_LPDM')
output_wd <- file.path(stilt_wd, 'out')
lib.loc <- .libPaths()[1]

# Load the necessary libraries
setwd(stilt_wd)
source('r/dependencies.r')
require(ncdf4)
require(gtools)
require(fields)

# Overwrite old obs files?
overwrite = TRUE

# Find all CO2 observations that have a footprint
# Load the receptor file
origin <- as.Date("1970-1-1")
ncfile <- file.path(stilt_wd, 'rec_sepoct23_lez.nc')
nc <- nc_open(ncfile)
fjul <- ncvar_get(nc, varid="fjul")
lati <- ncvar_get(nc, varid="lat")
long <- ncvar_get(nc, varid="lon")
zagl <- ncvar_get(nc, varid="agl")
co2 <- ncvar_get(nc, varid="co2")
co2e <- ncvar_get(nc, varid="co2e")
yr <- ncvar_get(nc, varid="yr")
mon <- ncvar_get(nc, varid="mon")
day <- ncvar_get(nc, varid="day")
hr <- ncvar_get(nc, varid="hr")
nc_close(nc)

# Convert time format
yyyymmddHH <- rep("yyyymmdd HH", length(co2))
for (i in 1:length(yyyymmddHH)) {
  yyyymmddHH[i] = sprintf("%04i/%02i/%02i %02i", yr[i], mon[i], day[i], hr[i])
}
run_times <- as.POSIXct(yyyymmddHH, tz = 'UTC', format = "%Y/%m/%d %H")

# Filter for unique combinations
rID <- strftime(run_times, paste0('%Y%m%d%H00_', long, '_', lati, '_', zagl), 'UTC')
uID <- !duplicated(rID)
rID <- rID[uID]
run_times <- run_times[uID]
fjul <- fjul[uID] * 24*60*60  # Convert to seconds since 1970-01-01 00:00:00Z
lati <- lati[uID]
long <- long[uID]
zagl <- zagl[uID]
co2A <- co2
co2 <- co2[uID]
co2e <- co2e[uID]

# Get an index for each observation that has a footprint
oMatch = logical(length(rID))
for (i in 1:length(oMatch)) {
  oMatch[i] = file.exists(paste(output_wd, '/footprints/', rID[i], '_foot.nc', sep=""))
}

# Trim our structure down
rID <- rID[oMatch]
run_times <- run_times[oMatch]
fjul <- fjul[oMatch]
lati <- lati[oMatch]
long <- long[oMatch]
zagl <- zagl[oMatch]
co2 <- co2[oMatch]
co2e <- co2e[oMatch]

# Load the Background Data File, we will save this in the amf background field
background_file <- read.csv(file = file.path(stilt_wd, 'co2_background_hourly_23.csv'))

# Loop through the observations and find the background
print(" ")
print(" *** MAKING OBS FILES ***")
obsInd_list <- 1:length(rID)
splitList <- split(obsInd_list, sort(obsInd_list %% nSeg))
myInds <- sample(splitList[[my_num]])  # Shuffle them so we can run multiple at once

for (i in myInds) {
  print(sprintf(" #%6i/%6i: (%s)", i, length(rID), rID[i]))
  outName <- paste(output_wd, '/obs/obs_', rID[i], '.nc', sep="")
  print(outName)
  
  if (!file.exists(outName) | overwrite) {
    out <- tryCatch({
      # Receptor information
      obs_yr <- as.numeric(strftime(run_times[i], "%Y", tz='UTC'))
      obs_mon <- as.numeric(strftime(run_times[i], "%m", tz='UTC'))
      obs_day <- as.numeric(strftime(run_times[i], "%d", tz='UTC'))
      obs_hr <- as.numeric(strftime(run_times[i], "%H", tz='UTC'))
      obs_jul <- fjul[i]
      obs_lon <- long[i]
      obs_lat <- lati[i]
      obs_agl <- zagl[i]
      obs_co2 <- co2[i]
      obs_err <- co2e[i]
      
      # Get data from the footprint file
      ncfile <- nc_open(paste(output_wd, '/footprints/', rID[i], '_foot.nc', sep=""))
      print(paste(output_wd, '/footprints/', rID[i], '_foot.nc', sep=""))
      lon_vec <- ncvar_get(ncfile, varid='lon')
      lat_vec <- ncvar_get(ncfile, varid='lat')
      foot <- ncvar_get(ncfile, varid='foot')
      ftimes <- ncvar_get(ncfile, varid='time')
      nc_close(ncfile)
      
      # Placeholder fields for inversion
      bkg_val_NOAA <- -999
      bkg_err_NOAA <- -999
      bkg_val_NASA <- -999
      bkg_err_NASA <- -999
      end_lon <- -999
      end_lat <- -999
      end_agl <- -999
      amf_lon <- -999
      amf_lat <- -999
      amf_agl <- -999
      amf_tim <- -999
      
      # Use same amf_tim as obs_tim, get the ID of this line
      amfTID <- which.min(abs(background_file$jdate - obs_jul))
      amf_tim <- background_file$jdate[amfTID]
      
      # Get the co2 value and background
      amf_co2 <- background_file$bkg_co2[amfTID]
      amf_err <- background_file$bkg_err[amfTID]
      
      # Create netCDF dimensions and variables
      dimINFO <- ncdim_def("info", "na", 1)
      dimTIME <- ncdim_def("time", "seconds since 1970-01-01 00:00:00Z", ftimes)
      dimLON <- ncdim_def("lon", "degrees_east", lon_vec)
      dimLAT <- ncdim_def("lat", "degrees_north", lat_vec)
      
      var_foot <- ncvar_def("foot", "ppm/(umol*m-2*s-1)", compression=9, list(dimLON, dimLAT, dimTIME), -999, longname="stilt surface influence footprint")
      var_co2 <- ncvar_def("co2", "ppm", dimINFO, -999, longname="dry air mole mixing ratio at the receptor")
      var_co2_err <- ncvar_def("co2_err", "ppm", dimINFO, -999, longname="Error in the dry air mole mixing ratio at the receptor")
      var_bkg_NOAA <- ncvar_def("bkg_co2_NOAA", "ppm", dimINFO, -999, longname="dry air mole mixing ratio at the NOAA curtain")
      var_bkg_err_NOAA <- ncvar_def("bkg_err_NOAA", "ppm", dimINFO, -999, longname="Error in the dry air mole mixing ratio at the NOAA curtain")
      var_bkg_NASA <- ncvar_def("bkg_co2_NASA", "ppm", dimINFO, -999, longname="dry air mole mixing ratio from the NASA CMS run")
      var_bkg_err_NASA <- ncvar_def("bkg_err_NASA", "ppm", dimINFO, -999, longname="Error in the dry air mole mixing ratio from the NASA CMS run")
      var_jul <- ncvar_def("jul", "seconds since 1970-01-01 00:00:00Z", dimINFO, -999, longname="julian date")
      var_yr <- ncvar_def("yr", "yyyy", dimINFO, -999, longname="stilt back trajectory start time")
      var_mon <- ncvar_def("mon", "mm", dimINFO, -999, longname="stilt back trajectory start time")
      var_day <- ncvar_def("day", "dd", dimINFO, -999, longname="stilt back trajectory start time")
      var_hr <- ncvar_def("hr", "hh", dimINFO, -999, longname="stilt back trajectory start time")
      var_obs_lat <- ncvar_def("obs_lat", "degrees_north", dimINFO, -999, longname="stilt back trajectory start latitude")
      var_obs_lon <- ncvar_def("obs_lon", "degrees_east", dimINFO, -999, longname="stilt back trajectory start longitude")
      var_obs_agl <- ncvar_def("obs_agl", "m AGL", dimINFO, -999, longname="stilt back trajectory start altitude")
      var_end_lat <- ncvar_def("end_lat", "degrees_north", dimINFO, -999, longname="stilt back trajectory end latitude")
      var_end_lon <- ncvar_def("end_lon", "degrees_east", dimINFO, -999, longname="stilt back trajectory end longitude")
      var_end_agl <- ncvar_def("end_agl", "m AGL", dimINFO, -999, longname="stilt back trajectory end altitude")
      var_amf_lat <- ncvar_def("ameriflux_lat", "degrees_north", dimINFO, -999, longname="stilt back trajectory latitude closest to AmeriFlux")
      var_amf_lon <- ncvar_def("ameriflux_lon", "degrees_east", dimINFO, -999, longname="stilt back trajectory longitude closest to AmeriFlux")
      var_amf_agl <- ncvar_def("ameriflux_agl", "m AGL", dimINFO, -999, longname="stilt back trajectory altitude closest to AmeriFlux")
      var_amf_tim <- ncvar_def("ameriflux_julian", "seconds since 1970-01-01 00:00:00Z", dimINFO, -999, longname="stilt back trajectory julian date closest to AmeriFlux")
      var_amf_co2 <- ncvar_def("ameriflux_co2", "ppm", dimINFO, -999, longname="AmeriFlux CO2 concentration for closest stilt back trajectory")
      var_amf_err <- ncvar_def("ameriflux_err", "ppm", dimINFO, -999, longname="AmeriFlux CO2 uncertainty for closest stilt back trajectory")
      
      # Create and write to netCDF file
      ncid <- nc_create(outName, force_v4=TRUE, list(var_co2, var_co2_err, var_bkg_NOAA, var_bkg_err_NOAA, var_bkg_NASA, var_bkg_err_NASA, var_yr, var_mon, var_day, var_hr, var_jul, var_obs_lat, var_obs_lon, var_obs_agl, var_end_lon, var_end_lat, var_end_agl, var_amf_lon, var_amf_lat, var_amf_agl, var_amf_tim, var_amf_co2, var_amf_err, var_foot))
      nc_close(ncid)
      ncid <- nc_open(outName, write=TRUE)
      
      # Add the variables
      ncvar_put(ncid, var_yr, obs_yr)
      ncvar_put(ncid, var_mon, obs_mon)
      ncvar_put(ncid, var_day, obs_day)
      ncvar_put(ncid, var_hr, obs_hr)
      ncvar_put(ncid, var_jul, obs_jul)
      ncvar_put(ncid, var_obs_lon, obs_lon)
      ncvar_put(ncid, var_obs_lat, obs_lat)
      ncvar_put(ncid, var_obs_agl, obs_agl)
      ncvar_put(ncid, var_end_lon, end_lon)
      ncvar_put(ncid, var_end_lat, end_lat)
      ncvar_put(ncid, var_end_agl, end_agl)
      ncvar_put(ncid, var_foot, foot)
      ncvar_put(ncid, var_co2, obs_co2)
      ncvar_put(ncid, var_co2_err, obs_err)
      ncvar_put(ncid, var_bkg_NOAA, bkg_val_NOAA)
      ncvar_put(ncid, var_bkg_err_NOAA, bkg_err_NOAA)
      ncvar_put(ncid, var_bkg_NASA, bkg_val_NASA)
      ncvar_put(ncid, var_bkg_err_NASA, bkg_err_NASA)
      ncvar_put(ncid, var_amf_lon, amf_lon)
      ncvar_put(ncid, var_amf_lat, amf_lat)
      ncvar_put(ncid, var_amf_agl, amf_agl)
      ncvar_put(ncid, var_amf_tim, amf_tim)
      ncvar_put(ncid, var_amf_co2, amf_co2)
      ncvar_put(ncid, var_amf_err, amf_err)
      nc_close(ncid)
      
      # If successful, return 1
      1
    }, error = function(cond) {
      message("Something went wrong, here's the message:")
      message(cond)
      # If we get an error, return 0
      0
    }, warning = function(cond) {
      message("Warning:")
      message(cond)
      # If we get a warning, return NULL
      NULL
    })
    
    out
  }
}

# End of script

