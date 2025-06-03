#!/bin/csh -f

### Specify the base directory
set baseDir = "/mnt/data2/skodoli/INVERSION/BEACON_inv_tr"
#set obsDir  = "/clusterfs/aiolos/aturner/BEACON_obs_files"
set obsDir  = "/mnt/data2/skodoli/INVERSION/BEACON_inv_tr/STILT_out/obs"
#set emsDir  = "/clusterfs/aiolos/aturner/Emissions"
set emsDir  = "/mnt/data2/skodoli/INVERSION/BEACON_inv_tr/ems/merged/"

### Define the link pattern
set yrPat  = "20??"
set monPat = "??"
set dayPat = "??"
set hrPat  = "??"
# Obs
set timPat = "${yrPat}${monPat}${dayPat}${hrPat}"
set obsVar = "obs_${timPat}_*.nc"
# Emissions
set timPat = "${yrPat}x${monPat}x${dayPat}x${hrPat}"
set emsVar = "MERGED_UKGHG_${timPat}.nc"

### Remove the files
#rm $baseDir/.
#rm $baseDir/.

### Link the obs
ln -s ${obsDir}/$obsVar ${baseDir}/.
echo 'There are '`ls ${baseDir}/obs/obs_*.nc | wc -l`' obs files'

### Link the emissions
# Standard
ln -s ${emsDir}/$emsVar ${baseDir}/.
echo 'There are '`ls ${baseDir}/prior/*.nc | wc -l`' ems files'
# Double
#ln -s ${baseDir}/ems_2x/full_ems/$emsVar ${baseDir}/ems_2x/.
#ln -s ${emsDir}/turner_2x/$emsVar ${baseDir}/ems/.
#echo 'There are '`ls ${baseDir}/ems_2x/*.ncdf | wc -l`' ems_2x files'

exit(0)
