
project <- 'myproject'
stilt_wd <- file.path('/mnt/data2/skodoli/myproject_LPDM/')
output_wd <- file.path(stilt_wd, 'out')
lib.loc <- .libPaths()[1]
#source('/media/hdd2/skodoli/STILTR/myproject/r/dependencies.r')
library(ncdf4)
require(pracma)
library(parallel)

# Simulation timing, yyyy-mm-dd HH:MM:SS (UTC)
origin <- as.Date("1970-1-1")
ncfile <- nc_open(paste('/mnt/data2/skodoli/Receptor_file_BEACON_oxna/rec_sepoct23_lez.nc'))
lati <- ncvar_get(ncfile, varid="lat")
long <- ncvar_get(ncfile, varid="lon")
zagl <- ncvar_get(ncfile, varid="agl")
co2 <- ncvar_get(ncfile, varid="co2")
co2e <- ncvar_get(ncfile, varid="co2e")
yr <- ncvar_get(ncfile, varid="yr")
mon <- ncvar_get(ncfile, varid="mon")
day <- ncvar_get(ncfile, varid="day")
hr <- ncvar_get(ncfile, varid="hr")
nc_close(ncfile)

yyyymmddHH <- rep("yyyymmdd HH", length(co2))
for (i in 1:length(yyyymmddHH)) {
   yyyymmddHH[i] = sprintf("%04i/%02i/%02i %02i", yr[i], mon[i], day[i], hr[i])
}
run_times <- as.POSIXct(yyyymmddHH, tz = 'UTC', format = "%Y/%m/%d %H")
rID <- strftime(run_times, paste0('%Y%m%d%H_', long, '_', lati, '_', zagl), 'UTC')
uID <- !duplicated(rID)
rID <- rID[uID]
run_times <- run_times[uID]
lati <- lati[uID]
long <- long[uID]
zagl <- zagl[uID]
co2 <- co2[uID]
co2e <- co2e[uID]

receptors <- data.frame(run_time = run_times, lati = lati, long = long, zagl = zagl, co2 = co2, co2e = co2e)

# Footprint grid settings
projection <- '+proj=longlat'
smooth_factor <- 1
time_integrate <- F

# Grid details from the emission file
nLon <- 377                       # Number of longitude points
nLat <- 445                       # Number of lattitude points


# Bounding region
xlims <- c(-7.23904689, -1.26064689) # Larger Glasgow domain
ylims <- c(54.00623474, 57.99335474) # Larger Glasgow domain

#xlims <- c(-7.2, -1.3) # Larger Glasgow domain
#ylims <- c(54.0, 58.0) # Larger Glasgow domain

# Create the grid
lons <- linspace(xlims[1], xlims[2], nLon)
lats <- linspace(ylims[1], ylims[2], nLat)

#lons <- seq(xlims[1], xlims[2], length.out = nLon)
#lats <- seq(ylims[1], ylims[2], length.out = nLat)

# Ensure the correct number of points
#if(length(lons) != nLon) stop("Longitude grid points do not match nLon")
#if(length(lats) != nLat) stop("Latitude grid points do not match nLat")

# Required params for the grid
#xmn <- -7.23904689
#xmx <- -1.26064689
#ymn <- 54.00623474 
#ymx <- 57.99335474


#xlims[1] <- xmn
#xlims[2] <- xmx
#ylims[1] <- ymn
#ylims[2] <- ymx


xmn <- xlims[1]
xmx <- xlims[2]
ymn <- ylims[1]
ymx <- ylims[2]
nx <- nLon
ny <- nLat
xres <- 0.015900000000000247
yres <- 0.008980000000001098
lons <- linspace(xlims[1], xlims[2], nLon)
lats <- linspace(ylims[1], ylims[2], nLat)

#xres <- (xmx - xmn) / (nx - 1)
#yres <- (ymx - ymn) / (ny - 1)
#xres <-  0.015900000000000247
#yres <- 0.008980000000001098 
# Print to verify
#print(paste("Number of longitude points:", length(lons)))
#print(paste("Number of latitude points:", length(lats)))
#print(paste("Longitude resolution:", xres))
#print(paste("Latitude resolution:", yres))

#print("Longitude points:")
#print(lons)
#print("Latitude points:")
#print(lats)


# Meteorological data input
met_path_inner <- '/mnt/data2/skodoli/STILT_INPUT/JJASO_2023'
met_file_format <- 'wrfout_d02_%Y-%m-%d.arl'
met_subgrid_buffer <- 0.1
met_subgrid_enable <- F
met_subgrid_levels <- NA
n_met_min <- 1

# Model control
n_hours <- -72
numpar <- 1000
rm_dat <- T
run_foot <- T
run_trajec <- T
timeout <- 3600
#varsiwant <- c('time', 'indx', 'long', 'lati', 'zagl', 'foot', 'mlht', 'dens',
 #              'samt', 'sigw', 'tlgr', 'dmas', 'rain')

varsiwant  <- c('time', 'indx', 'long', 'lati', 'zagl', 'sigw', 'tlgr', 'zsfc',
                'icdx', 'temp', 'samt', 'foot', 'shtf', 'tcld', 'dmas', 'dens',
                'rhfr', 'sphu', 'lcld', 'zloc', 'dswf', 'wout', 'mlht',
                'rain', 'crai', 'pres')

hnf_plume <- T
projection <- '+proj=longlat'
smooth_factor <- 1
time_integrate <- F

# Transport and dispersion settings
capemin <- -1
cmass <- 0
conage <- 48
cpack <- 1
delt <- 1
dxf <- 1
dyf <- 1
dzf <- 0.1
efile <- ''
emisshrs <- 0.01
frhmax <- 3
frhs <- 1
frme <- 0.1
frmr <- 0
frts <- 0.1
frvs <- 0.01
hscale <- 10800
ichem <- 8
idsp <- 2
iconvect <- 0
initd <- 0
isot  <- 0
k10m <- 1
kagl <- 1
kbls <- 1
kblt <- 5
kdef <- 0
khinp <- 0
khmax <- 9999
kmix0 <- 250
kmixd <- 3
kmsl <- 0
kpuff <- 0
krand <- 4
krnd <- 6
kspl <- 1
kwet <- 1
kzmix <- 0
maxdim <- 1
maxpar <- min(10000, numpar)
mgmin <- 2000
mhrs <- 9999
nbptyp <- 1
ncycl <- 0
ndump <- 0
ninit <- 1
nstr <- 0
nturb <- 0
nver <- 0
outdt <- 0
outfrac <- 0.9
p10f <- 1
pinbc <- ''
pinpf <- ''
poutf <- ''
qcycle <- 0
random <- 1
rhb <- 80
rht <- 60
splitf <- 1
tkerd <- 0.18
tkern <- 0.18
tlfrac <- 0.1
tout <- 0
tratio <- 0.75
tvmix <- 1
veght <- 0.5
vscale <- 200
vscaleu <- 200
vscales <- -1
wbbh <- 0
wbwf <- 0
wbwr <- 0
wvert <- FALSE
w_option <- 0
zicontroltf <- 0
ziscale <- rep(list(rep(0.8, 24)), nrow(receptors))
z_top <- 25000

# Transport error settings
horcoruverr <- NA
siguverr <- NA
tluverr <- NA
zcoruverr <- NA

horcorzierr <- NA
sigzierr <- NA
tlzierr <- NA

# Interface to mutate the output object with user defined functions
before_trajec <- function() {output}
before_footprint <- function() {output}

# Source dependencies ----------------------------------------------------------
setwd(stilt_wd)
source('r/dependencies.r')

# Structure out directory ------------------------------------------------------
system(paste0('rm -r ', output_wd, '/footprints'), ignore.stderr = T)
if (run_trajec) {
  system(paste0('rm -r ', output_wd, '/by-id'), ignore.stderr = T)
  system(paste0('rm -r ', output_wd, '/met'), ignore.stderr = T)
  system(paste0('rm -r ', output_wd, '/particles'), ignore.stderr = T)
}
for (d in c('by-id', 'particles', 'footprints')) {
  d <- file.path(output_wd, d)
  if (!file.exists(d)) dir.create(d, recursive = T)
}

# Run trajectory simulations ---------------------------------------------------
stilt_apply(X = 1:nrow(receptors),FUN = simulation_step,
            before_footprint = list(before_footprint),
            before_trajec = list(before_trajec),
	    lib.loc = lib.loc,
            rm_dat = rm_dat,
	    conage = conage, 
	    cpack = cpack,
	    delt = delt,
            emisshrs = emisshrs, 
	    dxf = dxf,
            dyf = dyf,
            dzf = dzf,
	    frhmax = frhmax, 
	    frhs = frhs,
            frme = frme, 
	    frmr = frmr, 
	    frts = frts, 
	    frvs = frvs,
            hnf_plume = hnf_plume, 
	    horcoruverr = horcoruverr,
            horcorzierr = horcorzierr, 
	    ichem = ichem,
            iconvect = iconvect, 
	    initd = initd, 
	    isot = isot,
            kbls = kbls, 
	    kblt = kblt, 
	    kdef = kdef, 
	    khmax = khmax,
            kmix0 = kmix0, 
	    kmixd = kmixd, 
	    kmsl = kmsl, 
	    kpuff = kpuff,
            krnd = krnd, 
	    kspl = kspl, 
	    kzmix = kzmix, 
	    maxdim = maxdim,
            maxpar = maxpar, 
	    met_path = met_path_inner,
            met_file_format = met_file_format,
            mgmin = mgmin, 
	    n_hours = n_hours, 
	    n_met_min = n_met_min,
            ncycl = ncycl, 
	    ndump = ndump, 
	    ninit = ninit,
            nturb = nturb, 
	    numpar = numpar, 
	    outdt = outdt,
            outfrac = outfrac, 
	    output_wd = output_wd, 
	    p10f = p10f,
            projection = projection, 
	    qcycle = qcycle,
            r_run_time = receptors$run_time, 
	    r_lati = receptors$lati,
            r_long = receptors$long, 
	    r_zagl = receptors$zagl,
            random = random, 
	    run_trajec = run_trajec, 
	    run_foot = run_foot,
            siguverr = siguverr,
	    sigzierr = sigzierr,
            smooth_factor = smooth_factor,
	    splitf = splitf,
            stilt_wd = stilt_wd,
	    time_integrate = time_integrate,
            timeout = timeout,
	    tkerd = tkerd, tkern = tkern,
            tlfrac = tlfrac,
	    tluverr = tluverr, 
	    tlzierr = tlzierr,
            tratio = tratio, 
	    tvmix = tvmix, 
	    varsiwant = list(varsiwant),
            veght = veght, 
	    vscale = vscale, 
            w_option = w_option,
            wbbh = wbbh,
            wbwf = wbwf,
            wbwr = wbwr,
            wvert = wvert,
            wvert <- wvert,
	    xmn = xmn, 
	    xmx = xmx, 
	    xres = xres, 
	    ymn = ymn, 
	    ymx = ymx,
            yres = yres, 
	    nx = nx, 
	    ny = ny, 
	    zicontroltf = zicontroltf, 
	    ziscale = ziscale, 
	    z_top = z_top, 
	    zcoruverr = zcoruverr)

