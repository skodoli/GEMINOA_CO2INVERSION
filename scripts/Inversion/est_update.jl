using PyCall
pyimport("matplotlib").use("Agg")
pyplot = pyimport("matplotlib.pyplot")

### Libraries
using Distributed
@everywhere using Printf
@everywhere using NetCDF
@everywhere using CSV
@everywhere using Dates
@everywhere using DataFrames
using PyPlot
plt = PyPlot

### Load my functions
include("./Util/plot_funcs.jl")
include("./Util/read_funcs.jl")
include("./Util/get_data_funcs.jl")
include("./Util/reshape_funcs.jl")
include("./Util/solve_inv_funcs.jl")

### ========================================
### Parameters
### ========================================

### Globals needed for other files
global origin     = Dates.DateTime(1970,1,1,0,0,0)
global IntType    = Int64   # Precision we are using for integers
global FltType    = Float32 # Precision we are using for floats
global lowBound   = 1e-5    # Smallest number to include in our correlation matrices
global diag_prior = false   # Are we using a diagonal prior?
global minUncert  = 1.0     # Lower bound on the prior uncertainty (umol/m2/s)
global minObsErr  = 1.0     # Lower bound on the measurement uncertainty (ppm)
global obsFreq    = 60.0    # Observations per hour

### Time period for the inversion
startTime  = Dates.DateTime(2022,09,01) # Start of inversion period
invWindow  = Dates.Day(1)               # Time window for inversion
nHr        = 36                         # Number of backhours
UTC_to_glasgowtime = Dates.Hour(1)

### Different grids to use
# Full grid used for footprints
full_xLim = [ -125.0, -120.0 ]
full_yLim = [   36.0,   40.0 ]
big_xLim,big_yLim = full_xLim,full_yLim
# Medium sized grid
medium_xLim = [-123.60,-121.60]
medium_yLim = [  36.80,  38.60]
med_xLim,med_yLim = medium_xLim,medium_yLim
# Bay Area domain (smallest grid)
BayArea_xLim = [-5.1059, -3.5059]
BayArea_yLim = [55.3736, 56.3736]
small_xLim,small_yLim = BayArea_xLim,BayArea_yLim
# Inversion grid to use
Inv_lonLim = small_xLim
Inv_latLim = small_yLim

ems_uncert =  50/100    # %
# Model error at different hours of the day
mod_err = [[00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23]  # hour
           [ 3  3  3  3  3  3  4  5  8  6  4  2  1  1  1  1  2  4  6  8  5  4  3  3]] # error (ppm)

### Information for the k-fold cross validation
cross_valid = false # Set to true for cross validation
k_fold      = 10    # Percentage to leave out
k_ind       = 1     # Which index are we running?
k_fold_dat  = (cross_valid, k_fold, k_ind)
# Set k_ind to zero if we are not doing cross_validation
k_ind = cross_valid ? k_ind : 0

### Save data?
save_plot = false
save_ems  = true

### Store the inversion data
startTime  = startTime - UTC_to_glasgowtime # Convert from glasgowtime to UTC
endTime    = startTime + invWindow
Inv_timLim = [ startTime - Dates.Hour(nHr), endTime + Dates.Hour(nHr) ]

### Make output folders?
ispath("./output")        ? nothing : mkdir("./output")
ispath("./output/ems")    ? nothing : mkdir("./output/ems")
ispath("./output/obs")    ? nothing : mkdir("./output/obs")
ispath("./output/images") ? nothing : mkdir("./output/images")

### ========================================
### Get the data
### ========================================

### Read the observation information
println("* READING OBSERVATION AND FOOTPRINT DATA")
(latF,lonF,obsInfo,foot) = get_footprint_data(Inv_latLim,Inv_lonLim,Inv_timLim)

### Read the emission information
println("* READING EMISSIONS DATA")
(latE,lonE,ems,emsTime) = get_emission_data(Inv_latLim,Inv_lonLim,Inv_timLim)

### Diagnostic
println("* ALL DATA READ")

println(latE, lonE, latF, lonF)

### Make sure the lats/lons match
if (sum(abs.(lonE - lonF)) > 1e-3) || (sum(abs.(latE - latF)) > 1e-3)
   error("LATS AND LONS DON'T MATCH!")
else
   lon, lat = lonE, latE
end

### ========================================
### Estimate the emissions
### ========================================

### Remove the extra processors
rmprocs([2:24])

### Construct the input tuples
inv_data   = (lon,lat,Inv_latLim,Inv_lonLim,startTime,endTime,ems_uncert,mod_err,k_fold_dat)
foot_data  = (obsInfo,foot,nHr)
ems_data   = (ems,emsTime)
input_data = (inv_data,foot_data,ems_data)

println("* ESTIMATING EMISSIONS")
@time (lon_red,lat_red,ems_Times,x_pri,x_hat,obs_compare,dofs) = estimate_ems(input_data)

### ========================================
### Store the solution
### ========================================

# Modified save_emissions function
function save_emissions(outDir, x_hat, lon, lat, times)
    println("Entering save_emissions function")
    println("outDir: $outDir")
    println("x_hat size: ", size(x_hat))
    println("lon size: ", size(lon))
    println("lat size: ", size(lat))
    println("times length: ", length(times))

    if !isdir(outDir)
        mkdir(outDir)
        println("Created directory: $outDir")
    else
        println("Directory already exists: $outDir")
    end

    for t in 1:length(times)
        filename = joinpath(outDir, @sprintf("emissions_%04d.nc", t))
        println("Attempting to save file: $filename")
        try
            NetCDF.create(filename, "emissions", ["lon", "lat"], [length(lon), length(lat)])
            println("NetCDF file created")
            
            NetCDF.putvar(filename, "emissions", x_hat[:,:,t])
            println("Emissions data written")
            
            NetCDF.putvar(filename, "lon", lon)
            println("Longitude data written")
            
            NetCDF.putvar(filename, "lat", lat)
            println("Latitude data written")
            
            NetCDF.putatt(filename, "emissions", "time", Dates.format(times[t], "yyyy-mm-dd HH:MM:SS"))
            println("Time attribute added")
            
            NetCDF.close(filename)
            println("File closed")
            
            if isfile(filename)
                println("File successfully created and exists: $filename")
                println("File size: $(filesize(filename)) bytes")
            else
                println("WARNING: File does not exist after saving: $filename")
            end
        catch e
            println("Error saving file $filename: ", e)
            @error "Stack trace:" exception=(e, catch_backtrace())
        end
    end
    println("Emissions saving process completed")
end

### Save emissions?
if save_ems
    println("* STORING EMISSIONS")
    outDir = cross_valid ? @sprintf("./output/ems_%04i",k_ind) : "./output/ems"
    println("Output directory: $outDir")
    
    # Check if output directory exists and is writable
    if !isdir(outDir)
        try
            mkdir(outDir)
            println("Created output directory: $outDir")
        catch e
            println("Error creating output directory: ", e)
        end
    else
        println("Output directory already exists")
    end
    
    if isdir(outDir)
        testfile = joinpath(outDir, "test.txt")
        try
            open(testfile, "w") do io
                write(io, "Test")
            end
            println("Successfully wrote test file")
            rm(testfile)
            println("Successfully removed test file")
        catch e
            println("Error writing to output directory: ", e)
        end
    end
    
    # Verify NetCDF functionality
    test_file = joinpath(outDir, "test.nc")
    try
        NetCDF.create(test_file, "test", ["x"], [1])
        NetCDF.putvar(test_file, "test", [1.0])
        NetCDF.close(test_file)
        println("NetCDF test file created successfully")
        if isfile(test_file)
            println("NetCDF test file exists")
            println("NetCDF test file size: $(filesize(test_file)) bytes")
            rm(test_file)
            println("NetCDF test file removed")
        else
            println("WARNING: NetCDF test file does not exist after creation")
        end
    catch e
        println("Error creating NetCDF test file: ", e)
    end
    
    # Call save_emissions with error handling
    try
        save_emissions(outDir, x_hat, lon_red, lat_red, ems_Times)
        println("Emissions saving function completed")
    catch e
        println("Error in save_emissions: ", e)
        @error "Stack trace:" exception=(e, catch_backtrace())
    end

    # Check if files were actually created
    emission_files = filter(f -> startswith(f, "emissions_") && endswith(f, ".nc"), readdir(outDir))
    if isempty(emission_files)
        println("WARNING: No emission files were created in $outDir")
    else
        println("$(length(emission_files)) emission files were created in $outDir")
    end

    ### Estimated emissions
    println("* STORING OBSERVATIONS")
    dateName   = Dates.format(startTime+UTC_to_glasgowtime,Dates.DateFormat("yyyymmdd"))
    if cross_valid
        obsCSVname = @sprintf("./output/obs/%s-%04i_DAILY.csv",dateName,k_ind)
    else
        obsCSVname = @sprintf("./output/obs/%s_DAILY.csv",dateName)
    end
    save_observations(obsCSVname,obs_compare)
end
