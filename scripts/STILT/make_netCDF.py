import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc
import glob
import zipfile

#Read in the node names
node_names = pd.read_csv("beaconNodes.csv")

# Fill in missing values
node_names  = node_names.fillna({'height_above_ground': 3})

nodeID  = node_names['id']
nodeLAT = node_names['lat']
nodeLON = node_names['lng']
nodeAGL = node_names['height_above_ground']

#read in the dataset

zf = zipfile.ZipFile('/mnt/data2/skodoli/Receptor_file_BEACON_oxna/geminoa-master/data/beacon/sepoct23_lez.zip') 
names = zf.namelist()

#names

#read in the first site

names = zf.namelist()

#make an array of the bad data (files which give the empty data effor)

bads=[]
for i in np.array(names):
    try:
      #  df_temp = pd.read_csv(zf.open(i), index_col=0, parse_dates=[0])
      df_temp = pd.read_csv(zf.open(i), index_col=0, parse_dates=[0], dayfirst=True)
    except:
        df_temp = pd.DataFrame()
        bads.append(i)
        
#make a list of the file names
names = list(names)

#remove the bad files

good_names = [x for x in names if x not in bads]

#combine all sites' data into one dataframe

#dfs = [pd.DataFrame() if (pd.read_csv(zf.open(f), index_col=[0], parse_dates=[0]).empty == True) else  pd.read_csv(zf.open(f), index_col=[0], parse_dates=[0]) for f in good_names]

dfs = [pd.DataFrame() if (pd.read_csv(zf.open(f), index_col=[0], parse_dates=[0], dayfirst=True).empty == True) 
       else pd.read_csv(zf.open(f), index_col=[0], parse_dates=[0], dayfirst=True) for f in good_names]


finaldf = pd.concat(dfs, axis=0, join='outer').sort_index()

finaldf = finaldf.merge(node_names[['id','lat','lng','height_above_ground']], left_on='node_id', right_on = 'id', how='left')


finaldf = finaldf[finaldf['co2_corrected_avg']>0]

#finaldf


#finaldf = finaldf.set_index(pd.to_datetime(finaldf['datetime'],format="%d/%m/%Y %H:%M"))
finaldf['datetime'] = pd.to_datetime(finaldf['datetime'], dayfirst=True)
finaldf = finaldf.set_index(finaldf['datetime'])


#finaldf

# pivot table to have each site in it's own column, with node ids as column titles

#df = finaldf.pivot_table(index = finaldf.index,columns = 'node_id', values = ['co2_corrected_avg'])

#df['co2_corrected_avg'].median(axis=1).plot(figsize=(12,5), label='CO2')
#plt.show()
#df['co2_corrected_avg_t_drift_applied-level-2'].median(axis=1).plot(figsize=(12,5), label='CO2')
#plt.ylabel('CO Concentration (ppm)')
#df['co2_corrected_avg_t_drift_applied-level-2'].count(axis=1).plot(figsize=(12,5),secondary_y=True,style='g', alpha=0.9, label='Nodes')
#plt.ylabel('Nodes Working')
#plt.legend()

fn = 'rec_sepoct23_lez.nc'
ncid = nc.Dataset(fn, 'w', format='NETCDF4')

#define dimensions
nObs = len(finaldf)

#Add dimensions to netcdf
dimid = ncid.createDimension('id', nObs)

#define the netcdf variables
fjulid = ncid.createVariable('fjul', 'double', ('id',))
latid  = ncid.createVariable('lat', 'double', ('id',))
lonid  = ncid.createVariable('lon', 'double', ('id',))
aglid  = ncid.createVariable('agl', 'double', ('id',))
co2id  = ncid.createVariable('co2', 'double', ('id',))
co2eid = ncid.createVariable('co2e', 'double', ('id',))
yrid   = ncid.createVariable('yr', 'double', ('id',))
monid  = ncid.createVariable('mon', 'double', ('id',))
dayid  = ncid.createVariable('day', 'double', ('id',))
hrid   = ncid.createVariable('hr', 'double', ('id',))


fjulid[:] = finaldf['epoch'].values
latid[:]  = finaldf['lat'].values
lonid[:]  = finaldf['lng'].values
aglid[:]  = finaldf['height_above_ground'].values
co2id[:]  = finaldf['co2_corrected_avg'].values
co2eid[:] = finaldf['co2_corrected_stdev'].values
yrid[:]   = finaldf.index.year.values
monid[:]  = finaldf.index.month.values
dayid[:]  = finaldf.index.day.values
hrid[:]   = finaldf.index.hour.values
aglid
ncid.close()
