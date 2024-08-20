# Generate a bathymetry file by summing e3t to bottom_level
# jelt
# 20Aug'24

import numpy as np
import matplotlib.pyplot as plt
import xarray as xr

## set some paths and load data
fn_dom = 'domain_cfg_co9amm7_MEsL51r10-07.nc'
ds = xr.open_dataset(fn_dom)

## Generate bathmetry variable
top_level = ds.top_level.squeeze()  # is zero over land. 1 over water
bottom_level = ds.bottom_level.squeeze()  # zero over land

# bottom_level values are indexed from 1, not zero.
# if bottom_level = 2, two levels to add together: np.sum (dz[0:2] ). This works in python speak
Bathymetry = np.zeros((ds.dims['y'],ds.dims['x']))
for j in range(ds.dims['y']):
    for i in range(ds.dims['x']):
        Bathymetry[j,i] = np.sum( ds.e3t_0.squeeze()[ 0:int(bottom_level[j,i]), j, i] )  

## Write bathymetry to a new file
ds2 = xr.Dataset(
data_vars=dict(
    Bathymetry=(["y", "x"], Bathymetry,{'units':'m'}),
),
coords=dict(
    nav_lon=(["y", "x"], ds.nav_lon.data),
    nav_lat=(["y", "x"], ds.nav_lat.data),
),
attrs=dict(description="Bathymetry generated from e3t_0 and bottom_level: "+fn_dom),)
ds2.to_netcdf(fn_dom.replace('.nc','_bathmetry.nc'))
