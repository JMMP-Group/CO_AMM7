build open boundaries
**********************

Process to build open boundary files from GLOSEA6 data.
location: JASMIN

This process generates the BDY files from within the CO_AMM7/BDY directory. 



Build pyBDY
***********

Following the guidance in the pyBDY repo E.g.::

    git clone https://github.com/NOC-MSM/pyBDY.git
    cd pyBDY
    micromamba env create -n pybdy -f environment.yml python=3.9
    
    micromamba activate pybdy
    
    # Lib/ dir is found 3 levels back from:
    readlink -f $(which java)
    
    export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/
    export JVM_PATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/lib/amd64/server/libjvm.so
    
    (had to manually find libjvm.so and set it)

    I had some issues with JAVA on JASMIN, which I couldn't solve in a general way so I commented out the bits I didn't need:

    Comment out `pyBDY/src/pybdy/profiler.py`::

	line35 
	# from PyQt5.QtWidgets import QMessageBox

	lines 506-508
	# QMessageBox.warning(
        #     None, "NRCT", "Mask is not set, setting a 1 grid " + "point border mask"
        #)

    Then install::

    	pip install -e .





Prepare input files for pyBDY
*****************************

PyBDY doesn't like using ncml to read the expected ``Bathymetry`` variable from bathymetry file. So we make it manually from the domain configuration file (pyBDY expects variables: ``nav_lat``, ``nav_lon`` and ``Bathymetry``)::
	
	cp /gws/nopw/j04/jmmp/public/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc CO_AMM7/BDY/.
	cd CO_AMM7/BDY
        python generate_bathymetry.py

This creates file ``domain_cfg_co9amm7_MEsL51r10-07_bathmetry.nc``.


PyBDy expects particular variables (``e3u`` not ``e3u_0`` etc) in the file for the destination vertical grid. Create a fake zgr mesh for AMM7::

	module load jaspy
	ncks -v mbathy,nav_lat,nav_lon,nav_lev,e3u_0,e3v_0,e3w_0,e3t_0 domain_cfg_co9amm7_MEsL51r10-07.nc domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3u_0,e3u domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3v_0,e3v domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3w_0,e3w domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3t_0,e3t domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc

NB use of NCML renaming magic (e.g. ``inputs_AMM7_dst_zgr.ncml``) doesn't seem to work as an input for namelist variable ``sn_dst_zgr``. 



	FYI (but not used here). If the boundaries are not simply the edges of the domain, then a boundary mask can be used. This can also be generated with the PyNEMO GUI. The mask variable takes values (-1 mask, 1 wet, 0 land). Get a template from domain_cfg.nc and then modify as desired around the boundary::
		
		module load jaspy
		rm -f bdy_mask.nc tmp[12].nc
		ncks -v top_level domain_cfg.nc tmp1.nc
		ncrename -h -v top_level,mask tmp1.nc tmp2.nc
		ncwa -a t tmp2.nc bdy_mask.nc
		rm -f tmp[12].nc
	
	Then in ipython::
		
		import netCDF4, numpy
		dset = netCDF4.Dataset('bdy_mask.nc','a')
		dset.variables['mask'][0,:]  = -1     # Southern boundary
		dset.variables['mask'][-1,:] = -1    # Northern boundary
		dset.variables['mask'][:,-1] = -1    # Eastern boundary
		dset.variables['mask'][:,0] = -1        # Western boundary
		dset.close()






In this particular workflow an intermediate step is used to produce cut down version of the global data. A domain configuration file for the cutout needs to be prepared to have the expected variables::

``ncdump -h mesh_mask_glosea6_amm15_subset.nc``

::
    
    netcdf mesh_mask_glosea6_amm15_subset {
    dimensions:
    	t = UNLIMITED ; // (1 currently)
    	y = 177 ;
    	x = 218 ;
    	z = 75 ;
    variables:
    	float nav_lon(y, x) ;
    		nav_lon:_FillValue = NaNf ;
    	float nav_lat(y, x) ;
    		nav_lat:_FillValue = NaNf ;
    	float nav_lev(z) ;
    		nav_lev:_FillValue = NaNf ;
    	double time_counter(t) ;
    		time_counter:_FillValue = NaN ;
	...
    	double e1t(t, y, x) ;
    		e1t:_FillValue = NaN ;
    	double e1u(t, y, x) ;
    		e1u:_FillValue = NaN ;
	...
    	short mbathy(t, y, x) ;
    	short misf(t, y, x) ;
    	float isfdraft(t, y, x) ;
    		isfdraft:_FillValue = NaNf ;
    	double e3t_0(t, z, y, x) ;
    		e3t_0:_FillValue = NaN ;
    	double e3u_0(t, z, y, x) ;
    		e3u_0:_FillValue = NaN ;
    	double e3v_0(t, z, y, x) ;
    		e3v_0:_FillValue = NaN ;
    	double e3w_0(t, z, y, x) ;
    		e3w_0:_FillValue = NaN ;
    	float gdept_0(t, z, y, x) ;
    		gdept_0:_FillValue = NaNf ;
    	float gdepu(t, z, y, x) ;
    		gdepu:_FillValue = NaNf ;
    	float gdepv(t, z, y, x) ;
    		gdepv:_FillValue = NaNf ;
    	float gdepw_0(t, z, y, x) ;
    		gdepw_0:_FillValue = NaNf ;
	...
    
    // global attributes:
    		:file_name = "mesh_mask.nc" ;
		:TimeStamp = "01/11/2016 14:11:44 +0000" ;
		:_NCProperties = "version=2,netcdf=4.9.2,hdf5=1.14.2" ;

But ``pyBDY`` expects ``gdept_0(t, z, y, x)`` to be ``gdept_0(t, z)``. Since this is a z-level parents the x,y dimensions can be collapsed (put it in the BDY folder)::

	cd CO_AMM7/BDY
	python
	import xarray as xr
	#ds = xr.load_dataset("/gws/nopw/j04/jmmp/MASS/GloSea6/Grid/mesh_mask_glosea6_amm15_subset.nc")
	ds = xr.load_dataset("mesh_mask.nc")

	ds = ds.isel(x=slice(1030,1220), y=slice(843,1076))


	ds['gdept_0'] = ds.gdept_0.mean(dim='x').mean(dim='y')
	ds.to_netcdf("mesh_mask_cutout_for_AMMregion_flatten_gdept_0.nc")



Start from here if pyBDY is already built
*****************************************

Load the environment variables and activate the python environment::

    export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/
    export JVM_PATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/lib/amd64/server/libjvm.so
    micromamba activate pybdy

The ``CO_AMM7/BDY/`` folder of this repository contains all the scripts, namelist and ncml files you need to generate boundary files from GLOSEA6 parent data (assuming you have that parent data)::

The following is a template for how one could launch pyBDY on some data but will run out of memory or not so a batch of years::

	cd CO_AMM7/BDY
	pybdy -s namelist_local_glosea6.bdy

Runs out of memory --> try the lotus queue::

	sbatch lotus_demo.sh

We want to create a lot of files but the java doesn't like handling too many files at once. It can do a month at a time so the plan is to create directories for each month of parent (src) data and loop over each month. Symbolic links are created for the parent data. This script is handled in the ``lotus_glosea_to_amm7.sh`` script.

Edit the year and month(s) in `lotus_glosea_to_amm7.sh` and press go::

	cd CO_AMM7/BDY
	sbatch lotus_glosea_to_amm7.sh

This will output ...
If things go wrong check the ``nrct.log file`` and fix it.





