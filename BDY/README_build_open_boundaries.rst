build open boundaries
**********************

Process to build open boundary files from GLOSEA6 data.
location: JASMIN

This process generates the BDY files from within the CO_AMM7/BDY directory. 

Because GLOSEA6 data are large we have an intermediate step to create a bigger-than-AMM-cut-out of daily files. The starting point here is access to these daily files and a domain configuration file for the cutout:

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
    	byte tmask(t, z, y, x) ;
    	byte umask(t, z, y, x) ;
    	byte vmask(t, z, y, x) ;
    	byte fmask(t, z, y, x) ;
    	byte tmaskutil(t, y, x) ;
    	byte umaskutil(t, y, x) ;
    	byte vmaskutil(t, y, x) ;
    	byte fmaskutil(t, y, x) ;
    	float glamt(t, y, x) ;
    		glamt:_FillValue = NaNf ;
    	float glamu(t, y, x) ;
    		glamu:_FillValue = NaNf ;
    	float glamv(t, y, x) ;
    		glamv:_FillValue = NaNf ;
    	float glamf(t, y, x) ;
    		glamf:_FillValue = NaNf ;
    	float gphit(t, y, x) ;
    		gphit:_FillValue = NaNf ;
    	float gphiu(t, y, x) ;
    		gphiu:_FillValue = NaNf ;
    	float gphiv(t, y, x) ;
    		gphiv:_FillValue = NaNf ;
    	float gphif(t, y, x) ;
    		gphif:_FillValue = NaNf ;
    	double e1t(t, y, x) ;
    		e1t:_FillValue = NaN ;
    	double e1u(t, y, x) ;
    		e1u:_FillValue = NaN ;
    	double e1v(t, y, x) ;
    		e1v:_FillValue = NaN ;
    	double e1f(t, y, x) ;
    		e1f:_FillValue = NaN ;
    	double e2t(t, y, x) ;
    		e2t:_FillValue = NaN ;
    	double e2u(t, y, x) ;
    		e2u:_FillValue = NaN ;
    	double e2v(t, y, x) ;
    		e2v:_FillValue = NaN ;
    	double e2f(t, y, x) ;
    		e2f:_FillValue = NaN ;
    	double ff(t, y, x) ;
    		ff:_FillValue = NaN ;
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
    	double gdept_1d(t, z) ;
    		gdept_1d:_FillValue = NaN ;
    	double gdepw_1d(t, z) ;
    		gdepw_1d:_FillValue = NaN ;
    	double e3t_1d(t, z) ;
    		e3t_1d:_FillValue = NaN ;
    	double e3w_1d(t, z) ;
    		e3w_1d:_FillValue = NaN ;
    
    // global attributes:
    		:file_name = "mesh_mask.nc" ;
		:TimeStamp = "01/11/2016 14:11:44 +0000" ;
		:_NCProperties = "version=2,netcdf=4.9.2,hdf5=1.14.2" ;


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

    Fix `pyBDY/src/pybdy/nemo_bdy_extr_tm3.py`::

	line 823
	-                dst_bdy = np.zeros_like(dist_fac)
	+                dst_bdy = np.zeros_like(dist_fac) * np.nan

    Then install::

    	pip install -e .


Prepare input files for pyBDY
*****************************

PyBDY doesn't like using ncml to read the expected `Bathymetry` variable from bathymetry file. So we make it manually from the domain configuration file (pyBDY expects variables: nav_lat, nav_lon and Bathymetry)::
	
	cp /gws/nopw/j04/jmmp/public/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc CO_AMM7/BDY/.
	cd CO_AMM7/BDY
        python generate_bathymetry.py

This creates file `domain_cfg_co9amm7_MEsL51r10-07_bathmetry.nc`.


PyBDy expects particular variables (`e3u` not `e3u_0` etc) in the file for the destination vertical grid. Create a fake zgr mesh for AMM7::

	module load jaspy
	ncks -v mbathy,nav_lat,nav_lon,nav_lev,e3u_0,e3v_0,e3w_0,e3t_0 domain_cfg_co9amm7_MEsL51r10-07.nc domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3u_0,e3u domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3v_0,e3v domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3w_0,e3w domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc
	ncrename -O -v e3t_0,e3t domain_cfg_co9amm7_MEsL51r10-07_dst_zgr.nc

NB use of NCML renaming magic (e.g. `inputs_AMM7_dst_zgr.ncml`) doesn't seem to work for this input `sn_dst_zgr`. 


Start from here if pyBDY is already built
*****************************************

Load the environment variables and activate the python environment::

    export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/
    export JVM_PATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/lib/amd64/server/libjvm.so
    micromamba activate pybdy

The `CO_AMM7/BDY/` folder of this repository contains all the scripts, namelist and ncml files you need to generate boundary files from GLOSEA6 parent data (assuming you have that parent data)::

The following is a template for how one could launch pyBDY on some data but will run out of memory or not so a batch of years::

	cd CO_AMM7/BDY
	pybdy -s namelist_local_glosea6.bdy

Runs out of memory --> try the lotus queue::

	sbatch lotus_demo.sh

We want to create a lot of files but the java doesn't like handling too many files at once. It can do a month at a time so the plan is to create directories for each month of parent (src) data and loop over each month. Symbolic links are created for the parent data. This script is handled in the `lotus_glosea_to_amm7.sh` script.

Edit the year and month(s) in `lotus_glosea_to_amm7.sh` and press go::

	cd CO_AMM7/BDY
	sbatch lotus_glosea_to_amm7.sh

This will output ...
If things go wrong check the nrct.log file and fix it.





