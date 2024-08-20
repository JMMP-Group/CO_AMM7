build open boundaries
**********************

Process to build open boundary files from GLOSEA6 data.
location: JASMIN

This process generates the BDY files from within the pyNEMO directory tree. Specifically within the `inputs` directory of that repository. 
This folder contains the namelist and ncml files that need to be copied into the pyBDY inputs directory. These notes assume you are happy with this workflow.

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
    
    pip install -e .


Prepare input files for pyBDY
*****************************

PyBDY doesn't like using ncml to read the expected `Bathymetry` variable from bathymetry file. So we make it manually from the domain configuration file (pyBDY expects variables: nav_lat, nav_lon and Bathymetry)::
	
	cp /gws/nopw/j04/jmmp/public/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc .
	python generate_bathymetry.py
	mv domain_cfg_co9amm7_MEsL51r10-07_bathmetry.nc pyBDY/inputs/.


PyBDy expects particular variables (`e3u` not `e3u_0` etc) in the file for the destination vertical grid. Create a fake zgr mesh for AMM7::

	ncks -v mbathy,nav_lat,nav_lon,nav_lev,e3u_0,e3v_0,e3w_0,e3t_0 /gws/nopw/j04/jmmp/public/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc pyBDY/inputs/AMM7_zgr.nc
	ncrename -O -v e3u_0,e3u pyBDY/inputs/AMM7_zgr.nc
	ncrename -O -v e3v_0,e3v pyBDY/inputs/AMM7_zgr.nc
	ncrename -O -v e3w_0,e3w pyBDY/inputs/AMM7_zgr.nc
	ncrename -O -v e3t_0,e3t pyBDY/inputs/AMM7_zgr.nc

Or use `inputs_AMM7_dst.ncml` ?? TEST AND RENAME TO *zgr*?


Start from here if pyBDY is already built
*****************************************

Load the environment variables and activate the python environment::

    export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/
    export JVM_PATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/lib/amd64/server/libjvm.so
    micromamba activate pybdy

The `CO_AMM7/BDY/` folder of this repository contains all the namelist and ncml files you need to generate boundary files from GLOSEA6 parent data (assuming you have that parent data). These files are copied to the `pyBDY/inputs/` directory. Assuming you clone `pyBDY` and `CO_AMM7` into the same directory, navigate there and copy the files::

	cp CO_AMM7/BDY/*bdy pyBDY/inputs/.
	cd pyBDY/inputs/


The following is a template for how one could launch pyBDY on some data but will run out of memory or not so a batch of years::

	pybdy -s namelist_local_glosea6.bdy

Runs out of memory --> try the lotus queue::

	sbatch lotus_demo.sh

We want to create a lot of files but the java doesn't like handling too many files at once. It can do a month at a time so the plan is to create directories for each month of parent (src) data and loop over each month. Symbolic links are created for the parent data. This script is handled in the `lotus_glosea_to_amm7.sh` script.

Edit the year and month(s) in `lotus_glosea_to_amm7.sh` and press go::

	cd pyBDY/inputs
	sbatch lotus_glosea_to_amm7.sh

This will output ...
If things go wrong check the nrct.log file and fix it.





