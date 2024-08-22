# link to input files (other filenames are specified in namelist_cfg)

# domain_cfg.nc
wget -O domain_cfg.nc https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc

# bdy coordinate files (to be updated)
#wget http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.bdy.nc
#wget http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.skagbdy.nc



# RESTART FOR ... 
#ln -s /work/n01/n01/shared/CO_AMM15/P1_INPUTS/FORCING/RESTART/RESTART_BASED_ONCO7_20040101_TO_GEG_NICO_BALTIC_BLOCK_BUT_10M_MIN_RIV_DEP INITIAL_RESTART

#TIDE
wget -r -nH -e robots=off --cut-dirs=6 --no-parent --reject="index.html*" https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/TIDE/ORGINAL/
mv ORGINAL TIDE

# initial condition (to be updated)
#wget -P IC https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/IC/initcd_y2005m01.nc

#SBC
mkdir SBC
wget -P SBC -e robots=off -r -nd --no-parent -A '*2005*nc' https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/SBC/
wget -P SBC https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/SBC/ERA5_LSM.nc
wget -P SBC https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/SBC/weights_era5_bicubic.nc

#BDY (to be updated)
#mkdir BDY
#wget -P BDY -e robots=off -r -nd --no-parent --reject="index.html*" https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY/2005/

#BDY_SKAG (to be updated)
#mkdir BDY_SKAG
#wget -P BDY_SKAG -e robots=off -r -nd --no-parent --reject="index.html*" https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY_SKAG/2005/



#RIVERS
wget -P RIV -e robots=off -nd --no-parent --reject="index.html*" https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/RIV/SSB_climatology_1980_2014_noleap.nc
cd RIV
ln -s SSB_climatology_1980_2014_noleap.nc rivers_y2005.nc
cd ..



# make sure directory for outputting RESTART files exists
mkdir RESTARTS
