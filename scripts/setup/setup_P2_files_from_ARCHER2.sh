# link to input files (other filenames are specified in namelist_cfg)

# domain_cfg.nc
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.2/MODEL/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/DOM/domain_cfg_MEs_L51_r10-07_opt_v2.nc domain_cfg.nc

# bdy coordinate files
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/coordinates.bdy.nc coordinates.bdy.nc
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/coordinates.skagbdy.nc coordinates.skagbdy.nc


# RESTART FOR ... 
#ln -s /work/n01/n01/shared/CO_AMM15/P1_INPUTS/FORCING/RESTART/RESTART_BASED_ONCO7_20040101_TO_GEG_NICO_BALTIC_BLOCK_BUT_10M_MIN_RIV_DEP INITIAL_RESTART

#RIVERS
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/RIV .

# BDY, TIDE and ERA5 forcing
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/TIDE .
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/SBC .
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/BDY_SKAG .
ln -s /work/n01/n01/anwise/NEMO/VC_4.0.1/test/nemo/cfgs/AMM7/ENSEMBLE_INPUTS/BDY .

# make sure directory for outputting RESTART files exists
mkdir RESTARTS
