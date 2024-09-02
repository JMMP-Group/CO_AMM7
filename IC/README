Workflow for building initial conditions
****************************************

Download the software tool and edit it::

    git clone https://github.com/rdPatmore/genNEMO.git
    
    # edits paths for AMM7
    +++ b/InitialConditions/calc_ini_ts.py
    @@ -101,7 +101,8 @@ def interpolate_glosea6_to_co9(var, y='1993', m='01', d='01',
         ''' interpolate glosea6 data to co9 target grid '''
         # set file paths
    -    cfg_fn = '/gws/nopw/j04/jmmp/public/AMM15/DOMAIN_CFG/' + domcfg
    +    cfg_fn = '/gws/nopw/j04/jmmp/public/AMM7/CO9_repo/' + domcfg
     
    @@ -133,5 +134,5 @@ def create_uniform_forcing_masked():
         uniform_s.to_netcdf("amm15_uniform_s_masked.nc")
  
     if __name__ == '__main__':
    -    interpolate_glosea6_to_co9('vosaline', domcfg='CO7_EXACT_CFG_FILE.nc')
    -    interpolate_glosea6_to_co9('votemper', domcfg='CO7_EXACT_CFG_FILE.nc')
    +    interpolate_glosea6_to_co9('vosaline', domcfg='domain_cfg_co9amm7_MEsL51r10-07.nc')
    +    interpolate_glosea6_to_co9('votemper', domcfg='domain_cfg_co9amm7_MEsL51r10-07.nc')



Create a python environment::

  micromamba create -n ic_env python=3.10 xarray matplotlib numpy scipy h5netcdf


Move into directory::

  cd /home/users/jelt/GitHub/genNEMO/InitialConditions


Edit ``sub_jas.sh`` to use this environment (from line 13)::

  # executable 
  #python -u calc_mld.py
  #conda activate coast
  module load jaspy
  micromamba activate ic_env
  
  python -u calc_ini_ts.py

Execute::

  sbatch sub_jas.sh


IT WORKS. Outputting::
  
  glosea_ini_19930101_vosaline_domain_cfg_co9amm7_MEsL51r10-07.nc
  glosea_ini_19930101_votemper_domain_cfg_co9amm7_MEsL51r10-07.nc

Copy to ``archer2:/work/n01/n01/jelt/CO_AMM7/CO9_AMM7_P2/nemo/cfgs/AMM7/EXP_glosea6/IC/``::

  ssh archer2
  cd /work/n01/n01/jelt/CO_AMM7/CO9_AMM7_P2/nemo/cfgs/AMM7/EXP_glosea6/IC
  rsync -uvtr jelt@xfer1.jasmin.ac.uk:/home/users/jelt/GitHub/genNEMO/InitialConditions/glosea_ini_19930101_vo*nc .
