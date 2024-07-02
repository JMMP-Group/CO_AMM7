# CO9p2 AMM7 (Coastal Ocean Atlantic Margin Model 7km. NEMOv4.0.4)

This release of AMM7 was created to match the [AMM15 branch for CO9](https://github.com/JMMP-Group/CO_AMM15/tree/CO9) and associated [release](https://github.com/JMMP-Group/CO_AMM15/releases/tag/v9.2.1)
It is not directly tracible to other AMM7 releases. The closest neighbours (at 4.0.2 and 4.2) have additional code to diagnose momentum budgets. These changes are quite invasive and are not in the AMM15 CO9p2 configuration so were not included in this AMM7 release. (NB the changes to momentum budgets do appear in the subsequent AMM15 configurations, inline with developments made in AMM7)

The AMM7 neighbour at NEMOv4.0.2 is discussed in the paper "Using shelfedge transport composition and sensitivity experiments to understand processes driving sea level on the Northwest European Shelf" submited to JGR:Oceans in Novemeber 2023 by Wise, Anthony; Calafat, Francisco M.; Hughes, Christopher W.; Jevrejeva, Svetlana; Katsman, Caroline A.; Oelsmann, Julius; Piecuch, Christopher G.; Polton, Jeff A.; Richter, Kristin.
This neighbouring AMM7 configuration is detailed in the Ocean Modelling article [The effect of vertical coordinates on the accuracy of a shelf sea model](https://doi.org/10.1016/j.ocemod.2021.101935) and accompanying [repository](https://zenodo.org/badge/latestdoi/235544712). The vertical coordinate reference used are MEs_r10_r07. This branch updates the NEMO codebase from 4.0.2 to 4.0.4.

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

---

## Configuration
<p align="center">
<img src="https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/CO9_repo/CO9_AMM7_domain_bathy.jpg" width="600" >
</p>

|  **Configuration** | **Specification** |
|-------------- | -------------- |
| **Nemo-ocean repository** | http://forge.ipsl.jussieu.fr/nemo/svn/NEMO |
| **Branch** | releases/r4.0/r4.0.4  Revision=13653|
| **Components** | OCE |
| **CPP keys** | key_mpp_mpi key_nosignedzero key_iomput |
| **Grid** | AMM7 |
| **Resolution** | 7 km |
| **Horizontal Gridpoints** | 111,375 (y=375, x=297) |
| **Vertical Levels** | 51 |
| **Vertical Coordinates** | Multi-Envelope - 2 envelopes of quasi-sigma terrain following |
| **Time Step [s]** | 300 / 30 |
| **Boundaries** | 2 sets of unstructured 2d and 3d open boundaries (1 for open ocean, 1 for Baltic) |

Configuration differences between CO9_AMM15p2 and this configuration, CO9_AMM7

|  **variable** | **CO9_AMM15p2** | **CO9_AMM7** | **Wise 4.0.2** |
|-------------- | -------------- | --------------| --------------|
| **&nam_tide** | 35 FES | 16 TPXO | 16 TPXO |
| **&NAMZDF_GLS** | NN_STAB_FUNC=1 (KC94) | NN_STAB_FUNC=2 (CanutoA) | |
| **&NAMZDF_GLS** | RN_EPSMIN=1E-9  | RN_EPSMIN=1E-12 | |
| **&NAMDRG_BOT** | RN_CD0=2.5E-3 | RN_CD0=1.E-3 | |
| **&NAMTRA_LDF** | ln_traldf_blp=.true. | ln_traldf_lap=.true. | |
|                 | ln_traldf_lev=.true. | ln_traldf_hor=.true. ||
|                 | nn_aht_ijk_t=0      | nn_aht_ijk_t=0 ||
|                 | rn_Ld=493           | rn_Ld=1000  ||
|                 | rn_Ud=0.01          | rn_Ud=0.01 ||
| **&NAMDYN_LDF** | ln_dynldf_blp=.true. | ln_dynldf_blp=.true. ||
|                 | ln_dynldf_lev=.true. | ln_dynldf_lev=.true. ||
|                 | nn_ahm_ijk_t=31     | nn_ahm_ijk_t=0 ||
|                 | rn_Lv=493           | rn_Lv=1000  ||
|                 | rn_Uv=0.01          | rn_Uv=0.012 ||
| **&NAMDYN_SPG** | ln_dynspg_ts=.true. | ln_dynspg_ts=.true. ||
|                 |   ln_bt_fw=.FALSE.  |  ln_bt_fw=.TRUE.   | |
|                 |   ln_bt_auto=.TRUE. |  ln_bt_auto=.FALSE. ||
|                 |   -->  rn_bt_cmax   =  0.8 | --> nn_baro      = 30 ||
| **&NAMTRA_ADV** |  LN_TRAADV_FCT=TRUE. |  LN_TRAADV_FCT=TRUE. | |
|                 | NN_FCT_H=2 | NN_FCT_H=4 ||
| **&NAMSBC**      | ln_traqsr   = .true. | ln_traqsr   = .true. | |
|                  | ln_apr_dyn  = .true. | ln_apr_dyn  = .true. | |
|                  | ln_lsm = 10          | ln_lsm = 1 | | 
| **&namsbc_apr**  | ln_apr_obc  = .true. | ln_apr_obc  = .true. | |
| 
 
---

## Install guidance:

To clone the git repository locally
```
git clone git@github.com:JMMP-Group/CO_AMM7.git
git checkout CO9_AMM7_v4.0.4  # or get the release code
```

# Run the setup script on ARCHER2
```
cd CO_AMM7

./scripts/setup/setup_AMM7_CO9_P2_archer -w <WORKING_DIRECTORY> -s <REPOSITORY_DIRECTORY>
# e.g. ./scripts/setup/setup_AMM7_CO9_P2_archer -w /work/n01/n01/$USER/CO_AMM7 -s /work/n01/n01/$USER/CO_AMM7
```

This will obtain the base code from the NEMO repository, build XIOS and build NEMO with the changes given in MY_SRC.

Input files can then be downloaded into the experiment directory  and the configuration should then be setup. A sample runscript is included to run on ARCHER2 .

For example:
```
cd /work/n01/n01/$USER/CO_AMM7/CO9_AMM7_P2/nemo/cfgs/AMM7
cp -rP EXPREF EXP01

cd /work/n01/n01/$USER/CO_AMM7/CO9_AMM7_P2/nemo/cfgs/AMM7/EXP01
. /work/n01/n01/$USER/CO_AMM7/scripts/setup/setup_P2_files.sh

cd EXP01
cp /work/n01/n01/$USER/CO_AMM7/scripts/run/runscript.slurm .
sbatch runscript.slurm
```

---

## Configuration Input Files

|  **Input** | **Download Location** |
|-------------- | -------------- |
| **Domain_cfg.nc** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc |
| **Open ocean boundary coordinates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.bdy.nc |
| **Baltic coordimates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.skagbdy.nc |

---

## Sample Forcing Files

| **Forcing** | **Download Location** |
|-------------- | ------------------|
| **Surface boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/SBC/ |
| **Open ocean boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY/ |
| **Baltic boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY_SKAG/ |
| **River runoff** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/RIV/ |
| **Tide** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/TIDE/ |
| **Initial condition** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/IC/ |

---
