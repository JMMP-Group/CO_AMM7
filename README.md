# CO9 AMM7 (Coastal Ocean Atlantic Margin Model 7km) 
[![DOI](https://zenodo.org/badge/235544712.svg)](https://zenodo.org/badge/latestdoi/235544712)

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

---

## Configuration
<p align="center">
<img src="https://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/CO9_repo/CO9_AMM7_domain_bathy.jpg" width="600" >
</p>

|  **Configuration Option** | **Specification** |
|-------------- | -------------- |  
| **Nemo-ocean repository** | http://forge.ipsl.jussieu.fr/nemo/svn/NEMO | 
| **Branch** | releases/r4.0/r4.0.2 |  
| **Components** | OCE | 
| **CPP keys** | key_mpp_mpi key_vectopt_loop key_nosignedzero key_iomput | 
| **Grid** | ORCA | 
| **Resolution** | 7 km | 
| **Horizontal Gridpoints** | 111,375 (y=375, x=297) | 
| **Vertical Levels** | 51 |
| **Vertical Coordinates** | Multi-Envelope - 2 envelopes of quasi-sigma terrain following |
| **Time Step [s]** | 300 |
| **Atmospheric Condition** | Bulk formula COARE 3.5 | 
| **River Runoffs** | Daily T&S distributed through column | 
| **Boundaries** | 2 sets of unstructured 2d and 3d open boundaries (1 for open ocean, 1 for Baltic) |
| **Boundary Tidal Sonstituents** | 15 |
| **Equation of State** | eos80 |

---

## Configuration Input Files

|  **Input** | **Download Location** |
|-------------- | -------------- |  
| **Domain_cfg.nc** | https://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/CO9_repo/domain_cfg_MEs_L51_r10-07_opt_v2.nc |
| **Open ocean boundary coordinates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/grid/coordinates.bdy.nc |
| **Baltic coordimates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/grid/coordinates.skagbdy.nc |

---

## Sample Forcing Files

| **Forcing** | **Download Location** |
|-------------- | ------------------|
| **Surface boundary** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/inputs/SBC/ |
| **Open ocean boundary** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/inputs/BDY/ |
| **Baltic boundary** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/inputs/BDY_SKAG/ |
| **River runoff** | http://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/inputs/RIV/ |

---

## Quick Start:

```
git clone git@github.com:JMMP-Group/CO_AMM7.git
./CO_AMM7/scripts/setup/amm7_setup_archer -w $PWD/test -x $PWD/test -s $PWD/CO_AMM7
cd test/nemo/cfgs/AMM7/EXP00
```
Edit the project code in  `runscript.pbs` then:
```
qsub -q short runscript.pbs
```

## ENSEMBLE Runs:

*as above, but after installation*:

```
cd test/nemo/cfgs/AMM7/ENSEMBLE_CONTROL
```
Edit the `ensemble_hpg.pbs` script to add/remove model ensemble members from the list, then:
```
qsub ensemble_hpg.pbs
```
NB. If you alter the number of ensemble members you will have to change the number of cores requested, e.g. `#PBS -l select=21` for 3 members etc.

For reference, a list of current ensemble members can be found [here](https://github.com/JMMP-Group/AMM7_zgr/blob/master/scripts/setup/model_list)

### Forcing data:

[JMMP-COLLAB](http://gws-access.ceda.ac.uk/public/jmmp_collab/AMM7/)
