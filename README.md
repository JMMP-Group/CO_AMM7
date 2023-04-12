# CO9 AMM7 (Coastal Ocean Atlantic Margin Model 7km) 

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

The key update in this version of the AMM7 configuration is the change to a multi-envelope vertical coordinate system (from a quasi-sigma terrain following system). Details of this vertical coordinate system change can be found in the Ocean Modelling article [The effect of vertical coordinates on the accuracy of a shelf sea model](https://doi.org/10.1016/j.ocemod.2021.101935) and accompanying [repository](https://zenodo.org/badge/latestdoi/235544712). 

---

## Configuration
<p align="center">
<img src="https://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/CO9_repo/CO9_AMM7_domain_bathy.jpg" width="600" >
</p>

|  **Configuration ** | **Specification** |
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
| **Time Step [s]** | 300 / 30 |
| **Boundaries** | 2 sets of unstructured 2d and 3d open boundaries (1 for open ocean, 1 for Baltic) |

---

## Install guidance:

To clone the git repository locally
```
git clone git@github.com:JMMP-Group/CO9_AMM7.git
```

To then download NEMO and copy files from the git repo into the appropriate directories 
```
./CO9_AMM7/scripts/setup/amm7_setup_light -w $PWD/CO9_AMM7_instance -s $PWD/CO9_AMM7
```

For ARCHER2 follow the instructions [here](https://github.com/hpc-uk/build-instructions/tree/main/apps/NEMO) to compile XIOS and then setup the compiler configuration file for NEMO.
NEMO can then be compiled with
```
cd CO9_AMM7_instance/nemo
./makenemo -m X86_ARCHER2-Cray -r AMM7 -j 16
```

Create a link to xios in the experiment (i.e. EXP00) directory, e.g.
```
ln -s ${PRFX}/xios/2.5/cmpich8-ucx/cce12/bin/xios_server.exe xios_server.exe
```
Input files can then be downloaded into the experiment directory  and the configuration should then be setup. A sample runscript is included to run on ARCHER2 in the EXP00 directory.

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
