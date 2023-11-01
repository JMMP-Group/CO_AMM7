# CO AMM7 (Coastal Ocean Atlantic Margin Model 7km) 

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

The key update in this version of the AMM7 configuration is the change to a multi-envelope vertical coordinate system (from a quasi-sigma terrain following system). Details of this vertical coordinate system change can be found in the Ocean Modelling article [The effect of vertical coordinates on the accuracy of a shelf sea model](https://doi.org/10.1016/j.ocemod.2021.101935) and accompanying [repository](https://zenodo.org/badge/latestdoi/235544712). 

---

## Configuration
<p align="center">
<img src="https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/CO9_repo/CO9_AMM7_domain_bathy.jpg" width="600" >
</p>

|  **Configuration ** | **Specification** |
|-------------- | -------------- |
| **Version** | r4.0.2/r4.0.4/r4.2.0/r4.2.1/branch4.2 |
| **Components** | OCE |
| **Grid** | ORCA |
| **Resolution** | 7 km |
| **Horizontal Gridpoints** | 111,375 (y=375, x=297) |
| **Vertical Levels** | 51 |
| **Vertical Coordinates** | Multi-Envelope - 2 envelopes of quasi-sigma terrain following or SF12 |
| **Time Step [s]** | 300 / 30 |
| **Boundaries** | 2 sets of unstructured 2d and 3d open boundaries (1 for open ocean, 1 for Baltic) |

---

## Quick Start:

```
git clone git@github.com:JMMP-Group/CO_AMM7.git
git checkout NEMOv4.2
./cfgs/CO_AMM7/scripts/setup/AMM7_setup -p $PWD/AMM7_4p0p2 -r $PWD/cfgs/CO_AMM7 \
                                      -n 4.0.2 -x 2 -m archer2 -a mpich -c cray
cd AMM7_4p0p2/nemo/cfgs/AMM7/
cp -rP EXPREF EXP_MYRUN
cd EXP_MYRUN
```
Edit the project code and options in  `runscript.slurm` then:
```
sbatch runscript.slurm
```
This will produce a 5 day mean output from the beginning of 2005. The run should take 5 minutes to complete once in the machine.
Currently AMM7 can be compiled with `-a mpich -c cray` and `-a mpich -c gnu`. For setup script options: `./cfgs/CO_AMM7/scripts/setup/AMM7_setup -h`

### Forcing data and configuration files:

If running the setup script on ARCHER2 these are symbolically linked, so there is no need to download the data.

|  **Input** | **Download Location** |
|-------------- | -------------- |
| **Domain_cfg.nc** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/CO9_repo/domain_cfg_co9amm7_MEsL51r10-07.nc |
| **Open ocean boundary coordinates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.bdy.nc |
| **Baltic coordimates.bdy.nc** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/grid/coordinates.skagbdy.nc |

---

| **Forcing** | **Download Location** |
|-------------- | ------------------|
| **Surface boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/SBC/ |
| **Open ocean boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY/ |
| **Baltic boundary** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/BDY_SKAG/ |
| **River runoff** | http://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/RIV/ |
| **Tide** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/TIDE/ |
| **Initial condition** | https://gws-access.jasmin.ac.uk/public/jmmp/AMM7/inputs/IC/ |

---
