# CO9 AMM7 @ NEMO 4.2.1 (Coastal Ocean Atlantic Margin Model 7km) 

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

This branch of the repository is a DRAFT version of the CO9 AMM7 configuration at NEMO version 4.2.1 (the Master and CO9_AMM7 branches use NEMO at version 4.0.2). 

Change to the configuration to note include a change of pressure gradient scheme from PRJ to DJC and a change of equation of state from EOS80 to TEOS10 (conservative temperature and absolute salinity must now be supplied). Currently the surface bulk formulation is set to ECMWF (consisitent with ERA5 forcing) rather than COARE. The MY_SRC files have been harmonised with AMM15. 

---

## Configuration
<p align="center">
<img src="https://gws-access.jasmin.ac.uk/public/jmmp_collab/AMM7/CO9_repo/CO9_AMM7_domain_bathy.jpg" width="600" >
</p>

|  **Configuration ** | **Specification** |
|-------------- | -------------- |
| **Nemo-ocean repository** | https://forge.nemo-ocean.eu/nemo/nemo.git |
| **Branch** | 4.2.1 tagged release |
| **Components** | OCE |
| **CPP keys** | key_nosignedzero key_xios key_qco |
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
cd <path to directory to install repo into>
git clone git@github.com:JMMP-Group/CO9_AMM7_v4.2.git
```

To then download NEMO and copy files from the git repo into the appropriate directories 
```
CO9_AMM7_v4.2/scripts/setup/amm7_setup_light -w <path to directory to install NEMO into> -s <path to the repo>
```

The remaining setup instructions are machine dependent and beyond scope. For ARCHER2i, instructions can be found [here](https://github.com/hpc-uk/build-instructions/tree/main/apps/NEMO).

As an example the following process has been used on ARCHER2 using XIOS2 and the CRAY compiler (valid at 14th Dec 2023)

Get compiler option files using a shared XIOS2 install
```
cd <path to your nemo install>/nemo_4.2.1
cp /work/n01/shared/nemo/ARCH/*4.2.fcm arch/NOC/.
```
Load appropriate modules
```
module swap craype-network-ofi craype-network-ucx
module swap cray-mpich cray-mpich-ucx
module load cray-hdf5-parallel/1.12.2.1
module load cray-netcdf-hdf5parallel/4.9.0.1

```
Compile NEMO
```
./makenemo -m X86_ARCHER2-Cray_4.2 -r AMM7 -j 16
```

Create a link to xios in the EXP00 directory
```
cd cfgs/AMM7
ln -s /work/n01/shared/nemo/XIOS2_Cray/bin/xios_server.exe EXP00/xios_server.exe
```
Create links to the nemo and xios executables from your reference experiment directory
```
cd EXP_REF
ln -s ../EXP00/xios_server.exe xios_server.exe
ln -s ../EXP00/nemo.exe nemo.exe
```

Input files can then be downloaded to the nemo_4.2.1/cfgs/AMM7/INPUTS child directories. Note these direcotries will need to be linked to in the EXP_REF directory.
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


